# PhysioExperiment Plumber API
# Main API definition for GUI backend

library(plumber)
library(PhysioExperiment)
library(jsonlite)

# Initialize session storage
.session_env <- new.env()
.session_env$datasets <- list()
.session_env$counter <- 0L

#* @apiTitle PhysioExperiment API
#* @apiDescription REST API for PhysioExperiment GUI backend
#* @apiVersion 1.0.0

# ============================================================================
# Utility Functions
# ============================================================================

generate_id <- function() {
  .session_env$counter <- .session_env$counter + 1L
  sprintf("ds_%06d", .session_env$counter)
}

success_response <- function(data = NULL) {
  list(success = TRUE, data = data)
}

error_response <- function(message) {
  list(success = FALSE, error = message)
}

dataset_to_json <- function(pe) {
  list(
    id = attr(pe, "session_id") %||% "unknown",
    name = attr(pe, "name") %||% "Untitled",
    nChannels = ncol(pe),
    nTimepoints = nrow(pe),
    samplingRate = samplingRate(pe),
    duration = nrow(pe) / samplingRate(pe),
    channels = as.character(colData(pe)$label %||% colnames(pe) %||% paste0("Ch", seq_len(ncol(pe)))),
    assays = names(SummarizedExperiment::assays(pe)),
    events = if (length(events(pe)) > 0) {
      lapply(seq_len(nrow(events(pe))), function(i) {
        list(
          onset = events(pe)$onset[i],
          duration = events(pe)$duration[i],
          type = as.character(events(pe)$type[i])
        )
      })
    } else NULL,
    metadata = as.list(S4Vectors::metadata(pe))
  )
}

# ============================================================================
# Health Check
# ============================================================================

#* Health check endpoint
#* @get /api/health
function() {
  list(
    status = "ok",
    version = as.character(packageVersion("PhysioExperiment")),
    timestamp = Sys.time()
  )
}

# ============================================================================
# Session Management
# ============================================================================

#* Create new session
#* @post /api/session
function() {
  session_id <- paste0("session_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
  success_response(list(sessionId = session_id))
}

#* Keep session alive
#* @post /api/session/<session_id>/keepalive
function(session_id) {
  success_response(list(sessionId = session_id, timestamp = Sys.time()))
}

# ============================================================================
# Dataset CRUD
# ============================================================================

#* List all datasets
#* @get /api/v1/datasets
function() {
  datasets <- lapply(.session_env$datasets, dataset_to_json)
  success_response(unname(datasets))
}

#* Import dataset from file
#* @post /api/v1/datasets
#* @param path:str Path to the data file
#* @param format:str File format (auto, edf, bdf, brainvision, hdf5, csv)
function(path, format = "auto") {
  tryCatch({
    # Validate path
    if (!file.exists(path)) {
      return(error_response(paste("File not found:", path)))
    }

    # Read file based on format
    pe <- switch(
      format,
      "auto" = readPhysio(path),
      "edf" = readEDF(path),
      "bdf" = readBDF(path),
      "brainvision" = readBrainVision(path),
      "hdf5" = readHDF5(path),
      "csv" = readCSV(path),
      readPhysio(path)
    )

    # Generate ID and store
    id <- generate_id()
    attr(pe, "session_id") <- id
    attr(pe, "name") <- basename(path)
    .session_env$datasets[[id]] <- pe

    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Get dataset info
#* @get /api/v1/datasets/<id>
function(id) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }
  success_response(dataset_to_json(.session_env$datasets[[id]]))
}

#* Delete dataset
#* @delete /api/v1/datasets/<id>
function(id) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }
  .session_env$datasets[[id]] <- NULL
  success_response()
}

# ============================================================================
# Signal Data
# ============================================================================

#* Get signal data
#* @get /api/v1/datasets/<id>/signals
#* @param channels:str Comma-separated channel indices (optional)
#* @param start:dbl Start time in seconds (optional)
#* @param end:dbl End time in seconds (optional)
#* @param downsample:int Downsample factor (optional)
function(id, channels = NULL, start = NULL, end = NULL, downsample = NULL) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  pe <- .session_env$datasets[[id]]
  sr <- samplingRate(pe)

  # Parse channels
  ch_idx <- if (!is.null(channels)) {
    as.integer(strsplit(channels, ",")[[1]])
  } else {
    seq_len(ncol(pe))
  }

  # Calculate time indices
  start_idx <- if (!is.null(start)) max(1, floor(as.numeric(start) * sr) + 1) else 1
  end_idx <- if (!is.null(end)) min(nrow(pe), ceiling(as.numeric(end) * sr)) else nrow(pe)

  # Extract data
  data <- SummarizedExperiment::assay(pe)[start_idx:end_idx, ch_idx, drop = FALSE]

  # Downsample if requested
  if (!is.null(downsample)) {
    ds <- as.integer(downsample)
    idx <- seq(1, nrow(data), by = ds)
    data <- data[idx, , drop = FALSE]
    sr <- sr / ds
  }

  # Build response
  time <- (seq_len(nrow(data)) - 1) / sr + (start_idx - 1) / samplingRate(pe)
  channel_labels <- colData(pe)$label[ch_idx] %||% paste0("Ch", ch_idx)

  signals <- setNames(
    lapply(seq_len(ncol(data)), function(j) as.vector(data[, j])),
    channel_labels
  )

  success_response(list(
    signals = signals,
    time = time,
    channels = as.character(channel_labels),
    samplingRate = sr
  ))
}

# ============================================================================
# Preprocessing
# ============================================================================

#* Apply bandpass filter
#* @post /api/v1/datasets/<id>/filter
#* @param low:dbl Low cutoff frequency (optional)
#* @param high:dbl High cutoff frequency (optional)
#* @param order:int Filter order (default 4)
#* @param type:str Filter type (butter or fir)
#* @param outputAssay:str Output assay name (default "filtered")
function(id, low = NULL, high = NULL, order = 4, type = "butter", outputAssay = "filtered") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    pe <- filterSignal(
      pe,
      low = if (!is.null(low)) as.numeric(low) else NULL,
      high = if (!is.null(high)) as.numeric(high) else NULL,
      order = as.integer(order),
      type = type,
      output_assay = outputAssay
    )

    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Apply reference change
#* @post /api/v1/datasets/<id>/rereference
#* @param method:str Reference method (average, linked, custom)
#* @param channels:str Reference channels for custom (comma-separated)
#* @param outputAssay:str Output assay name
function(id, method = "average", channels = NULL, outputAssay = "rereferenced") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    ref_channels <- if (!is.null(channels)) strsplit(channels, ",")[[1]] else NULL

    pe <- rereference(pe, method = method, channels = ref_channels, output_assay = outputAssay)

    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Resample dataset
#* @post /api/v1/datasets/<id>/resample
#* @param targetRate:dbl Target sampling rate
#* @param method:str Resampling method (linear, spline)
#* @param outputAssay:str Output assay name
function(id, targetRate, method = "spline", outputAssay = "resampled") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    pe <- resample(pe, target_rate = as.numeric(targetRate), method = method, output_assay = outputAssay)
    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# Epoching
# ============================================================================

#* Epoch data around events
#* @post /api/v1/datasets/<id>/epoch
#* @param tmin:dbl Time before event (negative)
#* @param tmax:dbl Time after event
#* @param eventType:str Event type to epoch around (optional)
#* @param baseline:str Baseline correction window as "start,end" (optional)
function(id, tmin, tmax, eventType = NULL, baseline = NULL) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    baseline_window <- if (!is.null(baseline)) {
      as.numeric(strsplit(baseline, ",")[[1]])
    } else NULL

    pe <- epochData(
      pe,
      tmin = as.numeric(tmin),
      tmax = as.numeric(tmax),
      event_type = eventType,
      baseline = baseline_window
    )

    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# Time-Frequency Analysis
# ============================================================================

#* Compute spectrogram
#* @post /api/v1/datasets/<id>/spectrogram
#* @param channel:int Channel index (1-based)
#* @param windowSize:int Window size in samples
#* @param overlap:dbl Overlap ratio (0-1)
#* @param windowType:str Window type (hanning, hamming, blackman, rectangular)
function(id, channel = 1, windowSize = 256, overlap = 0.5, windowType = "hanning") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    # Select channel
    pe_ch <- pe[, as.integer(channel)]

    result <- spectrogram(
      pe_ch,
      window_size = as.integer(windowSize),
      overlap = as.numeric(overlap),
      window_type = windowType
    )

    success_response(list(
      power = result$power,
      frequencies = result$frequencies,
      times = result$times,
      samplingRate = samplingRate(pe)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute wavelet transform
#* @post /api/v1/datasets/<id>/wavelet
#* @param frequencies:str Comma-separated frequencies
#* @param nCycles:int Number of cycles (default 5)
#* @param channel:int Channel index
function(id, frequencies, nCycles = 5, channel = 1) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    pe_ch <- pe[, as.integer(channel)]
    freqs <- as.numeric(strsplit(frequencies, ",")[[1]])

    result <- waveletTransform(pe_ch, frequencies = freqs, n_cycles = as.integer(nCycles))

    success_response(list(
      power = result$power,
      phase = result$phase,
      frequencies = result$frequencies,
      times = result$times
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute band power
#* @post /api/v1/datasets/<id>/bandpower
#* @param method:str Method (welch or fft)
#* @param relative:bool Compute relative power
function(id, method = "welch", relative = FALSE) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    result <- bandPower(pe, method = method, relative = as.logical(relative))
    success_response(result)
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# Connectivity
# ============================================================================

#* Compute connectivity
#* @post /api/v1/datasets/<id>/connectivity
#* @param method:str Connectivity method (coherence, plv, pli, wpli, correlation)
#* @param freqBand:str Frequency band as "low,high" (optional)
#* @param channels:str Channel indices (comma-separated, optional)
function(id, method = "coherence", freqBand = NULL, channels = NULL) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    freq_band <- if (!is.null(freqBand)) {
      as.numeric(strsplit(freqBand, ",")[[1]])
    } else NULL

    ch_idx <- if (!is.null(channels)) {
      as.integer(strsplit(channels, ",")[[1]])
    } else NULL

    result <- connectivity(pe, method = method, freq_band = freq_band, channels = ch_idx)

    channel_labels <- colData(pe)$label %||% paste0("Ch", seq_len(ncol(pe)))
    if (!is.null(ch_idx)) channel_labels <- channel_labels[ch_idx]

    success_response(list(
      matrix = result$matrix,
      method = method,
      freqBand = freq_band,
      channelNames = as.character(channel_labels)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# Statistics
# ============================================================================

#* T-test on epochs
#* @post /api/v1/datasets/<id>/statistics/ttest
#* @param conditionA:str Condition A name or indices
#* @param conditionB:str Condition B name or indices (optional for one-sample)
#* @param paired:bool Paired t-test
#* @param correction:str Multiple comparison correction (none, bonferroni, fdr)
function(id, conditionA, conditionB = NULL, paired = FALSE, correction = "fdr") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    # Parse conditions (could be indices or names)
    parse_condition <- function(cond) {
      if (grepl("^[0-9,]+$", cond)) {
        as.integer(strsplit(cond, ",")[[1]])
      } else {
        cond  # Return as string for name-based lookup
      }
    }

    cond1 <- parse_condition(conditionA)
    cond2 <- if (!is.null(conditionB)) parse_condition(conditionB) else NULL

    result <- tTestEpochs(
      pe,
      condition1 = cond1,
      condition2 = cond2,
      paired = as.logical(paired)
    )

    # Apply correction
    p_corrected <- correctPValues(result$p_values, method = correction)

    success_response(list(
      statistic = as.vector(result$t_values),
      pvalue = as.vector(p_corrected),
      significant = as.vector(p_corrected < 0.05),
      times = result$times,
      correction = correction
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* ANOVA on epochs
#* @post /api/v1/datasets/<id>/statistics/anova
#* @param groups:str Group variable name
function(id, groups) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    result <- anovaEpochs(pe, groups = groups)

    success_response(list(
      statistic = as.vector(result$f_values),
      pvalue = as.vector(result$p_values),
      significant = as.vector(result$p_values < 0.05),
      dfBetween = result$df_between,
      dfWithin = result$df_within,
      groups = result$groups
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Cluster permutation test
#* @post /api/v1/datasets/<id>/statistics/cluster-permutation
#* @param condition1:str Condition 1 indices (comma-separated, optional)
#* @param condition2:str Condition 2 indices (comma-separated, optional)
#* @param nPermutations:int Number of permutations
#* @param threshold:dbl Cluster-forming threshold (p-value)
function(id, condition1 = NULL, condition2 = NULL, nPermutations = 1000, threshold = 0.05) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    cond1 <- if (!is.null(condition1)) as.integer(strsplit(condition1, ",")[[1]]) else NULL
    cond2 <- if (!is.null(condition2)) as.integer(strsplit(condition2, ",")[[1]]) else NULL

    result <- clusterPermutationTest(
      pe,
      condition1 = cond1,
      condition2 = cond2,
      n_permutations = as.integer(nPermutations),
      threshold = as.numeric(threshold)
    )

    success_response(list(
      clusters = result$clusters,
      clusterMask = result$cluster_mask,
      tObs = result$t_obs,
      permutationDistribution = result$permutation_distribution
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# EMG Processing
# ============================================================================

#* Rectify EMG signal
#* @post /api/v1/datasets/<id>/emg/rectify
#* @param channels:str Channel indices (comma-separated, optional)
#* @param outputAssay:str Output assay name (default "rectified")
function(id, channels = NULL, outputAssay = "rectified") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    ch_idx <- if (!is.null(channels)) as.integer(strsplit(channels, ",")[[1]]) else NULL

    pe <- rectifyEMG(pe, channels = ch_idx, output_assay = outputAssay)
    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute EMG envelope
#* @post /api/v1/datasets/<id>/emg/envelope
#* @param method:str Method (lowpass, rms, hilbert)
#* @param cutoffFreq:dbl Cutoff frequency for lowpass (default 6 Hz)
#* @param windowMs:dbl Window size in ms for RMS (default 100)
#* @param outputAssay:str Output assay name (default "envelope")
function(id, method = "lowpass", cutoffFreq = 6, windowMs = 100, outputAssay = "envelope") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)

    pe <- emgEnvelope(
      pe,
      sampling_rate = sr,
      method = method,
      cutoff_freq = as.numeric(cutoffFreq),
      window_ms = as.numeric(windowMs),
      output_assay = outputAssay
    )
    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute RMS
#* @post /api/v1/datasets/<id>/emg/rms
#* @param windowMs:dbl Window size in ms (default 100)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
#* @param channels:str Channel indices (comma-separated, optional)
#* @param outputAssay:str Output assay name (default "rms")
function(id, windowMs = 100, overlap = 0.5, channels = NULL, outputAssay = "rms") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- if (!is.null(channels)) as.integer(strsplit(channels, ",")[[1]]) else NULL

    pe <- computeRMS(
      pe,
      sampling_rate = sr,
      window_ms = as.numeric(windowMs),
      overlap = as.numeric(overlap),
      channels = ch_idx,
      output_assay = outputAssay
    )
    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute MAV (Mean Absolute Value)
#* @post /api/v1/datasets/<id>/emg/mav
#* @param windowMs:dbl Window size in ms (default 100)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
#* @param channels:str Channel indices (comma-separated, optional)
#* @param outputAssay:str Output assay name (default "mav")
function(id, windowMs = 100, overlap = 0.5, channels = NULL, outputAssay = "mav") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- if (!is.null(channels)) as.integer(strsplit(channels, ",")[[1]]) else NULL

    pe <- computeMAV(
      pe,
      sampling_rate = sr,
      window_ms = as.numeric(windowMs),
      overlap = as.numeric(overlap),
      channels = ch_idx,
      output_assay = outputAssay
    )
    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* MVC Normalize EMG
#* @post /api/v1/datasets/<id>/emg/mvc-normalize
#* @param mvc:str MVC values (comma-separated per channel, optional)
#* @param mvcMethod:str Method to compute MVC (peak, mean, percentile)
#* @param percentile:dbl Percentile for percentile method (default 95)
#* @param channels:str Channel indices (comma-separated, optional)
#* @param outputAssay:str Output assay name (default "mvc_normalized")
function(id, mvc = NULL, mvcMethod = "peak", percentile = 95, channels = NULL, outputAssay = "mvc_normalized") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    mvc_values <- if (!is.null(mvc)) as.numeric(strsplit(mvc, ",")[[1]]) else NULL
    ch_idx <- if (!is.null(channels)) as.integer(strsplit(channels, ",")[[1]]) else NULL

    pe <- mvcNormalize(
      pe,
      mvc = mvc_values,
      mvc_method = mvcMethod,
      percentile = as.numeric(percentile),
      channels = ch_idx,
      output_assay = outputAssay
    )
    .session_env$datasets[[id]] <- pe
    success_response(dataset_to_json(pe))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Detect muscle onset
#* @post /api/v1/datasets/<id>/emg/detect-onset
#* @param method:str Detection method (double_threshold, single_threshold, tkeo, hodges)
#* @param thresholdSd:dbl Threshold in SD units (default 3)
#* @param minDurationMs:dbl Minimum duration in ms (default 25)
#* @param channel:int Channel index (1-based)
function(id, method = "double_threshold", thresholdSd = 3, minDurationMs = 25, channel = 1) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- as.integer(channel)

    result <- detectMuscleOnset(
      pe[, ch_idx],
      sampling_rate = sr,
      method = method,
      threshold_sd = as.numeric(thresholdSd),
      min_duration_ms = as.numeric(minDurationMs)
    )

    success_response(list(
      onsets = result$onsets,
      offsets = result$offsets,
      durations = result$durations,
      onsetTimes = result$onset_times,
      offsetTimes = result$offset_times,
      method = method,
      thresholdSd = as.numeric(thresholdSd)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute co-contraction index
#* @post /api/v1/datasets/<id>/emg/co-contraction
#* @param agonist:int Agonist channel index (1-based)
#* @param antagonist:int Antagonist channel index (1-based)
#* @param method:str CCI method (overlap, ratio, rudolph)
#* @param windowMs:dbl Window size in ms for windowed CCI (optional)
function(id, agonist, antagonist, method = "overlap", windowMs = NULL) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    data <- SummarizedExperiment::assay(pe)

    ag_signal <- data[, as.integer(agonist)]
    ant_signal <- data[, as.integer(antagonist)]

    if (!is.null(windowMs)) {
      result <- windowedCCI(
        ag_signal, ant_signal,
        sampling_rate = sr,
        window_ms = as.numeric(windowMs),
        method = method
      )
      success_response(list(
        cci = result$cci,
        times = result$times,
        method = method,
        windowMs = as.numeric(windowMs)
      ))
    } else {
      cci <- coContractionIndex(ag_signal, ant_signal, sr, method = method)
      success_response(list(
        cci = cci,
        method = method
      ))
    }
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Extract EMG features
#* @post /api/v1/datasets/<id>/emg/features
#* @param windowMs:dbl Window size in ms (default 250)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
#* @param features:str Features to extract (comma-separated: mav,rms,wl,zc,ssc,iemg,var,mnf,mdf,pkf)
#* @param channels:str Channel indices (comma-separated, optional)
function(id, windowMs = 250, overlap = 0.5, features = "mav,rms,wl,zc,ssc", channels = NULL) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    feature_list <- strsplit(features, ",")[[1]]
    ch_idx <- if (!is.null(channels)) as.integer(strsplit(channels, ",")[[1]]) else NULL

    result <- extractEMGFeatures(
      pe,
      sampling_rate = sr,
      window_ms = as.numeric(windowMs),
      overlap = as.numeric(overlap),
      features = feature_list,
      channels = ch_idx
    )

    success_response(list(
      features = result$features,
      featureNames = result$feature_names,
      times = result$times,
      channels = result$channels,
      windowMs = as.numeric(windowMs)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# Fatigue Analysis
# ============================================================================

#* Compute median frequency
#* @post /api/v1/datasets/<id>/fatigue/median-frequency
#* @param channel:int Channel index (1-based)
#* @param windowSize:int Window size in samples (default 512)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
#* @param freqRange:str Frequency range as "low,high" (default "20,500")
function(id, channel = 1, windowSize = 512, overlap = 0.5, freqRange = "20,500") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- as.integer(channel)
    freq_band <- as.numeric(strsplit(freqRange, ",")[[1]])

    result <- medianFrequency(
      pe[, ch_idx],
      sampling_rate = sr,
      window_size = as.integer(windowSize),
      overlap = as.numeric(overlap),
      freq_range = freq_band
    )

    success_response(list(
      mdf = result$mdf,
      times = result$times,
      samplingRate = sr,
      windowSize = as.integer(windowSize)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute mean frequency
#* @post /api/v1/datasets/<id>/fatigue/mean-frequency
#* @param channel:int Channel index (1-based)
#* @param windowSize:int Window size in samples (default 512)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
#* @param freqRange:str Frequency range as "low,high" (default "20,500")
function(id, channel = 1, windowSize = 512, overlap = 0.5, freqRange = "20,500") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- as.integer(channel)
    freq_band <- as.numeric(strsplit(freqRange, ",")[[1]])

    result <- meanFrequency(
      pe[, ch_idx],
      sampling_rate = sr,
      window_size = as.integer(windowSize),
      overlap = as.numeric(overlap),
      freq_range = freq_band
    )

    success_response(list(
      mnf = result$mnf,
      times = result$times,
      samplingRate = sr,
      windowSize = as.integer(windowSize)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compute fatigue index
#* @post /api/v1/datasets/<id>/fatigue/index
#* @param channel:int Channel index (1-based)
#* @param method:str Fatigue index method (mdf_slope, mnf_slope, spectral_ratio, amplitude_ratio)
#* @param windowSize:int Window size in samples (default 512)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
function(id, channel = 1, method = "mdf_slope", windowSize = 512, overlap = 0.5) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- as.integer(channel)

    result <- fatigueIndex(
      pe[, ch_idx],
      sampling_rate = sr,
      method = method,
      window_size = as.integer(windowSize),
      overlap = as.numeric(overlap)
    )

    success_response(list(
      fatigueIndex = result$fatigue_index,
      slope = result$slope,
      intercept = result$intercept,
      rSquared = result$r_squared,
      method = method
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Comprehensive fatigue analysis
#* @post /api/v1/datasets/<id>/fatigue/analyze
#* @param channels:str Channel indices (comma-separated, optional)
#* @param windowSize:int Window size in samples (default 512)
#* @param overlap:dbl Overlap ratio (0-1, default 0.5)
function(id, channels = NULL, windowSize = 512, overlap = 0.5) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]
    sr <- samplingRate(pe)
    ch_idx <- if (!is.null(channels)) as.integer(strsplit(channels, ",")[[1]]) else NULL

    result <- analyzeFatigue(
      pe,
      sampling_rate = sr,
      window_size = as.integer(windowSize),
      overlap = as.numeric(overlap),
      channels = ch_idx
    )

    success_response(list(
      mdf = result$mdf,
      mnf = result$mnf,
      fatigueIndices = result$fatigue_indices,
      times = result$times,
      channels = result$channels
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# Muscle Synergy Analysis
# ============================================================================

#* NMF synergy extraction
#* @post /api/v1/datasets/<id>/synergy/nmf
#* @param nSynergies:int Number of synergies (default 4)
#* @param nReplicates:int Number of replicates (default 50)
#* @param maxIterations:int Max iterations (default 1000)
#* @param tolerance:dbl Convergence tolerance (default 1e-4)
#* @param normalizeMuscles:bool Normalize muscle activations (default TRUE)
function(id, nSynergies = 4, nReplicates = 50, maxIterations = 1000, tolerance = 1e-4, normalizeMuscles = TRUE) {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    result <- nmfSynergy(
      pe,
      n_synergies = as.integer(nSynergies),
      n_replicates = as.integer(nReplicates),
      max_iterations = as.integer(maxIterations),
      tolerance = as.numeric(tolerance),
      normalize_muscles = as.logical(normalizeMuscles)
    )

    channel_labels <- colData(pe)$label %||% paste0("Ch", seq_len(ncol(pe)))

    success_response(list(
      weights = result$W,
      activations = result$H,
      vaf = result$VAF,
      reconstruction = result$reconstruction,
      residuals = result$residuals,
      nSynergies = as.integer(nSynergies),
      muscleNames = as.character(channel_labels)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Find optimal number of synergies
#* @post /api/v1/datasets/<id>/synergy/optimal
#* @param vafThreshold:dbl VAF threshold percentage (default 90)
#* @param maxSynergies:int Maximum synergies to test (default 10)
#* @param nReplicates:int Number of replicates per synergy count (default 20)
#* @param method:str Method (threshold, elbow, aic)
function(id, vafThreshold = 90, maxSynergies = 10, nReplicates = 20, method = "threshold") {
  if (!id %in% names(.session_env$datasets)) {
    return(error_response("Dataset not found"))
  }

  tryCatch({
    pe <- .session_env$datasets[[id]]

    result <- optimalSynergyNumber(
      pe,
      vaf_threshold = as.numeric(vafThreshold),
      max_synergies = as.integer(maxSynergies),
      n_replicates = as.integer(nReplicates),
      method = method
    )

    success_response(list(
      optimalN = result$optimal_n,
      vafValues = result$vaf_values,
      vafSd = result$vaf_sd,
      nSynergiesRange = result$n_synergies,
      method = method,
      threshold = as.numeric(vafThreshold)
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Compare synergy similarity between two datasets
#* @post /api/v1/synergy/similarity
#* @param id1:str First dataset ID
#* @param id2:str Second dataset ID
#* @param metric:str Similarity metric (correlation, cosine, dot_product)
#* @param matchSynergies:bool Match synergies for optimal comparison (default TRUE)
function(id1, id2, metric = "correlation", matchSynergies = TRUE) {
  if (!id1 %in% names(.session_env$datasets)) {
    return(error_response("Dataset 1 not found"))
  }
  if (!id2 %in% names(.session_env$datasets)) {
    return(error_response("Dataset 2 not found"))
  }

  tryCatch({
    pe1 <- .session_env$datasets[[id1]]
    pe2 <- .session_env$datasets[[id2]]

    # Assume synergy results are stored in metadata
    syn1 <- S4Vectors::metadata(pe1)$synergy
    syn2 <- S4Vectors::metadata(pe2)$synergy

    if (is.null(syn1) || is.null(syn2)) {
      return(error_response("Run NMF synergy extraction first on both datasets"))
    }

    result <- synergySimilarity(
      syn1, syn2,
      metric = metric,
      match_synergies = as.logical(matchSynergies)
    )

    success_response(list(
      similarityMatrix = result$similarity_matrix,
      meanSimilarity = result$mean_similarity,
      matching = result$matching,
      metric = metric
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

#* Statistical test for synergy similarity
#* @post /api/v1/synergy/similarity-test
#* @param id1:str First dataset ID
#* @param id2:str Second dataset ID
#* @param nPermutations:int Number of permutations (default 1000)
#* @param metric:str Similarity metric (correlation, cosine)
function(id1, id2, nPermutations = 1000, metric = "correlation") {
  if (!id1 %in% names(.session_env$datasets)) {
    return(error_response("Dataset 1 not found"))
  }
  if (!id2 %in% names(.session_env$datasets)) {
    return(error_response("Dataset 2 not found"))
  }

  tryCatch({
    pe1 <- .session_env$datasets[[id1]]
    pe2 <- .session_env$datasets[[id2]]

    syn1 <- S4Vectors::metadata(pe1)$synergy
    syn2 <- S4Vectors::metadata(pe2)$synergy

    if (is.null(syn1) || is.null(syn2)) {
      return(error_response("Run NMF synergy extraction first on both datasets"))
    }

    result <- testSynergySimilarity(
      syn1, syn2,
      n_permutations = as.integer(nPermutations),
      metric = metric
    )

    success_response(list(
      observedSimilarity = result$observed_similarity,
      pValue = result$p_value,
      permutationDistribution = result$permutation_distribution,
      significant = result$p_value < 0.05
    ))
  }, error = function(e) {
    error_response(conditionMessage(e))
  })
}

# ============================================================================
# CORS and Error Handling Filters
# ============================================================================

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }

  plumber::forward()
}

#* @plumber
function(pr) {
  pr %>%
    pr_set_serializer(serializer_json(auto_unbox = TRUE))
}
