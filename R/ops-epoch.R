#' Epoching operations for PhysioExperiment
#'
#' Functions for segmenting continuous data into epochs/trials based on events.

#' Epoch data around events
#'
#' Extracts epochs (segments) of data around specified events.
#'
#' @param x A PhysioExperiment object.
#' @param tmin Time before event onset in seconds (negative for pre-stimulus).
#' @param tmax Time after event onset in seconds.
#' @param event_type Character vector of event types to epoch around.
#'   If NULL, uses all events. Ignored if `events` is provided.
#' @param baseline Numeric vector of length 2 specifying baseline period
#'   (tmin, tmax) for baseline correction. NULL for no correction.
#' @param reject Amplitude threshold for epoch rejection. NULL to keep all.
#' @param events An EventQuery object for advanced event filtering.
#'   If provided, overrides `event_type`.
#' @param min_length Minimum epoch length in seconds when using variable-length
#'   epochs (tmax as event name). Epochs shorter than this are excluded.
#' @return A new PhysioExperiment object with epoched data.
#'   The assay becomes 4D: (time x channel x epoch x sample).
#' @export
#' @examples
#' # Create continuous data with events
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000)),
#'   samplingRate = 100
#' )
#' pe <- addEvents(pe, onset = c(1, 2, 3, 4, 5), type = "stimulus")
#'
#' # Extract epochs: 200ms before to 800ms after stimulus
#' epochs <- epochData(pe, tmin = -0.2, tmax = 0.8)
#'
#' # With baseline correction
#' epochs_bl <- epochData(pe, tmin = -0.2, tmax = 0.8,
#'                        baseline = c(-0.2, 0))
#'
#' # With artifact rejection
#' epochs_clean <- epochData(pe, tmin = -0.2, tmax = 0.8,
#'                           reject = 100)
epochData <- function(x, tmin = -0.2, tmax = 0.8, event_type = NULL,
                      baseline = NULL, reject = NULL, events = NULL,
                      min_length = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate is required for epoching", call. = FALSE)
  }


  # Get events - from EventQuery or legacy parameter
  if (!is.null(events) && inherits(events, "EventQuery")) {
    event_df <- resolveQuery(events)
  } else {
    ev <- getEvents(x, type = event_type)
    event_df <- ev@events
  }

  if (nrow(event_df) == 0) {
    if (!is.null(events) && inherits(events, "EventQuery")) {
      stop("No events found after applying EventQuery filters. Check filter criteria.",
           call. = FALSE)
    } else if (!is.null(event_type)) {
      stop(sprintf("No events of type '%s' found for epoching.",
                   paste(event_type, collapse = "', '")), call. = FALSE)
    } else {
      stop("No events found for epoching. Add events with addEvents() first.",
           call. = FALSE)
    }
  }

  # Handle variable-length epochs when tmax is an event type name
  variable_length_mode <- is.character(tmax) && length(tmax) == 1
  variable_lengths <- NULL

  if (variable_length_mode) {
    # Get end events by type
    end_event_type <- tmax
    all_events <- getEvents(x)@events
    end_events <- all_events[all_events$type == end_event_type, ]

    if (nrow(end_events) == 0) {
      stop(sprintf("No events of type '%s' found for variable-length epochs",
                   end_event_type), call. = FALSE)
    }

    # For each start event, find the next end event
    start_onsets <- event_df$onset
    end_onsets <- end_events$onset

    # Find matching end events and calculate durations
    durations <- numeric(length(start_onsets))
    valid_pairs <- logical(length(start_onsets))

    for (i in seq_along(start_onsets)) {
      # Find next end event after this start
      candidates <- end_onsets[end_onsets > start_onsets[i]]
      if (length(candidates) > 0) {
        durations[i] <- min(candidates) - start_onsets[i]
        valid_pairs[i] <- TRUE
      }
    }

    # Filter by min_length if specified
    if (!is.null(min_length)) {
      valid_pairs <- valid_pairs & durations >= min_length
    }

    if (sum(valid_pairs) == 0) {
      stop("No valid epoch pairs found (check min_length or event ordering)",
           call. = FALSE)
    }

    # Update event_df to only include valid start events
    event_df <- event_df[valid_pairs, ]
    variable_lengths <- durations[valid_pairs]

    # Set tmax to the maximum duration for array allocation
    tmax <- max(variable_lengths)
  }

  # Calculate sample indices
  pre_samples <- abs(as.integer(round(tmin * sr)))
  post_samples <- as.integer(round(tmax * sr))
  epoch_length <- pre_samples + post_samples + 1

  # Get data
  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available", call. = FALSE)
  }
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  n_timepoints <- dims[1]

  # Determine number of channels and samples based on dimensionality
  if (length(dims) == 2) {
    n_channels <- dims[2]
    n_samples <- 1L
    data <- array(data, dim = c(dims[1], dims[2], 1))
  } else if (length(dims) == 3) {
    n_channels <- dims[2]
    n_samples <- dims[3]
  } else {
    stop("Assay must be 2D or 3D", call. = FALSE)
  }

  # Convert event times to sample indices
  event_samples <- timeToSamples(x, event_df$onset)

  # Find valid epochs (within data bounds)
  valid_idx <- which(
    event_samples - pre_samples >= 1 &
    event_samples + post_samples <= n_timepoints
  )

  if (length(valid_idx) == 0) {
    data_length <- n_timepoints / sr
    first_event <- min(event_df$onset)
    last_event <- max(event_df$onset)
    stop(sprintf(
      "No valid epochs found within data bounds.
    - Data length: %.2f seconds (%d samples)
    - Epoch window: %.2f to %.2f seconds
    - Events range: %.2f to %.2f seconds
    - First event requires data from %.2f to %.2f seconds
  Hint: Reduce |tmin| or |tmax|, or use events within data range.",
      data_length, n_timepoints, tmin, tmax,
      first_event, last_event,
      first_event + tmin, first_event + tmax
    ), call. = FALSE)
  }

  n_epochs <- length(valid_idx)
  valid_events <- event_df[valid_idx, ]
  valid_samples <- event_samples[valid_idx]

  # Update variable_lengths if in variable-length mode
  if (variable_length_mode && !is.null(variable_lengths)) {
    variable_lengths <- variable_lengths[valid_idx]
  }

  # Create epoched data array: time x channel x epoch x sample
  epoched <- array(NA_real_, dim = c(epoch_length, n_channels, n_epochs, n_samples))

  if (variable_length_mode && !is.null(variable_lengths)) {
    # Variable-length extraction: extract each epoch individually with padding
    for (i in seq_len(n_epochs)) {
      len_samples <- as.integer(round(variable_lengths[i] * sr)) + pre_samples + 1
      len_samples <- min(len_samples, epoch_length)  # Cap at max length

      start_idx <- valid_samples[i] - pre_samples
      end_idx <- start_idx + len_samples - 1

      for (ch in seq_len(n_channels)) {
        for (s in seq_len(n_samples)) {
          epoched[seq_len(len_samples), ch, i, s] <- data[start_idx:end_idx, ch, s]
          # Remaining samples stay as NA (padding)
        }
      }
    }
  } else {
    # Standard fixed-length extraction (vectorized)
    # Pre-compute all time indices for all epochs
    time_offsets <- seq(-pre_samples, post_samples)
    idx_matrix <- outer(time_offsets, valid_samples, FUN = function(t, s) t + s)

    # Extract all epochs at once for each channel and sample
    for (ch in seq_len(n_channels)) {
      for (s in seq_len(n_samples)) {
        channel_data <- data[, ch, s]
        epoched[, ch, , s] <- matrix(channel_data[idx_matrix],
                                      nrow = epoch_length, ncol = n_epochs)
      }
    }
  }

  # Baseline correction
  if (!is.null(baseline)) {
    if (length(baseline) != 2) {
      stop("'baseline' must be a numeric vector of length 2", call. = FALSE)
    }

    bl_start <- as.integer(round((baseline[1] - tmin) * sr)) + 1
    bl_end <- as.integer(round((baseline[2] - tmin) * sr)) + 1
    bl_start <- max(1, bl_start)
    bl_end <- min(epoch_length, bl_end)

    # Vectorized baseline correction using apply + sweep
    bl_means <- apply(epoched[bl_start:bl_end, , , , drop = FALSE],
                      c(2, 3, 4), mean, na.rm = TRUE)
    epoched <- sweep(epoched, c(2, 3, 4), bl_means, FUN = "-")
  }

  # Epoch rejection
  if (!is.null(reject)) {
    keep <- rep(TRUE, n_epochs)
    for (i in seq_len(n_epochs)) {
      epoch_data <- epoched[, , i, , drop = FALSE]
      if (any(abs(epoch_data) > reject, na.rm = TRUE)) {
        keep[i] <- FALSE
      }
    }

    if (sum(keep) == 0) {
      stop("All epochs rejected", call. = FALSE)
    }

    epoched <- epoched[, , keep, , drop = FALSE]
    valid_events <- valid_events[keep, ]
    n_epochs <- sum(keep)

    # Update variable_lengths if in variable-length mode
    if (variable_length_mode && !is.null(variable_lengths)) {
      variable_lengths <- variable_lengths[keep]
    }

    if (sum(!keep) > 0) {
      message(sprintf("Rejected %d epochs (%.1f%%)",
                      sum(!keep), 100 * sum(!keep) / length(keep)))
    }
  }

  # Create new PhysioExperiment with epoched data
  new_assays <- S4Vectors::SimpleList(epoched = epoched)

  # Create epoch metadata
  epoch_data <- S4Vectors::DataFrame(
    epoch_id = seq_len(n_epochs),
    event_type = valid_events$type,
    event_value = valid_events$value,
    event_onset = valid_events$onset
  )

  # Create new rowData matching epoch_length (dim[1])
  # This represents time points within each epoch

  epoch_times <- seq(tmin, tmax, by = 1 / sr)
  new_rowData <- S4Vectors::DataFrame(
    time_idx = seq_len(epoch_length),
    time = epoch_times[seq_len(epoch_length)]
  )

  # Create new colData matching n_channels (dim[2])
  # Preserve channel information from original colData
  orig_colData <- SummarizedExperiment::colData(x)
  if (nrow(orig_colData) == n_channels) {
    new_colData <- orig_colData
  } else {
    # Create basic channel metadata
    new_colData <- S4Vectors::DataFrame(
      channel_id = seq_len(n_channels)
    )
  }

  # Build metadata list
  meta <- list(
    epoch_tmin = tmin,
    epoch_tmax = tmax,
    epoch_info = epoch_data,
    parent_events = events,
    n_epochs = n_epochs,
    n_samples = n_samples
  )

  # Add variable_lengths if in variable-length mode
  if (variable_length_mode && !is.null(variable_lengths)) {
    meta$variable_lengths <- variable_lengths
  }

  # Create new object
  new_pe <- PhysioExperiment(
    assays = new_assays,
    rowData = new_rowData,
    colData = new_colData,
    metadata = meta,
    samplingRate = sr
  )

  new_pe
}

#' Average epochs
#'
#' Computes the average across epochs, optionally by condition.
#'
#' @param x An epoched PhysioExperiment object.
#' @param by Optional column name in epoch_info to group by.
#' @return A PhysioExperiment object with averaged epochs.
#' @export
averageEpochs <- function(x, by = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for averaging", call. = FALSE)
  }

  epoch_info <- S4Vectors::metadata(x)$epoch_info

  if (is.null(by)) {
    # Average all epochs
    averaged <- apply(data, c(1, 2, 4), mean, na.rm = TRUE)
    new_assays <- S4Vectors::SimpleList(averaged = averaged)

    new_pe <- PhysioExperiment(
      assays = new_assays,
      colData = SummarizedExperiment::colData(x),
      metadata = S4Vectors::metadata(x),
      samplingRate = samplingRate(x)
    )
  } else {
    # Average by condition
    if (!by %in% names(epoch_info)) {
      stop(sprintf("Column '%s' not found in epoch_info", by), call. = FALSE)
    }

    conditions <- unique(epoch_info[[by]])
    n_cond <- length(conditions)

    averaged <- array(NA_real_, dim = c(dims[1], dims[2], n_cond, dims[4]))

    for (i in seq_len(n_cond)) {
      idx <- which(epoch_info[[by]] == conditions[i])
      averaged[, , i, ] <- apply(data[, , idx, , drop = FALSE], c(1, 2, 4), mean, na.rm = TRUE)
    }

    new_assays <- S4Vectors::SimpleList(averaged = averaged)

    cond_df <- S4Vectors::DataFrame(condition = conditions)

    meta <- S4Vectors::metadata(x)
    meta$conditions <- cond_df

    new_pe <- PhysioExperiment(
      assays = new_assays,
      colData = SummarizedExperiment::colData(x),
      metadata = meta,
      samplingRate = samplingRate(x)
    )
  }

  new_pe
}

#' Grand average across subjects/samples
#'
#' Computes grand average across multiple PhysioExperiment objects.
#'
#' @param ... PhysioExperiment objects or a list of them.
#' @return A PhysioExperiment object with grand averaged data.
#' @export
grandAverage <- function(...) {
  objects <- list(...)
  if (length(objects) == 1 && is.list(objects[[1]])) {
    objects <- objects[[1]]
  }

  if (length(objects) < 2) {
    stop("At least 2 objects required for grand average", call. = FALSE)
  }

  # Validate all objects
  for (i in seq_along(objects)) {
    if (!inherits(objects[[i]], "PhysioExperiment")) {
      stop("All objects must be PhysioExperiment instances", call. = FALSE)
    }
  }

  # Get reference dimensions from first object
  ref <- objects[[1]]
  assay_name <- defaultAssay(ref)
  ref_data <- SummarizedExperiment::assay(ref, assay_name)
  ref_dims <- dim(ref_data)
  n_dims <- length(ref_dims)

  # Stack all data along a new dimension
  all_data <- array(NA_real_, dim = c(ref_dims, length(objects)))

  for (i in seq_along(objects)) {
    obj <- objects[[i]]
    data <- SummarizedExperiment::assay(obj, defaultAssay(obj))

    if (!identical(dim(data), ref_dims)) {
      stop(sprintf("Object %d has different dimensions", i), call. = FALSE)
    }

    # Dynamic indexing based on number of dimensions
    if (n_dims == 2) {
      all_data[, , i] <- data
    } else if (n_dims == 3) {
      all_data[, , , i] <- data
    } else if (n_dims == 4) {
      all_data[, , , , i] <- data
    } else {
      # Generic approach for any dimension using do.call
      # Build index list: all dimensions get TRUE (select all), last dim gets i
      idx_list <- c(
        replicate(n_dims, substitute(), simplify = FALSE),
        list(i)
      )
      all_data <- do.call(`[<-`, c(list(all_data), idx_list, list(value = data)))
    }
  }

  # Compute mean across objects (last dimension)
  grand_avg <- apply(all_data, seq_along(ref_dims), mean, na.rm = TRUE)

  new_assays <- S4Vectors::SimpleList(grand_average = grand_avg)

  PhysioExperiment(
    assays = new_assays,
    colData = SummarizedExperiment::colData(ref),
    metadata = list(n_subjects = length(objects)),
    samplingRate = samplingRate(ref)
  )
}

#' Get epoch time vector
#'
#' Returns the time vector for epoched data relative to event onset.
#'
#' @param x An epoched PhysioExperiment object.
#' @return Numeric vector of times in seconds.
#' @export
epochTimes <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  meta <- S4Vectors::metadata(x)
  tmin <- meta$epoch_tmin
  tmax <- meta$epoch_tmax

  if (is.null(tmin) || is.null(tmax)) {
    stop("Object does not contain epoch timing information", call. = FALSE)
  }

  sr <- samplingRate(x)
  seq(tmin, tmax, by = 1 / sr)
}
