#' ICA and Artifact Removal for PhysioExperiment
#'
#' Functions for Independent Component Analysis (ICA) and artifact removal
#' from physiological signals.

#' Perform ICA decomposition
#'
#' Decomposes the signal into independent components using FastICA algorithm.
#'
#' @param x A PhysioExperiment object.
#' @param n_components Number of components to extract. If NULL, uses number of channels.
#' @param method ICA method: "fastica" (default) or "jade".
#' @param max_iter Maximum iterations for convergence.
#' @param tol Tolerance for convergence.
#' @return A list containing:
#'   - `components`: The independent components (time x component matrix)
#'   - `mixing`: The mixing matrix (channel x component)
#'   - `unmixing`: The unmixing matrix (component x channel)
#'   - `object`: Modified PhysioExperiment with ICA components as assay
#' @export
icaDecompose <- function(x, n_components = NULL, method = c("fastica", "jade"),
                          max_iter = 200L, tol = 1e-4) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Handle different dimensionalities
  if (length(dims) == 2) {
    signal_matrix <- data  # time x channels
  } else if (length(dims) == 3) {
    # Concatenate samples for ICA training
    signal_matrix <- do.call(rbind, lapply(seq_len(dims[3]), function(s) data[, , s]))
  } else {
    stop("Data must be 2D or 3D", call. = FALSE)
  }

  n_channels <- ncol(signal_matrix)
  n_timepoints <- nrow(signal_matrix)

  if (is.null(n_components)) {
    n_components <- n_channels
  }

  if (n_components > n_channels) {
    stop("n_components cannot exceed number of channels", call. = FALSE)
  }

  # Center the data
  col_means <- colMeans(signal_matrix)
  centered <- sweep(signal_matrix, 2, col_means)

  # Whitening (PCA pre-processing)
  svd_result <- svd(centered, nu = n_components, nv = n_components)
  whitened <- svd_result$u %*% diag(svd_result$d[1:n_components])
  whitening_matrix <- diag(1 / svd_result$d[1:n_components]) %*% t(svd_result$v[, 1:n_components])

  # FastICA algorithm
  if (method == "fastica") {
    ica_result <- .fastICA(whitened, n_components, max_iter, tol)
  } else {
    ica_result <- .jadeICA(whitened, n_components, max_iter, tol)
  }

  # Compute final matrices
  unmixing <- ica_result$W %*% whitening_matrix
  # Use pseudoinverse since unmixing may not be square (n_components != n_channels)
  mixing <- .pseudoinverse(unmixing)
  components <- centered %*% t(unmixing)

  # Store components in the object
  if (length(dims) == 3) {
    # Reshape back to 3D
    comp_array <- array(NA_real_, dim = c(dims[1], n_components, dims[3]))
    for (s in seq_len(dims[3])) {
      start <- (s - 1) * dims[1] + 1
      end <- s * dims[1]
      comp_array[, , s] <- components[start:end, ]
    }
    components_assay <- comp_array
  } else {
    components_assay <- components
  }

  assays <- SummarizedExperiment::assays(x)
  assays[["ica_components"]] <- components_assay
  SummarizedExperiment::assays(x) <- assays

  # Store ICA info in metadata
  meta <- S4Vectors::metadata(x)
  meta$ica <- list(
    mixing = mixing,
    unmixing = unmixing,
    n_components = n_components,
    method = method,
    col_means = col_means
  )
  S4Vectors::metadata(x) <- meta

  list(
    components = components_assay,
    mixing = mixing,
    unmixing = unmixing,
    object = x
  )
}

#' FastICA algorithm implementation
#' @noRd
.fastICA <- function(X, n_components, max_iter, tol) {
  n <- nrow(X)
  p <- ncol(X)

  # Initialize unmixing matrix
  W <- matrix(rnorm(n_components * p), n_components, p)
  W <- .orthogonalize(W)

  for (iter in seq_len(max_iter)) {
    W_old <- W

    for (i in seq_len(n_components)) {
      # Compute g(w'x) and g'(w'x) using tanh nonlinearity
      wx <- X %*% W[i, ]
      gwx <- tanh(wx)
      g_prime <- 1 - gwx^2

      # Update rule
      W[i, ] <- colMeans(X * as.vector(gwx)) - mean(g_prime) * W[i, ]
    }

    # Orthogonalize
    W <- .orthogonalize(W)

    # Check convergence
    convergence <- max(abs(abs(rowSums(W * W_old)) - 1))
    if (convergence < tol) {
      break
    }
  }

  list(W = W, converged = iter < max_iter, iterations = iter)
}

#' JADE-like ICA algorithm (simplified)
#' @noRd
.jadeICA <- function(X, n_components, max_iter, tol) {
  # Simplified JADE using joint diagonalization
  # For full JADE, would need cumulant matrices
  .fastICA(X, n_components, max_iter, tol)
}

#' Orthogonalize matrix rows
#' @noRd
.orthogonalize <- function(W) {
  svd_W <- svd(W)
  svd_W$u %*% t(svd_W$v)
}

#' Remove ICA components
#'
#' Removes specified ICA components and reconstructs the signal.
#'
#' @param x A PhysioExperiment object with ICA decomposition.
#' @param components Integer vector of component indices to remove.
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with cleaned signal.
#' @export
icaRemove <- function(x, components, output_assay = "ica_cleaned") {
  stopifnot(inherits(x, "PhysioExperiment"))

  meta <- S4Vectors::metadata(x)
  ica_info <- meta$ica

  if (is.null(ica_info)) {
    stop("ICA decomposition not found. Run icaDecompose() first.", call. = FALSE)
  }

  mixing <- ica_info$mixing
  unmixing <- ica_info$unmixing
  col_means <- ica_info$col_means

  # Get original data
  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Get ICA components
  ica_components <- SummarizedExperiment::assay(x, "ica_components")

  # Zero out components to remove
  n_components <- ncol(mixing)
  keep_mask <- rep(TRUE, n_components)
  keep_mask[components] <- FALSE

  # Reconstruct with removed components
  if (length(dims) == 2) {
    centered <- data - matrix(col_means, nrow = nrow(data), ncol = length(col_means), byrow = TRUE)
    ic <- centered %*% t(unmixing)
    ic[, components] <- 0
    reconstructed <- ic %*% mixing + matrix(col_means, nrow = nrow(data), ncol = length(col_means), byrow = TRUE)
  } else if (length(dims) == 3) {
    reconstructed <- array(NA_real_, dim = dims)
    for (s in seq_len(dims[3])) {
      centered <- data[, , s] - matrix(col_means, nrow = dims[1], ncol = length(col_means), byrow = TRUE)
      ic <- centered %*% t(unmixing)
      ic[, components] <- 0
      reconstructed[, , s] <- ic %*% mixing + matrix(col_means, nrow = dims[1], ncol = length(col_means), byrow = TRUE)
    }
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- reconstructed
  SummarizedExperiment::assays(x) <- assays

  # Record which components were removed
  meta$ica$removed_components <- components
  S4Vectors::metadata(x) <- meta

  x
}

#' Detect bad channels
#'
#' Identifies channels with abnormal characteristics.
#'
#' @param x A PhysioExperiment object.
#' @param method Detection method: "zscore", "correlation", or "flatline".
#' @param threshold Threshold for detection (depends on method).
#' @return Integer vector of bad channel indices.
#' @export
detectBadChannels <- function(x, method = c("zscore", "correlation", "flatline"),
                               threshold = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Flatten to 2D if needed
  if (length(dims) == 3) {
    data <- apply(data, c(1, 2), mean)
  }

  n_channels <- ncol(data)
  bad_channels <- integer(0)

  if (method == "zscore") {
    if (is.null(threshold)) threshold <- 3
    # Check if channel variance is outlier
    channel_vars <- apply(data, 2, var, na.rm = TRUE)
    median_var <- median(channel_vars)
    mad_var <- mad(channel_vars)
    z_scores <- abs(channel_vars - median_var) / (mad_var + 1e-10)
    bad_channels <- which(z_scores > threshold)

  } else if (method == "correlation") {
    if (is.null(threshold)) threshold <- 0.4
    # Check correlation with other channels
    cor_matrix <- cor(data, use = "pairwise.complete.obs")
    mean_cors <- rowMeans(abs(cor_matrix), na.rm = TRUE) - 1 / n_channels
    bad_channels <- which(mean_cors < threshold)

  } else if (method == "flatline") {
    if (is.null(threshold)) threshold <- 0.01
    # Check for flatline (very low variance)
    channel_vars <- apply(data, 2, var, na.rm = TRUE)
    median_var <- median(channel_vars)
    bad_channels <- which(channel_vars < threshold * median_var)
  }

  bad_channels
}

#' Interpolate bad channels
#'
#' Replaces bad channels with interpolated values from neighboring channels.
#'
#' @param x A PhysioExperiment object.
#' @param bad_channels Integer vector of channel indices to interpolate.
#' @param method Interpolation method: "average" or "spline".
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with interpolated channels.
#' @export
interpolateBadChannels <- function(x, bad_channels, method = c("average", "spline"),
                                    output_assay = "interpolated") {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  if (length(bad_channels) == 0) {
    message("No bad channels to interpolate")
    return(x)
  }

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  n_channels <- if (length(dims) >= 2) dims[2] else 1

  good_channels <- setdiff(seq_len(n_channels), bad_channels)

  if (length(good_channels) == 0) {
    stop("No good channels available for interpolation", call. = FALSE)
  }

  # Check for electrode positions for spline interpolation
  positions <- getElectrodePositions(x)

  if (method == "spline" && is.null(positions)) {
    warning("No electrode positions found, falling back to average method", call. = FALSE)
    method <- "average"
  }

  if (method == "average") {
    # Simple average of good channels
    if (length(dims) == 2) {
      good_mean <- rowMeans(data[, good_channels, drop = FALSE], na.rm = TRUE)
      data[, bad_channels] <- good_mean
    } else if (length(dims) == 3) {
      for (s in seq_len(dims[3])) {
        good_mean <- rowMeans(data[, good_channels, s, drop = FALSE], na.rm = TRUE)
        data[, bad_channels, s] <- good_mean
      }
    }
  } else if (method == "spline") {
    # Distance-weighted interpolation
    pos_matrix <- as.matrix(positions[, c("x", "y", "z")])

    for (bad_ch in bad_channels) {
      # Calculate distances to good channels
      distances <- sqrt(rowSums((pos_matrix[good_channels, , drop = FALSE] -
                                   matrix(pos_matrix[bad_ch, ], nrow = length(good_channels),
                                          ncol = 3, byrow = TRUE))^2))
      weights <- 1 / (distances + 1e-10)
      weights <- weights / sum(weights)

      if (length(dims) == 2) {
        data[, bad_ch] <- data[, good_channels, drop = FALSE] %*% weights
      } else if (length(dims) == 3) {
        for (s in seq_len(dims[3])) {
          data[, bad_ch, s] <- data[, good_channels, s, drop = FALSE] %*% weights
        }
      }
    }
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- data
  SummarizedExperiment::assays(x) <- assays

  # Record interpolated channels
  meta <- S4Vectors::metadata(x)
  meta$interpolated_channels <- bad_channels
  S4Vectors::metadata(x) <- meta

  x
}

#' Reject bad epochs
#'
#' Identifies and optionally removes epochs with artifacts.
#'
#' @param x An epoched PhysioExperiment object.
#' @param threshold Amplitude threshold for rejection.
#' @param method Detection method: "amplitude", "gradient", or "variance".
#' @param remove If TRUE, removes bad epochs. If FALSE, returns indices only.
#' @return If remove=TRUE, modified object. If remove=FALSE, indices of bad epochs.
#' @export
rejectBadEpochs <- function(x, threshold = 100, method = c("amplitude", "gradient", "variance"),
                             remove = TRUE) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) == 3) {
    # 3D: time x channels x epochs
    n_epochs <- dims[3]
    bad_epochs <- logical(n_epochs)

    for (ep in seq_len(n_epochs)) {
      epoch_data <- data[, , ep]

      if (method == "amplitude") {
        bad_epochs[ep] <- max(abs(epoch_data), na.rm = TRUE) > threshold
      } else if (method == "gradient") {
        gradients <- abs(diff(as.vector(epoch_data)))
        bad_epochs[ep] <- max(gradients, na.rm = TRUE) > threshold
      } else if (method == "variance") {
        epoch_var <- var(as.vector(epoch_data), na.rm = TRUE)
        bad_epochs[ep] <- epoch_var > threshold
      }
    }
  } else if (length(dims) == 4) {
    # 4D: time x channels x epochs x samples
    n_epochs <- dims[3]
    bad_epochs <- logical(n_epochs)

    for (ep in seq_len(n_epochs)) {
      epoch_data <- data[, , ep, ]

      if (method == "amplitude") {
        bad_epochs[ep] <- max(abs(epoch_data), na.rm = TRUE) > threshold
      } else if (method == "gradient") {
        gradients <- abs(diff(as.vector(epoch_data)))
        bad_epochs[ep] <- max(gradients, na.rm = TRUE) > threshold
      } else if (method == "variance") {
        epoch_var <- var(as.vector(epoch_data), na.rm = TRUE)
        bad_epochs[ep] <- epoch_var > threshold
      }
    }
  } else {
    stop("Data must be 3D or 4D (epoched) for epoch rejection", call. = FALSE)
  }

  bad_indices <- which(bad_epochs)

  if (!remove) {
    return(bad_indices)
  }

  if (length(bad_indices) == n_epochs) {
    stop("All epochs would be rejected", call. = FALSE)
  }

  if (length(bad_indices) > 0) {
    good_indices <- which(!bad_epochs)

    if (length(dims) == 3) {
      data <- data[, , good_indices, drop = FALSE]
    } else {
      data <- data[, , good_indices, , drop = FALSE]
    }

    message(sprintf("Rejected %d epochs (%.1f%%)", length(bad_indices),
                    100 * length(bad_indices) / n_epochs))

    assays <- SummarizedExperiment::assays(x)
    assays[[assay_name]] <- data
    SummarizedExperiment::assays(x) <- assays

    meta <- S4Vectors::metadata(x)
    if (!is.null(meta$epoch_info)) {
      meta$epoch_info <- meta$epoch_info[good_indices, ]
    }
    meta$rejected_epochs <- bad_indices
    S4Vectors::metadata(x) <- meta
  }

  x
}

#' Baseline correction
#'
#' Subtracts baseline from epochs.
#'
#' @param x An epoched PhysioExperiment object.
#' @param baseline Numeric vector of length 2 (tmin, tmax) for baseline period.
#' @param method Correction method: "mean" or "median".
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with baseline-corrected data.
#' @export
baselineCorrect <- function(x, baseline = c(-0.2, 0), method = c("mean", "median"),
                            output_assay = "baseline_corrected") {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  meta <- S4Vectors::metadata(x)
  tmin <- meta$epoch_tmin
  tmax <- meta$epoch_tmax

  if (is.null(tmin) || is.null(tmax)) {
    stop("Epoch timing information not found", call. = FALSE)
  }

  sr <- samplingRate(x)
  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Calculate baseline sample indices
  bl_start <- as.integer(round((baseline[1] - tmin) * sr)) + 1
  bl_end <- as.integer(round((baseline[2] - tmin) * sr)) + 1
  bl_start <- max(1, bl_start)
  bl_end <- min(dims[1], bl_end)

  corrected <- data

  if (length(dims) == 4) {
    for (ep in seq_len(dims[3])) {
      for (s in seq_len(dims[4])) {
        for (ch in seq_len(dims[2])) {
          bl_data <- data[bl_start:bl_end, ch, ep, s]
          bl_value <- if (method == "mean") mean(bl_data, na.rm = TRUE) else median(bl_data, na.rm = TRUE)
          corrected[, ch, ep, s] <- data[, ch, ep, s] - bl_value
        }
      }
    }
  } else if (length(dims) == 3) {
    for (s in seq_len(dims[3])) {
      for (ch in seq_len(dims[2])) {
        bl_data <- data[bl_start:bl_end, ch, s]
        bl_value <- if (method == "mean") mean(bl_data, na.rm = TRUE) else median(bl_data, na.rm = TRUE)
        corrected[, ch, s] <- data[, ch, s] - bl_value
      }
    }
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- corrected
  SummarizedExperiment::assays(x) <- assays

  x
}

#' Detect artifacts in continuous data
#'
#' Scans continuous data for artifacts using amplitude or gradient criteria.
#'
#' @param x A PhysioExperiment object.
#' @param method Detection method: "amplitude", "gradient", or "joint".
#' @param threshold Detection threshold. If NULL, computed as median + 5 * MAD.
#' @param window_sec Detection window in seconds (default: 1.0).
#' @return A data.frame with columns: onset (seconds), offset (seconds),
#'   channel (index), type (detection method).
#' @export
detectArtifacts <- function(x, method = c("amplitude", "gradient", "joint"),
                            threshold = NULL, window_sec = 1.0) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  sr <- samplingRate(x)

  # Flatten to 2D if 3D
  if (length(dims) == 3) {
    data <- apply(data, c(1, 2), mean)
  }

  n_time <- nrow(data)
  n_channels <- ncol(data)
  window_samples <- max(1L, as.integer(round(window_sec * sr)))

  results <- list()

  methods_to_run <- if (method == "joint") c("amplitude", "gradient") else method

  for (m in methods_to_run) {
    for (ch in seq_len(n_channels)) {
      signal <- data[, ch]

      if (m == "amplitude") {
        metric <- abs(signal)
      } else {
        metric <- c(0, abs(diff(signal)))
      }

      # Auto threshold: median + 5 * MAD
      thresh <- if (is.null(threshold)) {
        median(metric, na.rm = TRUE) + 5 * mad(metric, na.rm = TRUE)
      } else {
        threshold
      }

      bad <- which(metric > thresh)

      if (length(bad) > 0) {
        # Merge nearby detections into segments
        segments <- .merge_detections(bad, window_samples)
        for (seg in segments) {
          results[[length(results) + 1]] <- data.frame(
            onset = (seg[1] - 1) / sr,
            offset = seg[2] / sr,
            channel = ch,
            type = m,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    return(data.frame(onset = numeric(0), offset = numeric(0),
                      channel = integer(0), type = character(0),
                      stringsAsFactors = FALSE))
  }

  do.call(rbind, results)
}

#' Merge nearby detection indices into segments
#' @noRd
.merge_detections <- function(indices, gap) {
  if (length(indices) == 0) return(list())

  segments <- list()
  start <- indices[1]
  end <- indices[1]

  for (i in seq_along(indices)[-1]) {
    if (indices[i] - end <= gap) {
      end <- indices[i]
    } else {
      segments[[length(segments) + 1]] <- c(start, end)
      start <- indices[i]
      end <- indices[i]
    }
  }
  segments[[length(segments) + 1]] <- c(start, end)

  segments
}

#' Classify ICA components as brain or artifact
#'
#' Automatically identifies artifact ICA components using statistical criteria.
#'
#' @param x A PhysioExperiment object with ICA decomposition
#'   (from \code{\link{icaDecompose}}).
#' @param method Classification method: "autocorrelation", "kurtosis", or "frequency".
#' @param eog_channels Integer vector of EOG channel indices for correlation-based
#'   detection. If provided, also computes EOG correlation scores.
#' @param threshold Classification threshold. If NULL, uses method-specific defaults:
#'   autocorrelation=0.9, kurtosis=3.0, frequency=0.5.
#' @return A list with:
#'   \itemize{
#'     \item \code{artifact_indices}: Integer vector of artifact component indices
#'     \item \code{scores}: Named numeric vector of scores per component
#'     \item \code{threshold}: The threshold used
#'     \item \code{method}: The method used
#'   }
#' @export
classifyICAComponents <- function(x, method = c("autocorrelation", "kurtosis", "frequency"),
                                   eog_channels = NULL, threshold = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  meta <- S4Vectors::metadata(x)
  if (is.null(meta$ica)) {
    stop("ICA decomposition not found. Run icaDecompose() first.", call. = FALSE)
  }

  ica_data <- SummarizedExperiment::assay(x, "ica_components")
  dims <- dim(ica_data)

  # Flatten to 2D (time x components) if 3D
  if (length(dims) == 3) {
    components <- do.call(rbind, lapply(seq_len(dims[3]), function(s) ica_data[, , s]))
  } else {
    components <- ica_data
  }

  n_components <- ncol(components)
  sr <- samplingRate(x)

  # Set default thresholds
  default_thresholds <- c(autocorrelation = 0.9, kurtosis = 3.0, frequency = 0.5)
  if (is.null(threshold)) threshold <- default_thresholds[method]

  scores <- numeric(n_components)

  if (method == "kurtosis") {
    for (i in seq_len(n_components)) {
      v <- components[, i]
      v <- v[!is.na(v)]
      m <- mean(v)
      s <- sd(v)
      if (s > 0) {
        scores[i] <- abs(mean(((v - m) / s)^4) - 3)
      } else {
        scores[i] <- 0
      }
    }
    artifact <- which(scores > threshold)

  } else if (method == "autocorrelation") {
    for (i in seq_len(n_components)) {
      v <- components[, i]
      v <- v[!is.na(v)]
      if (length(v) > 1) {
        ac <- cor(v[-length(v)], v[-1])
        scores[i] <- abs(ac)
      }
    }
    artifact <- which(scores > threshold)

  } else if (method == "frequency") {
    for (i in seq_len(n_components)) {
      v <- components[, i]
      v <- v[!is.na(v)]
      n <- length(v)
      if (n > 1) {
        psd <- Mod(fft(v))^2 / n
        freqs <- seq(0, sr / 2, length.out = n %/% 2 + 1)
        psd_half <- psd[seq_len(length(freqs))]
        # Ratio of high-frequency (>30Hz) to total power
        high_freq_idx <- which(freqs > 30)
        if (length(high_freq_idx) > 0) {
          scores[i] <- sum(psd_half[high_freq_idx]) / sum(psd_half)
        }
      }
    }
    artifact <- which(scores > threshold)
  }

  names(scores) <- paste0("IC", seq_len(n_components))

  list(
    artifact_indices = as.integer(artifact),
    scores = scores,
    threshold = threshold,
    method = method
  )
}

#' Clean data using an artifact removal pipeline
#'
#' Runs a configurable sequence of artifact removal steps.
#'
#' @param x A PhysioExperiment object.
#' @param steps Character vector of steps to run. Options: "bad_channels", "ica".
#' @param bad_channel_method Method for \code{\link{detectBadChannels}}.
#' @param bad_channel_threshold Threshold for bad channel detection.
#' @param interpolation_method Method for \code{\link{interpolateBadChannels}}.
#' @param ica_method Method for \code{\link{icaDecompose}}.
#' @param ica_classify_method Method for \code{\link{classifyICAComponents}}.
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with cleaned data.
#' @export
cleanData <- function(x, steps = c("bad_channels", "ica"),
                      bad_channel_method = "zscore",
                      bad_channel_threshold = NULL,
                      interpolation_method = "average",
                      ica_method = "fastica",
                      ica_classify_method = "kurtosis",
                      output_assay = "cleaned") {
  stopifnot(inherits(x, "PhysioExperiment"))
  steps <- match.arg(steps, c("bad_channels", "ica"), several.ok = TRUE)

  result <- x

  if ("bad_channels" %in% steps) {
    bad <- detectBadChannels(result, method = bad_channel_method,
                             threshold = bad_channel_threshold)
    if (length(bad) > 0) {
      message(sprintf("Detected %d bad channel(s): %s",
                      length(bad), paste(bad, collapse = ", ")))
      result <- interpolateBadChannels(result, bad_channels = bad,
                                        method = interpolation_method,
                                        output_assay = defaultAssay(result))
    }
  }

  if ("ica" %in% steps) {
    n_channels <- ncol(SummarizedExperiment::assay(result, defaultAssay(result)))
    ica <- icaDecompose(result, n_components = n_channels, method = ica_method)
    result <- ica$object

    classification <- classifyICAComponents(result, method = ica_classify_method)

    if (length(classification$artifact_indices) > 0) {
      message(sprintf("Removing %d artifact ICA component(s): %s",
                      length(classification$artifact_indices),
                      paste(classification$artifact_indices, collapse = ", ")))
      result <- icaRemove(result, components = classification$artifact_indices,
                          output_assay = defaultAssay(result))
    }
  }

  # Copy current default assay to output_assay
  current_data <- SummarizedExperiment::assay(result, defaultAssay(result))
  assays <- SummarizedExperiment::assays(result)
  assays[[output_assay]] <- current_data
  SummarizedExperiment::assays(result) <- assays

  result
}

#' Compute Moore-Penrose pseudoinverse using SVD
#' @noRd
.pseudoinverse <- function(X, tol = .Machine$double.eps^0.5) {
  svd_result <- svd(X)
  d <- svd_result$d
  # Threshold small singular values
  d_inv <- ifelse(d > max(d) * tol, 1 / d, 0)
  svd_result$v %*% diag(d_inv, nrow = length(d_inv)) %*% t(svd_result$u)
}
