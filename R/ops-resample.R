#' Resampling operations for PhysioExperiment
#'
#' Functions for resampling signal data to different sampling rates.

#' Resample signal data
#'
#' Resamples the signal data to a target sampling rate using interpolation.
#'
#' @param x A PhysioExperiment object.
#' @param target_rate Target sampling rate in Hz.
#' @param method Resampling method: "linear" (default), "spline", or "fft".
#' @param assay_name Optional assay name. If NULL, uses the default assay.
#' @param output_assay Name for the output assay. Default is "resampled".
#' @return A new PhysioExperiment object with resampled data.
#' @export
resample <- function(x, target_rate, method = c("linear", "spline", "fft"),
                     assay_name = NULL, output_assay = "resampled") {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate required for resampling", call. = FALSE)
  }

  if (target_rate <= 0) {
    stop("Target rate must be positive", call. = FALSE)
  }

  if (abs(sr - target_rate) < 1e-6) {
    message("Target rate equals current rate; no resampling needed")
    return(x)
  }

  if (is.null(assay_name)) {
    assay_name <- defaultAssay(x)
  }

  if (is.na(assay_name)) {
    stop("No assays available", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  n_old <- dims[1]

  # Calculate new length
  duration <- n_old / sr
  n_new <- as.integer(round(duration * target_rate))

  # Time vectors
  t_old <- seq(0, duration, length.out = n_old)
  t_new <- seq(0, duration, length.out = n_new)

  # Resample based on dimensionality
  if (length(dims) == 2) {
    resampled <- .resampleMatrix(data, t_old, t_new, method)
  } else if (length(dims) == 3) {
    resampled <- array(NA_real_, dim = c(n_new, dims[2], dims[3]))
    for (s in seq_len(dims[3])) {
      resampled[, , s] <- .resampleMatrix(data[, , s], t_old, t_new, method)
    }
  } else {
    stop("Only 2D or 3D arrays supported", call. = FALSE)
  }

  # Create new rowData matching new time points
  new_row_data <- S4Vectors::DataFrame(time_idx = seq_len(n_new))

  # Create new PhysioExperiment with new sampling rate
  new_pe <- PhysioExperiment(
    assays = S4Vectors::SimpleList(resampled),
    rowData = new_row_data,
    colData = SummarizedExperiment::colData(x),
    metadata = S4Vectors::metadata(x),
    samplingRate = target_rate
  )

  names(SummarizedExperiment::assays(new_pe)) <- output_assay
  new_pe
}

#' Resample matrix helper
#' @noRd
.resampleMatrix <- function(data, t_old, t_new, method) {
  n_new <- length(t_new)
  n_channels <- ncol(data)
  result <- matrix(NA_real_, nrow = n_new, ncol = n_channels)

  for (ch in seq_len(n_channels)) {
    vec <- data[, ch]

    if (method == "linear") {
      result[, ch] <- stats::approx(t_old, vec, t_new, method = "linear")$y
    } else if (method == "spline") {
      result[, ch] <- stats::spline(t_old, vec, xout = t_new, method = "natural")$y
    } else if (method == "fft") {
      result[, ch] <- .resampleFFT(vec, length(t_new))
    }
  }

  result
}

#' FFT-based resampling
#' @noRd
.resampleFFT <- function(x, n_new) {
  n_old <- length(x)

  if (n_new == n_old) return(x)

  # Compute FFT
  fft_x <- stats::fft(x)

  if (n_new > n_old) {
    # Upsample: zero-pad in frequency domain
    half <- floor(n_old / 2)
    fft_new <- complex(n_new)
    fft_new[1:(half + 1)] <- fft_x[1:(half + 1)]
    fft_new[(n_new - half + 1):n_new] <- fft_x[(n_old - half + 1):n_old]
  } else {
    # Downsample: truncate in frequency domain
    half <- floor(n_new / 2)
    fft_new <- complex(n_new)
    fft_new[1:(half + 1)] <- fft_x[1:(half + 1)]
    fft_new[(n_new - half + 1):n_new] <- fft_x[(n_old - half + 1):(n_old - half + n_new - half)]
  }

  # Inverse FFT and scale
  Re(stats::fft(fft_new, inverse = TRUE)) / n_old * n_new
}

#' Decimate signal
#'
#' Downsamples by an integer factor with anti-aliasing filter.
#'
#' @param x A PhysioExperiment object.
#' @param factor Integer decimation factor.
#' @param filter_order Order of the anti-aliasing lowpass filter.
#' @param output_assay Name for the output assay.
#' @return A new PhysioExperiment with decimated data.
#' @export
decimate <- function(x, factor, filter_order = 8L, output_assay = "decimated") {
  stopifnot(inherits(x, "PhysioExperiment"))
  factor <- as.integer(factor)

  if (factor < 2) {
    stop("Decimation factor must be >= 2", call. = FALSE)
  }

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate required", call. = FALSE)
  }

  # Apply anti-aliasing filter at Nyquist/factor
  cutoff <- (sr / 2) / factor * 0.8  # 80% of new Nyquist
  x_filtered <- butterworthFilter(x, high = cutoff, type = "low",
                                   order = filter_order, output_assay = ".temp")

  data <- SummarizedExperiment::assay(x_filtered, ".temp")
  dims <- dim(data)

  # Decimate
  indices <- seq(1, dims[1], by = factor)

  if (length(dims) == 2) {
    decimated <- data[indices, , drop = FALSE]
  } else if (length(dims) == 3) {
    decimated <- data[indices, , , drop = FALSE]
  } else {
    stop("Only 2D or 3D arrays supported", call. = FALSE)
  }

  # Create new rowData matching new time points
  n_new <- length(indices)
  new_row_data <- S4Vectors::DataFrame(time_idx = seq_len(n_new))

  # Create new object
  new_pe <- PhysioExperiment(
    assays = S4Vectors::SimpleList(decimated),
    rowData = new_row_data,
    colData = SummarizedExperiment::colData(x),
    metadata = S4Vectors::metadata(x),
    samplingRate = sr / factor
  )

  names(SummarizedExperiment::assays(new_pe)) <- output_assay
  new_pe
}

#' Interpolate signal
#'
#' Upsamples by an integer factor with interpolation.
#'
#' @param x A PhysioExperiment object.
#' @param factor Integer interpolation factor.
#' @param method Interpolation method: "linear" or "spline".
#' @param output_assay Name for the output assay.
#' @return A new PhysioExperiment with interpolated data.
#' @export
interpolate <- function(x, factor, method = c("linear", "spline"),
                        output_assay = "interpolated") {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)
  factor <- as.integer(factor)

  if (factor < 2) {
    stop("Interpolation factor must be >= 2", call. = FALSE)
  }

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate required", call. = FALSE)
  }

  target_rate <- sr * factor
  resample(x, target_rate, method = method, output_assay = output_assay)
}

#' Get sampling rates for all assays
#'
#' Returns sampling rates associated with each assay.
#'
#' @param x A PhysioExperiment object.
#' @return Named numeric vector of sampling rates.
#' @export
assaySamplingRates <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  meta <- S4Vectors::metadata(x)
  rates <- meta$assay_sampling_rates

  if (is.null(rates)) {
    # Default: all assays share the main sampling rate
    assay_names <- SummarizedExperiment::assayNames(x)
    sr <- samplingRate(x)
    rates <- rep(sr, length(assay_names))
    names(rates) <- assay_names
  }

  rates
}

#' Set sampling rate for a specific assay
#'
#' @param x A PhysioExperiment object.
#' @param assay_name Name of the assay.
#' @param rate Sampling rate for the assay.
#' @return Modified PhysioExperiment object.
#' @export
setAssaySamplingRate <- function(x, assay_name, rate) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (!assay_name %in% SummarizedExperiment::assayNames(x)) {
    stop("Assay not found: ", assay_name, call. = FALSE)
  }

  meta <- S4Vectors::metadata(x)
  rates <- meta$assay_sampling_rates

  if (is.null(rates)) {
    assay_names <- SummarizedExperiment::assayNames(x)
    rates <- rep(samplingRate(x), length(assay_names))
    names(rates) <- assay_names
  }

  rates[[assay_name]] <- rate
  meta$assay_sampling_rates <- rates
  S4Vectors::metadata(x) <- meta

  x
}
