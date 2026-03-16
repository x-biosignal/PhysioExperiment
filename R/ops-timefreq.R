#' Time-Frequency Analysis for PhysioExperiment
#'
#' Functions for time-frequency analysis including wavelet transforms,
#' spectrograms, and band power extraction.

#' Compute spectrogram (Short-Time Fourier Transform)
#'
#' Computes the spectrogram using STFT with proper power spectral density
#' normalization. The returned power values are PSD estimates in V^2/Hz
#' (one-sided spectrum with doubling of non-DC/Nyquist bins).
#'
#' @param x A PhysioExperiment object.
#' @param window_size Window size in samples.
#' @param overlap Overlap between windows (0-1).
#' @param window_type Window function: "hanning", "hamming", "blackman", or "rectangular".
#' @param channel Channel index to analyze.
#' @param sample Sample index (for 3D data).
#' @return A list containing:
#'   - `power`: Power spectral density matrix (frequency x time) in V^2/Hz
#'   - `frequencies`: Frequency vector
#'   - `times`: Time vector
#' @export
#' @examples
#' # Create example data
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000)),
#'   samplingRate = 250
#' )
#'
#' # Compute spectrogram for channel 1
#' spec <- spectrogram(pe, channel = 1)
#'
#' # Plot spectrogram
#' plotSpectrogram(spec, freq_range = c(1, 40))
spectrogram <- function(x, window_size = 256L, overlap = 0.5,
                        window_type = c("hanning", "hamming", "blackman", "rectangular"),
                        channel = 1L, sample = 1L) {
  stopifnot(inherits(x, "PhysioExperiment"))
  window_type <- match.arg(window_type)

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop(sprintf("Valid sampling rate required (got: %s). Set with samplingRate(x) <- rate",
                 if (is.na(sr)) "NA" else sr), call. = FALSE)
  }

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Extract signal
  if (length(dims) == 2) {
    signal <- data[, channel]
  } else if (length(dims) == 3) {
    signal <- data[, channel, sample]
  } else {
    stop(sprintf("Data must be 2D (time x channels) or 3D (time x channels x samples), got %dD",
                 length(dims)), call. = FALSE)
  }

  n <- length(signal)
  step_size <- as.integer(window_size * (1 - overlap))
  n_windows <- floor((n - window_size) / step_size) + 1

  # Generate window function
  window_func <- .getWindow(window_type, window_size)
  window_power <- sum(window_func^2)

  # Initialize output
  n_freqs <- floor(window_size / 2) + 1
  power_matrix <- matrix(NA_real_, nrow = n_freqs, ncol = n_windows)
  times <- numeric(n_windows)

  # Compute STFT with proper PSD normalization
  for (i in seq_len(n_windows)) {
    start <- (i - 1) * step_size + 1
    end <- start + window_size - 1
    segment <- signal[start:end] * window_func

    fft_result <- stats::fft(segment)
    psd <- Mod(fft_result[1:n_freqs])^2 / (sr * window_power)
    # One-sided spectrum: double non-DC and non-Nyquist bins
    psd[2:(n_freqs - 1)] <- 2 * psd[2:(n_freqs - 1)]
    power_matrix[, i] <- psd

    times[i] <- (start + end) / 2 / sr
  }

  # Frequency vector
  frequencies <- seq(0, sr / 2, length.out = n_freqs)

  list(
    power = power_matrix,
    frequencies = frequencies,
    times = times,
    sampling_rate = sr,
    window_size = window_size,
    overlap = overlap
  )
}

#' Wavelet transform
#'
#' Computes the continuous wavelet transform using Morlet wavelets.
#'
#' @param x A PhysioExperiment object.
#' @param frequencies Numeric vector of frequencies to analyze.
#' @param n_cycles Number of wavelet cycles (can be scalar or vector).
#' @param channel Channel index to analyze.
#' @param sample Sample index (for 3D data).
#' @param normalization Wavelet normalization method: \code{"L1"} (default,
#'   divides by sum of absolute values) or \code{"L2"} (divides by square root
#'   of sum of squared absolute values). L2 normalization preserves energy
#'   across frequencies and is preferred for power comparisons.
#' @return A list containing:
#'   - `power`: Power matrix (frequency x time)
#'   - `phase`: Phase matrix (frequency x time)
#'   - `frequencies`: Frequency vector
#'   - `times`: Time vector
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500)),
#'   samplingRate = 100
#' )
#'
#' # Compute wavelet transform (1-30 Hz)
#' wt <- waveletTransform(pe, frequencies = seq(1, 30), channel = 1)
#'
#' # Access power and phase
#' dim(wt$power)  # frequency x time
waveletTransform <- function(x, frequencies = seq(1, 40, by = 1),
                              n_cycles = 7, channel = 1L, sample = 1L,
                              normalization = c("L1", "L2")) {
  stopifnot(inherits(x, "PhysioExperiment"))
  normalization <- match.arg(normalization)

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop(sprintf("Valid sampling rate required (got: %s). Set with samplingRate(x) <- rate",
                 if (is.na(sr)) "NA" else sr), call. = FALSE)
  }

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Extract signal
  if (length(dims) == 2) {
    signal <- data[, channel]
  } else if (length(dims) == 3) {
    signal <- data[, channel, sample]
  } else {
    stop(sprintf("Data must be 2D (time x channels) or 3D (time x channels x samples), got %dD",
                 length(dims)), call. = FALSE)
  }

  n <- length(signal)
  n_freqs <- length(frequencies)

  # Handle n_cycles
  if (length(n_cycles) == 1) {
    n_cycles <- rep(n_cycles, n_freqs)
  }

  # Initialize output
  power_matrix <- matrix(NA_real_, nrow = n_freqs, ncol = n)
  phase_matrix <- matrix(NA_real_, nrow = n_freqs, ncol = n)

  # Compute wavelet transform for each frequency
  for (f_idx in seq_len(n_freqs)) {
    freq <- frequencies[f_idx]
    cycles <- n_cycles[f_idx]

    # Generate Morlet wavelet
    wavelet <- .morletWavelet(freq, sr, cycles, normalization = normalization)

    # Convolve signal with wavelet
    conv_result <- .convolveComplex(signal, wavelet)

    # Extract power and phase
    power_matrix[f_idx, ] <- Mod(conv_result)^2
    phase_matrix[f_idx, ] <- Arg(conv_result)
  }

  # Time vector
  times <- (seq_len(n) - 1) / sr

  list(
    power = power_matrix,
    phase = phase_matrix,
    frequencies = frequencies,
    times = times,
    sampling_rate = sr,
    n_cycles = n_cycles
  )
}

#' Generate Morlet wavelet
#' @noRd
.morletWavelet <- function(freq, sr, n_cycles,
                           normalization = c("L1", "L2")) {
  normalization <- match.arg(normalization)

  # Standard deviation of Gaussian
  sigma_t <- n_cycles / (2 * pi * freq)

  # Wavelet duration (6 sigma on each side)
  wavelet_duration <- 6 * sigma_t
  n_samples <- as.integer(2 * wavelet_duration * sr) + 1

  # Time vector centered at 0
  t <- seq(-wavelet_duration, wavelet_duration, length.out = n_samples)

  # Complex Morlet wavelet
  gaussian <- exp(-t^2 / (2 * sigma_t^2))
  sinusoid <- exp(2i * pi * freq * t)

  wavelet <- gaussian * sinusoid

  # Normalize
  if (normalization == "L1") {
    wavelet / sum(Mod(wavelet))
  } else {
    wavelet / sqrt(sum(Mod(wavelet)^2))
  }
}

#' Complex convolution
#' @noRd
.convolveComplex <- function(signal, wavelet) {
  n_signal <- length(signal)
  n_wavelet <- length(wavelet)

  # Pad signal for convolution
  n_fft <- n_signal + n_wavelet - 1
  n_fft <- 2^ceiling(log2(n_fft))  # Pad to power of 2

  signal_padded <- c(signal, rep(0, n_fft - n_signal))
  wavelet_padded <- c(wavelet, rep(0, n_fft - n_wavelet))

  # FFT convolution
  fft_signal <- stats::fft(signal_padded)
  fft_wavelet <- stats::fft(wavelet_padded)
  conv_result <- stats::fft(fft_signal * fft_wavelet, inverse = TRUE) / n_fft

  # Extract valid part (centered)
  half_wavelet <- floor(n_wavelet / 2)
  conv_result[(half_wavelet + 1):(half_wavelet + n_signal)]
}

#' Get window function
#' @noRd
.getWindow <- function(type, n) {
  t <- seq(0, 1, length.out = n)

  switch(type,
    hanning = 0.5 * (1 - cos(2 * pi * t)),
    hamming = 0.54 - 0.46 * cos(2 * pi * t),
    blackman = 0.42 - 0.5 * cos(2 * pi * t) + 0.08 * cos(4 * pi * t),
    rectangular = rep(1, n)
  )
}

#' Compute band power
#'
#' Extracts power in specified frequency bands.
#'
#' @param x A PhysioExperiment object.
#' @param bands Named list of frequency bands. Each element should be c(low, high).
#'   Default includes standard EEG bands.
#' @param method Method: "welch" (PSD) or "wavelet".
#' @param relative If TRUE, returns relative power (proportion of total).
#' @return A data.frame with band powers for each channel.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(2560), nrow = 256, ncol = 10)),
#'   rowData = S4Vectors::DataFrame(label = paste0("Ch", 1:10)),
#'   samplingRate = 256
#' )
#'
#' # Compute band power for standard EEG bands
#' bp <- bandPower(pe)
#' head(bp)
#'
#' # Compute relative band power
#' bp_rel <- bandPower(pe, relative = TRUE)
bandPower <- function(x, bands = NULL, method = c("welch", "wavelet"),
                      relative = FALSE) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  # Default EEG bands
  if (is.null(bands)) {
    bands <- list(
      delta = c(0.5, 4),
      theta = c(4, 8),
      alpha = c(8, 13),
      beta = c(13, 30),
      gamma = c(30, 100)
    )
  }

  sr <- samplingRate(x)
  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Flatten to 2D
  if (length(dims) == 3) {
    data <- apply(data, c(1, 2), mean)
    dims <- dim(data)
  }

  n_channels <- dims[2]
  n_bands <- length(bands)
  band_names <- names(bands)

  # Initialize result
  result <- matrix(NA_real_, nrow = n_channels, ncol = n_bands)
  colnames(result) <- band_names

  for (ch in seq_len(n_channels)) {
    signal <- data[, ch]

    if (method == "welch") {
      # Welch's method for PSD estimation
      psd <- .welchPSD(signal, sr)

      for (b in seq_len(n_bands)) {
        freq_range <- bands[[b]]
        freq_idx <- which(psd$frequencies >= freq_range[1] &
                            psd$frequencies <= freq_range[2])
        result[ch, b] <- sum(psd$power[freq_idx])
      }

    } else if (method == "wavelet") {
      # Wavelet-based power
      freqs <- seq(0.5, min(100, sr / 2 - 1), by = 0.5)
      wt <- waveletTransform(x, frequencies = freqs, channel = ch)

      for (b in seq_len(n_bands)) {
        freq_range <- bands[[b]]
        freq_idx <- which(wt$frequencies >= freq_range[1] &
                            wt$frequencies <= freq_range[2])
        result[ch, b] <- mean(wt$power[freq_idx, ])
      }
    }
  }

  # Convert to relative power if requested
  if (relative) {
    row_sums <- rowSums(result)
    result <- result / row_sums
  }

  # Create result data.frame
  ch_names <- channelNames(x)
  if (length(ch_names) != n_channels) {
    ch_names <- paste0("Ch", seq_len(n_channels))
  }

  result_df <- as.data.frame(result)
  result_df$channel <- ch_names
  result_df <- result_df[, c("channel", band_names)]

  result_df
}

#' Welch's PSD estimation
#' @noRd
.welchPSD <- function(signal, sr, nperseg = 256, noverlap = NULL,
                      one_sided = TRUE) {
  n <- length(signal)

  if (is.null(noverlap)) {
    noverlap <- floor(nperseg / 2)
  }

  step <- nperseg - noverlap
  n_segments <- floor((n - noverlap) / step)

  if (n_segments < 1) {
    nperseg <- n
    n_segments <- 1
    step <- nperseg
  }

  n_freqs <- floor(nperseg / 2) + 1
  psd <- numeric(n_freqs)

  window <- .getWindow("hanning", nperseg)
  window_sum <- sum(window^2)

  for (i in seq_len(n_segments)) {
    start <- (i - 1) * step + 1
    end <- start + nperseg - 1
    if (end > n) break

    segment <- signal[start:end] * window
    fft_result <- stats::fft(segment)
    psd <- psd + Mod(fft_result[1:n_freqs])^2
  }

  # Normalize
  psd <- psd / (n_segments * sr * window_sum)

  # Double power for one-sided spectrum (except DC and Nyquist)
  if (one_sided) {
    psd[2:(n_freqs - 1)] <- 2 * psd[2:(n_freqs - 1)]
  }

  frequencies <- seq(0, sr / 2, length.out = n_freqs)

  list(power = psd, frequencies = frequencies)
}

#' Hilbert transform for instantaneous amplitude/phase
#'
#' Computes the analytic signal using the Hilbert transform.
#' The analytic signal can be used to extract instantaneous amplitude
#' and phase.
#'
#' @param x A PhysioExperiment object.
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with analytic signal assay.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500)),
#'   samplingRate = 100
#' )
#'
#' # Compute Hilbert transform
#' pe <- hilbertTransform(pe)
#'
#' # Extract amplitude and phase
#' pe <- instantaneousAmplitude(pe)
#' pe <- instantaneousPhase(pe)
hilbertTransform <- function(x, output_assay = "analytic") {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  hilbert_1d <- function(signal) {
    n <- length(signal)
    fft_signal <- stats::fft(signal)

    # Create Hilbert transform filter
    h <- numeric(n)
    h[1] <- 1
    if (n %% 2 == 0) {
      h[2:(n / 2)] <- 2
      h[n / 2 + 1] <- 1
    } else {
      h[2:((n + 1) / 2)] <- 2
    }

    # Apply filter and inverse FFT
    analytic <- stats::fft(fft_signal * h, inverse = TRUE) / n
    analytic
  }

  if (length(dims) == 2) {
    result <- apply(data, 2, hilbert_1d)
  } else if (length(dims) == 3) {
    result <- array(NA_complex_, dim = dims)
    for (s in seq_len(dims[3])) {
      # Use drop=FALSE to preserve 2D matrix when dims[3]=1
      slice <- data[, , s, drop = FALSE]
      dim(slice) <- dims[1:2]
      result[, , s] <- apply(slice, 2, hilbert_1d)
    }
  } else {
    stop(sprintf("Data must be 2D (time x channels) or 3D (time x channels x samples), got %dD",
                 length(dims)), call. = FALSE)
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- result
  SummarizedExperiment::assays(x) <- assays

  x
}

#' Extract instantaneous amplitude (envelope)
#'
#' Extracts the instantaneous amplitude (envelope) from the analytic signal.
#'
#' @param x A PhysioExperiment object with analytic signal.
#' @param assay_name Name of the analytic signal assay.
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with amplitude assay.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500)),
#'   samplingRate = 100
#' )
#'
#' # First compute Hilbert transform, then extract amplitude
#' pe <- hilbertTransform(pe)
#' pe <- instantaneousAmplitude(pe)
instantaneousAmplitude <- function(x, assay_name = "analytic",
                                    output_assay = "amplitude") {
  stopifnot(inherits(x, "PhysioExperiment"))

  data <- SummarizedExperiment::assay(x, assay_name)
  amplitude <- Mod(data)

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- amplitude
  SummarizedExperiment::assays(x) <- assays

  x
}

#' Extract instantaneous phase
#'
#' Extracts the instantaneous phase from the analytic signal.
#'
#' @param x A PhysioExperiment object with analytic signal.
#' @param assay_name Name of the analytic signal assay.
#' @param output_assay Name for the output assay.
#' @return Modified PhysioExperiment with phase assay.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500)),
#'   samplingRate = 100
#' )
#'
#' # First compute Hilbert transform, then extract phase
#' pe <- hilbertTransform(pe)
#' pe <- instantaneousPhase(pe)
instantaneousPhase <- function(x, assay_name = "analytic",
                                output_assay = "phase") {
  stopifnot(inherits(x, "PhysioExperiment"))

  data <- SummarizedExperiment::assay(x, assay_name)
  phase <- Arg(data)

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- phase
  SummarizedExperiment::assays(x) <- assays

  x
}

#' Plot spectrogram
#'
#' Creates a visualization of a spectrogram result.
#'
#' @param spec Spectrogram result from spectrogram().
#' @param freq_range Optional frequency range to display.
#' @param log_power If TRUE, displays log power.
#' @return A ggplot object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000)),
#'   samplingRate = 250
#' )
#'
#' # Compute spectrogram
#' spec <- spectrogram(pe, channel = 1)
#'
#' # Plot with frequency range filter
#' plotSpectrogram(spec, freq_range = c(1, 50))
plotSpectrogram <- function(spec, freq_range = NULL, log_power = TRUE) {
  power <- spec$power
  freqs <- spec$frequencies
  times <- spec$times

  if (!is.null(freq_range)) {
    freq_idx <- which(freqs >= freq_range[1] & freqs <= freq_range[2])
    power <- power[freq_idx, ]
    freqs <- freqs[freq_idx]
  }

  if (log_power) {
    power <- 10 * log10(power + 1e-10)
  }

  # Create data frame for ggplot
  plot_df <- expand.grid(time = times, frequency = freqs)
  plot_df$power <- as.vector(t(power))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = frequency, fill = power)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(option = "plasma") +
    ggplot2::labs(x = "Time (s)", y = "Frequency (Hz)",
                  fill = if (log_power) "Power (dB)" else "Power",
                  title = "Spectrogram")

  p
}
