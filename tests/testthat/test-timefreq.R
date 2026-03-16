library(testthat)
library(PhysioExperiment)

make_test_signal <- function(sr = 500, duration = 2) {
  n <- as.integer(sr * duration)
  t <- seq(0, duration - 1/sr, length.out = n)

  # Create signal with known frequency components
  # 10 Hz and 30 Hz sine waves
  signal <- sin(2 * pi * 10 * t) + 0.5 * sin(2 * pi * 30 * t)

  data <- matrix(signal, ncol = 1)
  # rowData must have n rows (matching dim[1] = time points)
  row_data <- S4Vectors::DataFrame(time_idx = seq_len(n))
  # colData must have 1 row (matching dim[2] = channels)
  col_data <- S4Vectors::DataFrame(label = "Ch1", type = "EEG")

  # Convert to 3D array (time x channels x samples)
  assays <- S4Vectors::SimpleList(raw = array(data, dim = c(n, 1, 1)))
  PhysioExperiment(assays, rowData = row_data, colData = col_data, samplingRate = sr)
}

test_that("spectrogram computes STFT", {
  x <- make_test_signal(sr = 500, duration = 2)

  spec <- spectrogram(x, window_size = 256, overlap = 0.5)

  expect_type(spec, "list")
  expect_true("power" %in% names(spec))
  expect_true("frequencies" %in% names(spec))
  expect_true("times" %in% names(spec))

  # Check dimensions
  expect_equal(nrow(spec$power), floor(256 / 2) + 1)
  expect_true(length(spec$times) > 0)
  expect_equal(length(spec$frequencies), nrow(spec$power))
})

test_that("spectrogram identifies frequency peaks", {
  x <- make_test_signal(sr = 500, duration = 2)

  spec <- spectrogram(x, window_size = 256, overlap = 0.5)

  # Find frequency with maximum average power
  avg_power <- rowMeans(spec$power)

  # 10 Hz should have significant power
  idx_10hz <- which.min(abs(spec$frequencies - 10))
  idx_30hz <- which.min(abs(spec$frequencies - 30))

  # Power at 10 Hz should be higher than at 50 Hz (no signal there)
  idx_50hz <- which.min(abs(spec$frequencies - 50))
  expect_true(avg_power[idx_10hz] > avg_power[idx_50hz])
})

test_that("spectrogram window types work", {
  x <- make_test_signal()

  for (wtype in c("hanning", "hamming", "blackman", "rectangular")) {
    spec <- spectrogram(x, window_size = 128, window_type = wtype)
    expect_type(spec, "list")
    expect_true(all(!is.na(spec$power)))
  }
})

test_that("waveletTransform computes CWT", {
  x <- make_test_signal(sr = 500, duration = 2)

  wt <- waveletTransform(x, frequencies = seq(5, 40, by = 5), n_cycles = 5)

  expect_type(wt, "list")
  expect_true("power" %in% names(wt))
  expect_true("phase" %in% names(wt))
  expect_true("frequencies" %in% names(wt))
  expect_true("times" %in% names(wt))

  # Check dimensions
  expect_equal(nrow(wt$power), length(seq(5, 40, by = 5)))
  expect_equal(ncol(wt$power), 1000)  # sr * duration
})

test_that("waveletTransform identifies frequency components", {
  x <- make_test_signal(sr = 500, duration = 2)

  wt <- waveletTransform(x, frequencies = seq(5, 50, by = 5))

  # Average power at each frequency
  avg_power <- rowMeans(wt$power)

  # 10 Hz should have peak power
  idx_10hz <- which(wt$frequencies == 10)
  idx_30hz <- which(wt$frequencies == 30)
  idx_45hz <- which(wt$frequencies == 45)

  expect_true(avg_power[idx_10hz] > avg_power[idx_45hz])
})

test_that("bandPower extracts power in frequency bands", {
  x <- make_test_signal(sr = 500, duration = 2)

  bp <- bandPower(x, method = "welch")

  expect_s3_class(bp, "data.frame")
  expect_true("channel" %in% names(bp))
  expect_true("alpha" %in% names(bp))  # 8-13 Hz
  expect_true("beta" %in% names(bp))   # 13-30 Hz

  # Our 10 Hz signal should show up in alpha band
  expect_true(bp$alpha[1] > 0)
})

test_that("bandPower with custom bands", {
  x <- make_test_signal(sr = 500, duration = 2)

  custom_bands <- list(
    low = c(5, 15),
    high = c(25, 35)
  )

  bp <- bandPower(x, bands = custom_bands, method = "welch")

  expect_true("low" %in% names(bp))
  expect_true("high" %in% names(bp))

  # 10 Hz signal in "low" band, 30 Hz in "high" band
  expect_true(bp$low[1] > 0)
  expect_true(bp$high[1] > 0)
})

test_that("bandPower relative option works", {
  x <- make_test_signal()

  bp_abs <- bandPower(x, relative = FALSE)
  bp_rel <- bandPower(x, relative = TRUE)

  # Relative power should sum to 1
  total_rel <- sum(bp_rel[1, -1])  # Exclude channel column
  expect_equal(total_rel, 1, tolerance = 0.01)
})

test_that("hilbertTransform computes analytic signal", {
  x <- make_test_signal()

  y <- hilbertTransform(x, output_assay = "analytic")

  # Check that analytic assay was created
  expect_true("analytic" %in% SummarizedExperiment::assayNames(y))

  analytic <- SummarizedExperiment::assay(y, "analytic")
  expect_true(is.complex(analytic))
})

test_that("instantaneousAmplitude extracts envelope", {
  x <- make_test_signal()

  x <- hilbertTransform(x, output_assay = "analytic")
  y <- instantaneousAmplitude(x, assay_name = "analytic", output_assay = "amplitude")

  expect_true("amplitude" %in% SummarizedExperiment::assayNames(y))

  amp <- SummarizedExperiment::assay(y, "amplitude")
  expect_true(is.numeric(amp))
  expect_true(all(amp >= 0))  # Amplitude should be non-negative
})

test_that("instantaneousPhase extracts phase", {
  x <- make_test_signal()

  x <- hilbertTransform(x, output_assay = "analytic")
  y <- instantaneousPhase(x, assay_name = "analytic", output_assay = "phase")

  expect_true("phase" %in% SummarizedExperiment::assayNames(y))

  phase <- SummarizedExperiment::assay(y, "phase")
  expect_true(is.numeric(phase))
  # Phase should be in [-pi, pi]
  expect_true(all(phase >= -pi & phase <= pi))
})

test_that("spectrogram handles missing sampling rate",
  {
  x <- make_test_signal()
  # Use NA_real_ instead of NA (logical) to match numeric slot type
  samplingRate(x) <- NA_real_

  expect_error(spectrogram(x), "sampling rate")
})

test_that("waveletTransform handles 3D data", {
  # Create 3D data (samples x channels x epochs)
  x <- make_test_signal(sr = 250, duration = 1)
  data <- SummarizedExperiment::assay(x)
  data3d <- array(data, dim = c(nrow(data), 1, 2))
  SummarizedExperiment::assay(x, "raw") <- data3d

  wt <- waveletTransform(x, frequencies = c(5, 10, 20))

  expect_type(wt, "list")
  expect_equal(nrow(wt$power), 3)  # 3 frequencies
})

test_that("waveletTransform L2 normalization produces different power than L1", {
  x <- make_test_signal(sr = 500, duration = 2)

  wt_l1 <- waveletTransform(x, frequencies = seq(5, 40, by = 5),
                             n_cycles = 7, normalization = "L1")
  wt_l2 <- waveletTransform(x, frequencies = seq(5, 40, by = 5),
                             n_cycles = 7, normalization = "L2")

  # Both should produce valid output
  expect_equal(dim(wt_l1$power), dim(wt_l2$power))
  expect_equal(wt_l1$frequencies, wt_l2$frequencies)

  # Power values should differ between L1 and L2 normalization
  expect_false(isTRUE(all.equal(wt_l1$power, wt_l2$power)))

  # Both should still identify frequency peaks correctly
  avg_l1 <- rowMeans(wt_l1$power)
  avg_l2 <- rowMeans(wt_l2$power)
  idx_10hz <- which(wt_l1$frequencies == 10)
  idx_40hz <- which(wt_l1$frequencies == 40)
  expect_true(avg_l1[idx_10hz] > avg_l1[idx_40hz])
  expect_true(avg_l2[idx_10hz] > avg_l2[idx_40hz])
})

test_that("spectrogram returns proper PSD values", {
  # Create a pure 10 Hz sine wave with known amplitude
  sr <- 256
  duration <- 4
  n <- as.integer(sr * duration)
  t <- seq(0, duration - 1/sr, length.out = n)
  amplitude <- 1.0
  signal <- amplitude * sin(2 * pi * 10 * t)

  data <- matrix(signal, ncol = 1)
  row_data <- S4Vectors::DataFrame(time_idx = seq_len(n))
  col_data <- S4Vectors::DataFrame(label = "Ch1", type = "EEG")
  assays <- S4Vectors::SimpleList(raw = array(data, dim = c(n, 1, 1)))
  x <- PhysioExperiment(assays, rowData = row_data, colData = col_data,
                         samplingRate = sr)

  spec <- spectrogram(x, window_size = 256, overlap = 0.5, channel = 1)

  # Power matrix should be non-negative
  expect_true(all(spec$power >= 0))

  # Integrating PSD over frequency should give finite power
  df <- spec$frequencies[2] - spec$frequencies[1]
  total_power <- sum(rowMeans(spec$power) * df)
  expect_true(is.finite(total_power))
  expect_true(total_power > 0)

  # Peak should be at 10 Hz
  avg_power <- rowMeans(spec$power)
  peak_freq <- spec$frequencies[which.max(avg_power)]
  expect_equal(peak_freq, 10, tolerance = 2)
})

test_that("unified Welch PSD gives consistent results between one-sided and two-sided", {
  set.seed(42)
  sr <- 256
  n <- 1024
  signal <- rnorm(n)

  # One-sided (default)
  psd_one <- PhysioExperiment:::.welchPSD(signal, sr, nperseg = 256,
                                           one_sided = TRUE)
  # Two-sided
  psd_two <- PhysioExperiment:::.welchPSD(signal, sr, nperseg = 256,
                                           one_sided = FALSE)

  # Same frequencies

  expect_equal(psd_one$frequencies, psd_two$frequencies)

  # DC and Nyquist should be the same
  n_freqs <- length(psd_one$power)
  expect_equal(psd_one$power[1], psd_two$power[1])
  expect_equal(psd_one$power[n_freqs], psd_two$power[n_freqs])

  # Interior bins: one-sided should be exactly 2x two-sided
  expect_equal(psd_one$power[2:(n_freqs - 1)],
               2 * psd_two$power[2:(n_freqs - 1)])

  # .welchPSD2 wrapper should match two-sided .welchPSD
  psd2_result <- PhysioExperiment:::.welchPSD2(signal, nperseg = 256,
                                                noverlap = 128, sr = sr)
  expect_equal(psd2_result$psd, psd_two$power)
  expect_equal(psd2_result$frequencies, psd_two$frequencies)
})
