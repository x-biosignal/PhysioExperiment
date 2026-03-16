library(testthat)
library(PhysioExperiment)

# Basic filtering tests
test_that("filterSignals adds a filtered assay", {
  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)
  pe_filtered <- filterSignals(pe, window = 5)

  expect_true("filtered" %in% SummarizedExperiment::assayNames(pe_filtered))
  expect_s4_class(pe_filtered, "PhysioExperiment")
})

test_that("fftSignals computes FFT", {
  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)
  pe_fft <- fftSignals(pe)

  expect_true("fft" %in% SummarizedExperiment::assayNames(pe_fft))
})

# Advanced filtering tests
test_that("butterworthFilter applies bandpass filter", {
  skip_if_not_installed("signal")
  pe <- make_pe_2d(n_time = 500, n_channels = 2, sr = 250)

  pe_bp <- butterworthFilter(pe, low = 1, high = 40, order = 2)
  expect_s4_class(pe_bp, "PhysioExperiment")
  expect_true("filtered" %in% SummarizedExperiment::assayNames(pe_bp))
})

test_that("notchFilter removes line noise", {
  skip_if_not_installed("signal")
  pe <- make_pe_2d(n_time = 500, n_channels = 2, sr = 250)

  # Use bandwidth parameter instead of q
  pe_notch <- notchFilter(pe, freq = 50, bandwidth = 2)
  expect_s4_class(pe_notch, "PhysioExperiment")
})

# Re-referencing tests
test_that("rereference computes average reference", {
  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 100)
  # Use ref_type instead of method
  pe_ref <- rereference(pe, ref_type = "average")

  expect_s4_class(pe_ref, "PhysioExperiment")
  expect_true("rereferenced" %in% SummarizedExperiment::assayNames(pe_ref))
})

# NA handling tests - handleNA operates on vectors/matrices
test_that("checkNA detects missing values in PhysioExperiment", {
  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  # Introduce NA
  data <- SummarizedExperiment::assay(pe, "raw")
  data[1, 1] <- NA
  SummarizedExperiment::assays(pe)$raw <- data

  result <- checkNA(pe)
  expect_true(result$has_na)
})

test_that("handleNA replaces missing values in vectors", {
  # handleNA works on vectors/matrices directly
  x <- c(1, NA, 3, 4, 5)
  result <- handleNA(x, method = "interpolate")

  expect_false(any(is.na(result)))
})

# Resampling tests
test_that("decimate reduces sampling rate", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 2, sr = 500)
  pe_dec <- decimate(pe, factor = 2)

  expect_equal(samplingRate(pe_dec), 250)
})

# Time-frequency tests
test_that("spectrogram computes time-frequency representation", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 2, sr = 250)
  # Use window_size instead of window
  spec <- spectrogram(pe, window_size = 64, channel = 1)

  expect_true(is.list(spec))
  expect_true("power" %in% names(spec))
})

test_that("bandPower computes frequency band power", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 2, sr = 250)

  # bandPower uses bands list, not low/high
  power <- bandPower(pe, bands = list(alpha = c(8, 12)))
  expect_true(is.list(power) || is.data.frame(power))
})
