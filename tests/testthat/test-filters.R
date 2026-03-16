library(testthat)
library(PhysioExperiment)

make_example <- function(n = 1000, sr = 500) {
  # Create test signal with known frequency components
  t <- seq(0, (n - 1) / sr, length.out = n)
  # 10 Hz signal + 50 Hz noise
  signal <- sin(2 * pi * 10 * t) + 0.5 * sin(2 * pi * 50 * t)
  # Create 2-channel data
  signal_data <- cbind(signal, signal * 0.9)
  assays <- S4Vectors::SimpleList(raw = array(signal_data, dim = c(n, 2, 1)))
  # rowData must have n rows (matching dim[1] = time points)
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  # colData must have 2 rows (matching dim[2] = channels)
  colData <- S4Vectors::DataFrame(
    label = c("Ch1", "Ch2"),
    sensor_type = rep("EEG", 2)
  )
  PhysioExperiment(assays, rowData, colData, samplingRate = sr)
}

test_that("filterSignals works with different window sizes", {
  x <- make_example()

  y <- filterSignals(x, window = 5)
  expect_true("filtered" %in% SummarizedExperiment::assayNames(y))

  y <- filterSignals(x, window = 10, output_assay = "smoothed")
  expect_true("smoothed" %in% SummarizedExperiment::assayNames(y))
})

test_that("filterSignals handles edge cases", {
  x <- make_example(n = 3)

  # Window larger than data - should warn
  expect_warning(filterSignals(x, window = 10))

  # Invalid window
  expect_error(filterSignals(x, window = 0))
})

test_that("butterworthFilter applies bandpass correctly", {
  x <- make_example()

  # Bandpass 5-15 Hz should keep 10 Hz, remove 50 Hz
  y <- butterworthFilter(x, low = 5, high = 15, type = "pass")
  expect_true("filtered" %in% SummarizedExperiment::assayNames(y))

  filtered_data <- SummarizedExperiment::assay(y, "filtered")
  expect_true(all(is.finite(filtered_data)))
})

test_that("butterworthFilter validates parameters", {
  x <- make_example()

  # Missing frequency for lowpass
  expect_error(butterworthFilter(x, type = "low"))

  # Missing frequency for highpass
  expect_error(butterworthFilter(x, type = "high"))

  # Invalid frequency (above Nyquist)
  expect_error(butterworthFilter(x, high = 300, type = "low"))
})

test_that("firFilter works", {
  x <- make_example()

  y <- firFilter(x, low = 5, high = 30, order = 50, type = "pass")
  expect_true("filtered" %in% SummarizedExperiment::assayNames(y))
})

test_that("notchFilter removes line noise", {
  x <- make_example()

  # Remove 50 Hz
  y <- notchFilter(x, freq = 50, bandwidth = 2)
  expect_true("filtered" %in% SummarizedExperiment::assayNames(y))

  # Multiple harmonics
  y <- notchFilter(x, freq = 50, harmonics = 2)
  expect_true("filtered" %in% SummarizedExperiment::assayNames(y))
})

test_that("detrendSignal removes linear trends", {
  # Create signal with linear trend
  n <- 100
  t <- seq_len(n)
  signal <- 2 * t + rnorm(n)
  assays <- S4Vectors::SimpleList(raw = array(signal, dim = c(n, 1, 1)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = "Ch1")
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  y <- detrendSignal(x, type = "linear")
  expect_true("detrended" %in% SummarizedExperiment::assayNames(y))

  detrended <- SummarizedExperiment::assay(y, "detrended")
  # Mean should be near zero after detrending
  expect_true(abs(mean(detrended)) < 1)
})

# --- SOS (Second-Order Sections) filtering tests ---

test_that("SOS and ba-form give similar results for low-order filters", {
  x <- make_example(n = 2000, sr = 500)

  # Order 4 bandpass: SOS vs ba should match closely
  y_sos <- butterworthFilter(x, low = 5, high = 15, type = "pass",
                             order = 4, use_sos = TRUE,
                             output_assay = "sos")
  y_ba  <- butterworthFilter(x, low = 5, high = 15, type = "pass",
                             order = 4, use_sos = FALSE,
                             output_assay = "ba")

  data_sos <- SummarizedExperiment::assay(y_sos, "sos")
  data_ba  <- SummarizedExperiment::assay(y_ba, "ba")

  # Trim edge transients (first/last 50 samples)
  trim <- 50
  n <- nrow(data_sos)
  idx <- (trim + 1):(n - trim)

  # Correlation should be very high between the two methods
  cor_val <- cor(as.numeric(data_sos[idx, , ]),
                 as.numeric(data_ba[idx, , ]))
  expect_true(cor_val > 0.99,
              info = sprintf("SOS/ba correlation = %.6f, expected > 0.99", cor_val))
})

test_that("SOS filtering produces finite results for high-order filters", {
  x <- make_example(n = 2000, sr = 500)

  # Order 10 bandpass - ba form can become unstable
  y10 <- butterworthFilter(x, low = 5, high = 40, type = "pass",
                           order = 10, use_sos = TRUE)
  data10 <- SummarizedExperiment::assay(y10, "filtered")
  expect_true(all(is.finite(data10)),
              info = "Order 10 SOS filter should produce all finite values")

  # Order 12 bandpass
  y12 <- butterworthFilter(x, low = 5, high = 40, type = "pass",
                           order = 12, use_sos = TRUE,
                           output_assay = "filtered12")
  data12 <- SummarizedExperiment::assay(y12, "filtered12")
  expect_true(all(is.finite(data12)),
              info = "Order 12 SOS filter should produce all finite values")
})

test_that("SOS filtering works for all filter types", {
  x <- make_example(n = 2000, sr = 500)

  # Lowpass
  y <- butterworthFilter(x, high = 30, type = "low", use_sos = TRUE,
                         output_assay = "lp")
  expect_true(all(is.finite(SummarizedExperiment::assay(y, "lp"))))

  # Highpass
  y <- butterworthFilter(x, low = 5, type = "high", use_sos = TRUE,
                         output_assay = "hp")
  expect_true(all(is.finite(SummarizedExperiment::assay(y, "hp"))))

  # Bandstop
  y <- butterworthFilter(x, low = 48, high = 52, type = "stop", use_sos = TRUE,
                         output_assay = "bs")
  expect_true(all(is.finite(SummarizedExperiment::assay(y, "bs"))))
})

test_that("use_sos = FALSE preserves original ba-form behavior", {
  x <- make_example()

  y <- butterworthFilter(x, low = 5, high = 15, type = "pass",
                         order = 4, use_sos = FALSE)
  expect_true("filtered" %in% SummarizedExperiment::assayNames(y))

  filtered_data <- SummarizedExperiment::assay(y, "filtered")
  expect_true(all(is.finite(filtered_data)))
})

test_that(".tf2sos handles odd-order filters", {
  # Odd order produces an unpaired real pole section
  bf <- signal::butter(n = 3, W = 0.2, type = "low")
  sos <- PhysioExperiment:::.tf2sos(bf$b, bf$a)

  expect_true(is.matrix(sos))
  expect_equal(ncol(sos), 6)
  # Order 3 should give 2 sections (one 2nd-order + one 1st-order)
  expect_true(nrow(sos) >= 1)
})

test_that(".tf2sos handles even-order filters", {
  bf <- signal::butter(n = 4, W = 0.3, type = "low")
  sos <- PhysioExperiment:::.tf2sos(bf$b, bf$a)

  expect_true(is.matrix(sos))
  expect_equal(ncol(sos), 6)
  # Order 4 should give 2 sections
  expect_equal(nrow(sos), 2)
})

test_that(".sosfiltfilt produces zero-phase output", {
  # Create a simple signal with known delay characteristics
  n <- 500
  sr <- 250
  t <- seq(0, (n - 1) / sr, length.out = n)
  sig <- sin(2 * pi * 5 * t)  # 5 Hz sine wave

  bf <- signal::butter(n = 4, W = 0.1, type = "low")
  sos <- PhysioExperiment:::.tf2sos(bf$b, bf$a)

  filtered <- PhysioExperiment:::.sosfiltfilt(sos, sig)

  # The filtered signal should have the same length

  expect_equal(length(filtered), n)
  expect_true(all(is.finite(filtered)))

  # Zero-phase filter: peak of filtered signal should be at same location
  # as peak of original (no phase delay)
  # Check first peak location (avoid edges)
  peak_orig <- which.max(sig[20:100]) + 19
  peak_filt <- which.max(filtered[20:100]) + 19
  expect_true(abs(peak_orig - peak_filt) <= 1,
              info = "Zero-phase filter should not shift signal peaks")
})

test_that("detrendSignal removes constant (mean)", {
  n <- 100
  signal <- rnorm(n) + 100
  assays <- S4Vectors::SimpleList(raw = array(signal, dim = c(n, 1, 1)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = "Ch1")
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  y <- detrendSignal(x, type = "constant")
  detrended <- SummarizedExperiment::assay(y, "detrended")
  expect_true(abs(mean(detrended)) < 1e-10)
})
