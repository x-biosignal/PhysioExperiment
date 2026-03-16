library(testthat)
library(PhysioExperiment)

# --- Function existence ---

test_that("fftSignals function exists", {
  expect_true(is.function(fftSignals))
})

# --- Input validation ---

test_that("fftSignals requires PhysioExperiment object", {
  expect_error(fftSignals(list()))
})

# --- Basic FFT computation ---

test_that("fftSignals creates an 'fft' assay", {
  pe <- make_pe_2d(n_time = 256, n_channels = 4, sr = 256)

  pe_fft <- fftSignals(pe)

  expect_s4_class(pe_fft, "PhysioExperiment")
  expect_true("fft" %in% SummarizedExperiment::assayNames(pe_fft))
})

test_that("fftSignals preserves dimensions", {
  pe <- make_pe_2d(n_time = 512, n_channels = 3, sr = 250)

  pe_fft <- fftSignals(pe)

  orig_dim <- dim(SummarizedExperiment::assay(pe, "raw"))
  fft_dim <- dim(SummarizedExperiment::assay(pe_fft, "fft"))

  expect_equal(orig_dim, fft_dim)
})

test_that("fftSignals output contains non-negative magnitude values", {
  pe <- make_pe_2d(n_time = 256, n_channels = 2, sr = 256)

  pe_fft <- fftSignals(pe)
  fft_data <- SummarizedExperiment::assay(pe_fft, "fft")

  # Magnitude should be non-negative
  expect_true(all(fft_data >= 0))
})

test_that("fftSignals preserves original assay", {
  pe <- make_pe_2d(n_time = 256, n_channels = 2, sr = 256)

  pe_fft <- fftSignals(pe)

  expect_true("raw" %in% SummarizedExperiment::assayNames(pe_fft))
  orig_data <- SummarizedExperiment::assay(pe, "raw")
  kept_data <- SummarizedExperiment::assay(pe_fft, "raw")
  expect_identical(orig_data, kept_data)
})

test_that("fftSignals preserves sampling rate", {
  pe <- make_pe_2d(n_time = 256, n_channels = 2, sr = 500)

  pe_fft <- fftSignals(pe)

  expect_equal(samplingRate(pe_fft), 500)
})

# --- FFT correctness ---

test_that("fftSignals detects a known frequency peak", {
  # Create a pure 10 Hz sine wave at 256 Hz sampling rate
  sr <- 256
  n <- 256
  t <- seq(0, (n - 1) / sr, length.out = n)
  signal <- sin(2 * pi * 10 * t)

  pe <- PhysioExperiment(
    assays = list(raw = matrix(signal, ncol = 1)),
    colData = S4Vectors::DataFrame(label = "Ch1", type = "EEG"),
    samplingRate = sr
  )

  pe_fft <- fftSignals(pe)
  fft_data <- SummarizedExperiment::assay(pe_fft, "fft")

  # The 10 Hz component should be the dominant frequency

  # Frequency bin for 10 Hz = 10 * n / sr + 1 = 11
  freq_bin_10hz <- 10 * n / sr + 1
  peak_bin <- which.max(fft_data[1:floor(n / 2), 1])

  expect_equal(peak_bin, freq_bin_10hz)
})

test_that("fftSignals works with 3D data", {
  pe <- make_pe_3d(n_time = 64, n_channels = 2, n_samples = 3, sr = 128)

  pe_fft <- fftSignals(pe)

  expect_true("fft" %in% SummarizedExperiment::assayNames(pe_fft))

  fft_data <- SummarizedExperiment::assay(pe_fft, "fft")
  expect_equal(length(dim(fft_data)), 3)
  expect_true(all(fft_data >= 0))
})

test_that("fftSignals output is finite", {
  pe <- make_pe_2d(n_time = 128, n_channels = 4, sr = 128)

  pe_fft <- fftSignals(pe)
  fft_data <- SummarizedExperiment::assay(pe_fft, "fft")

  expect_true(all(is.finite(fft_data)))
})

# --- Edge cases ---

test_that("fftSignals works with single channel", {
  pe <- make_pe_2d(n_time = 100, n_channels = 1, sr = 100)

  pe_fft <- fftSignals(pe)

  expect_true("fft" %in% SummarizedExperiment::assayNames(pe_fft))
  expect_equal(ncol(SummarizedExperiment::assay(pe_fft, "fft")), 1)
})

test_that("fftSignals works with small data", {
  pe <- make_pe_2d(n_time = 4, n_channels = 2, sr = 100)

  pe_fft <- fftSignals(pe)

  expect_true("fft" %in% SummarizedExperiment::assayNames(pe_fft))
  fft_data <- SummarizedExperiment::assay(pe_fft, "fft")
  expect_equal(nrow(fft_data), 4)
})
