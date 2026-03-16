library(testthat)
library(PhysioExperiment)

make_test_data <- function(sr = 1000, duration = 1) {
  n <- as.integer(sr * duration)
  t <- seq(0, duration - 1/sr, length.out = n)
  # 10 Hz sine wave
  signal <- sin(2 * pi * 10 * t)
  assays <- S4Vectors::SimpleList(raw = array(signal, dim = c(n, 2, 1)))
  # rowData must have n rows (matching dim[1] = time points)
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  # colData must have 2 rows (matching dim[2] = channels)
  colData <- S4Vectors::DataFrame(label = c("Ch1", "Ch2"))
  PhysioExperiment(assays, rowData = rowData, colData = colData, samplingRate = sr)
}

test_that("resample changes sampling rate correctly", {
  x <- make_test_data(sr = 1000, duration = 1)

  # Downsample
  y <- resample(x, target_rate = 250, output_assay = "raw")
  expect_equal(samplingRate(y), 250)
  expect_equal(dim(SummarizedExperiment::assay(y))[1], 250)

  # Upsample
  z <- resample(x, target_rate = 2000, output_assay = "raw")
  expect_equal(samplingRate(z), 2000)
  expect_equal(dim(SummarizedExperiment::assay(z))[1], 2000)
})

test_that("resample preserves signal characteristics", {
  x <- make_test_data(sr = 1000, duration = 1)
  y <- resample(x, target_rate = 500, method = "spline", output_assay = "raw")

  # Check that signal range is similar
  orig_range <- range(SummarizedExperiment::assay(x))
  new_range <- range(SummarizedExperiment::assay(y))
  expect_true(abs(orig_range[1] - new_range[1]) < 0.1)
  expect_true(abs(orig_range[2] - new_range[2]) < 0.1)
})

test_that("resample handles same rate", {
  x <- make_test_data(sr = 1000)
  expect_message(resample(x, target_rate = 1000), "no resampling")
})

test_that("resample validates inputs", {
  x <- make_test_data()
  expect_error(resample(x, target_rate = -100))
})

test_that("decimate reduces sampling rate by factor", {
  x <- make_test_data(sr = 1000, duration = 1)
  y <- decimate(x, factor = 4)

  expect_equal(samplingRate(y), 250)
  expect_equal(dim(SummarizedExperiment::assay(y))[1], 250)
})

test_that("interpolate increases sampling rate by factor", {
  x <- make_test_data(sr = 100, duration = 1)
  y <- interpolate(x, factor = 2)

  expect_equal(samplingRate(y), 200)
  expect_equal(dim(SummarizedExperiment::assay(y))[1], 200)
})

test_that("assaySamplingRates returns rates for all assays", {
  x <- make_test_data(sr = 500)
  rates <- assaySamplingRates(x)

  expect_equal(rates[["raw"]], 500)
})

test_that("setAssaySamplingRate sets rate for specific assay", {
  x <- make_test_data(sr = 500)
  x <- setAssaySamplingRate(x, "raw", 1000)

  rates <- assaySamplingRates(x)
  expect_equal(rates[["raw"]], 1000)
})
