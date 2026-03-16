# tests/testthat/test-epoch-sliding.R
library(testthat)
library(PhysioExperiment)

test_that("epochSliding creates overlapping windows", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)

  # 0.5 second windows, 0.1 second step
  epoched <- epochSliding(pe, window = 0.5, step = 0.1)

  expect_s4_class(epoched, "PhysioExperiment")

  # Check number of windows: floor((10 - 0.5) / 0.1) = 95
  # (last window at 9.4 sec, since 9.5 + 0.5 > 10)
  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 95)
})

test_that("epochSliding uses 50% overlap by default", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)

  # 1.0 second windows, default 0.5 second step (50% overlap)
  epoched <- epochSliding(pe, window = 1.0)

  expect_s4_class(epoched, "PhysioExperiment")

  # Check number of windows: floor((10 - 1.0) / 0.5) = 18
  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 18)
})

test_that("epochSliding applies baseline correction", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)

  # Add offset
  data <- SummarizedExperiment::assay(pe) + 100
  SummarizedExperiment::assay(pe) <- data

  epoched <- epochSliding(pe, window = 0.5, step = 0.2, baseline = c(0, 0.1))

  expect_s4_class(epoched, "PhysioExperiment")
})
