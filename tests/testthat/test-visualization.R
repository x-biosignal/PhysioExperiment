library(testthat)
library(PhysioExperiment)

test_that("plotSignal returns ggplot", {
  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  p <- plotSignal(pe, channel = 1)
  expect_s3_class(p, "ggplot")
})

test_that("plotMultiChannel returns ggplot", {
  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 100)

  p <- plotMultiChannel(pe)
  expect_s3_class(p, "ggplot")
})

test_that("plotPSD returns ggplot", {
  pe <- make_pe_2d(n_time = 500, n_channels = 2, sr = 250)

  p <- plotPSD(pe, channel = 1)
  expect_s3_class(p, "ggplot")
})
