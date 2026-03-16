library(testthat)
library(PhysioExperiment)

test_that("PhysioExperiment constructor works with 2D matrix", {
  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 250)

  expect_s4_class(pe, "PhysioExperiment")
  expect_equal(samplingRate(pe), 250)
  expect_equal(nChannels(pe), 4)
})

test_that("PhysioExperiment timeIndex works correctly", {
  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 100)

  ti <- timeIndex(pe)
  expect_true(is.numeric(ti))
  expect_equal(length(ti), 100)
  expect_equal(ti[1], 0)
  expect_equal(ti[2], 0.01, tolerance = 1e-10)
})

test_that("samplingRate setter works", {
  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 250)
  samplingRate(pe) <- 500

  expect_equal(samplingRate(pe), 500)
})

test_that("defaultAssay returns first assay name", {
  pe <- make_pe_2d()

  expect_equal(defaultAssay(pe), "raw")
})

test_that("PhysioExperiment subsetting works", {
  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 250)

  # Subset channels
  pe_sub <- pe[, 1:2]
  expect_s4_class(pe_sub, "PhysioExperiment")
  expect_equal(nChannels(pe_sub), 2)
  expect_equal(samplingRate(pe_sub), 250)
})

test_that("duration calculates correctly", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)

  d <- duration(pe)
  expect_equal(d, 4, tolerance = 0.01)  # 1000/250 = 4 seconds
})

test_that("PhysioExperiment handles empty assays", {
  pe <- PhysioExperiment(samplingRate = 100)
  expect_s4_class(pe, "PhysioExperiment")
  expect_equal(samplingRate(pe), 100)
})
