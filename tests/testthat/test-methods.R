library(testthat)
library(PhysioExperiment)

make_test_obj <- function() {
  # Array is (time x channels x samples) = (100 x 5 x 2)
  assays <- S4Vectors::SimpleList(raw = array(rnorm(1000), dim = c(100, 5, 2)))
  # rowData must have 100 rows (matching dim[1] = time points)
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(100))
  # colData must have 5 rows (matching dim[2] = channels)
  colData <- S4Vectors::DataFrame(label = paste0("Ch", 1:5))
  PhysioExperiment(assays, rowData, colData, samplingRate = 100)
}

test_that("show method works", {
  x <- make_test_obj()
  expect_output(show(x), "PhysioExperiment")
  expect_output(show(x), "samplingRate")
})

test_that("length returns time points", {
  x <- make_test_obj()
  expect_equal(length(x), 100)
})

test_that("dim returns dimensions", {
  x <- make_test_obj()
  expect_equal(dim(x), c(100, 5, 2))
})

test_that("subsetting by time works", {
  x <- make_test_obj()
  y <- x[1:50, ]

  expect_equal(length(y), 50)
  expect_equal(nChannels(y), 5)
})

test_that("subsetting by channels works", {
  x <- make_test_obj()
  y <- x[, 1:3]

  expect_equal(length(y), 100)
  expect_equal(nChannels(y), 3)
})

test_that("subsetting by both dimensions works", {
  x <- make_test_obj()
  y <- x[1:50, 1:3]

  expect_equal(length(y), 50)
  expect_equal(nChannels(y), 3)
})

test_that("extractWindow extracts correct time range", {
  x <- make_test_obj()  # 100 samples at 100 Hz = 1 second
  y <- extractWindow(x, tmin = 0.2, tmax = 0.5)

  # Should be ~30 samples
  expect_true(abs(length(y) - 30) <= 1)
})

test_that("duration returns correct value", {
  x <- make_test_obj()  # 100 samples at 100 Hz
  expect_equal(duration(x), 1)
})

test_that("summary returns statistics", {
  x <- make_test_obj()
  stats <- summary(x)

  expect_s3_class(stats, "data.frame")
  expect_equal(nrow(stats), 5)  # 5 channels
  expect_true(all(c("min", "max", "mean", "sd") %in% names(stats)))
})

test_that("as.data.frame converts correctly", {
  x <- make_test_obj()
  df <- as.data.frame(x)

  expect_s3_class(df, "data.frame")
  expect_true("time" %in% names(df))
  expect_equal(nrow(df), 100)
})

test_that("cbindPhysio combines channels", {
  x <- make_test_obj()
  y <- pickChannels(x, 1:2)
  z <- pickChannels(x, 3:4)

  combined <- cbindPhysio(y, z)
  expect_equal(nChannels(combined), 4)
})

test_that("cbindPhysio checks sampling rate", {
  x <- make_test_obj()
  y <- make_test_obj()
  samplingRate(y) <- 200

  expect_error(cbindPhysio(x, y), "Sampling rates")
})

test_that("rbindPhysio concatenates time", {
  x <- make_test_obj()
  y <- make_test_obj()

  combined <- rbindPhysio(x, y)
  expect_equal(length(combined), 200)
  expect_equal(duration(combined), 2)
})

test_that("rbindPhysio checks channel count", {
  x <- make_test_obj()
  y <- pickChannels(make_test_obj(), 1:3)

  expect_error(rbindPhysio(x, y), "channels")
})
