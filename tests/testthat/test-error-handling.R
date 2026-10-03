# Comprehensive error handling tests for PhysioExperiment
# This file tests that functions properly validate inputs and provide
# informative error messages.
# Note: Tests for functions in other packages (filtering, epoching, etc.)
# are in their respective packages (PhysioPreprocess, PhysioAnalysis, PhysioIO)

# ---------------------------------------------------------------------
# PhysioExperiment class errors
# ---------------------------------------------------------------------

test_that("PhysioExperiment rejects negative sampling rate", {
  expect_error(
    PhysioExperiment(
      assays = list(raw = matrix(1:4, nrow = 2)),
      samplingRate = -100
    ),
    "positive"
  )
})

test_that("PhysioExperiment rejects vector sampling rate", {
  expect_error(
    PhysioExperiment(
      assays = list(raw = matrix(1:4, nrow = 2)),
      samplingRate = c(100, 200)
    ),
    "scalar"
  )
})

# ---------------------------------------------------------------------
# Event handling errors
# ---------------------------------------------------------------------

test_that("PhysioEvents rejects mismatched lengths", {
  expect_error(
    PhysioEvents(
      onset = c(1, 2, 3),
      type = c("a", "b")  # Wrong length
    ),
    "length"
  )
})

test_that("getEvents requires PhysioExperiment", {
  expect_error(getEvents(list()), "PhysioExperiment")
})

test_that("setEvents requires PhysioExperiment", {
  expect_error(setEvents(list(), PhysioEvents()), "PhysioExperiment")
})

test_that("addEvents requires PhysioExperiment", {
  expect_error(addEvents(list(), onset = 1), "PhysioExperiment")
})

test_that("timeToSamples requires valid sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:4, nrow = 2)),
    samplingRate = NA_real_
  )
  expect_error(timeToSamples(pe, 1), "sampling rate")
})

test_that("samplesToTime requires valid sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:4, nrow = 2)),
    samplingRate = NA_real_
  )
  expect_error(samplesToTime(pe, 1), "sampling rate")
})

# ---------------------------------------------------------------------
# Channel handling errors
# ---------------------------------------------------------------------

test_that("pickChannels errors on missing channels", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("A", "B", "C", "D")),
    samplingRate = 100
  )

  expect_error(pickChannels(pe, c("A", "Z")), "not found")
})

test_that("pickChannels errors on out-of-range indices", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    samplingRate = 100
  )

  expect_error(pickChannels(pe, c(1, 10)), "not found|out of")
})

test_that("dropChannels cannot drop all channels", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    samplingRate = 100
  )

  expect_error(dropChannels(pe, 1:4), "Cannot drop all")
})

test_that("setChannelTypes rejects wrong length", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:4)),
    samplingRate = 100
  )

  expect_error(setChannelTypes(pe, c("EEG", "EMG")), "Length")
})

test_that("renameChannels rejects mismatched lengths", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:4)),
    samplingRate = 100
  )

  expect_error(
    renameChannels(pe, old_names = c("Ch1", "Ch2"), new_names = "NewCh"),
    "same length"
  )
})

test_that("setElectrodePositions requires x, y, z columns", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    samplingRate = 100
  )

  expect_error(
    setElectrodePositions(pe, data.frame(a = 1:4, b = 1:4)),
    "x, y, z"
  )
})

# ---------------------------------------------------------------------
# Methods errors
# ---------------------------------------------------------------------

test_that("extractWindow requires valid sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:100, nrow = 10)),
    samplingRate = NA_real_
  )

  expect_error(extractWindow(pe, tmin = 0, tmax = 1), "sampling rate")
})

test_that("duration returns NA without valid sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:100, nrow = 10)),
    samplingRate = NA_real_
  )

  expect_true(is.na(duration(pe)))
})

test_that("cbindPhysio requires matching sampling rates", {
  pe1 <- PhysioExperiment(
    assays = list(raw = matrix(1:10, nrow = 5)),
    samplingRate = 100
  )
  pe2 <- PhysioExperiment(
    assays = list(raw = matrix(1:10, nrow = 5)),
    samplingRate = 200
  )

  expect_error(cbindPhysio(pe1, pe2), "Sampling rates must match")
})

test_that("rbindPhysio requires matching channels", {
  pe1 <- PhysioExperiment(
    assays = list(raw = matrix(1:10, nrow = 5, ncol = 2)),
    samplingRate = 100
  )
  pe2 <- PhysioExperiment(
    assays = list(raw = matrix(1:15, nrow = 5, ncol = 3)),
    samplingRate = 100
  )

  expect_error(rbindPhysio(pe1, pe2), "channels must match")
})
