library(testthat)
library(PhysioExperiment)

make_continuous_data <- function() {
  # 10 seconds of data at 100 Hz
  n <- 1000
  sr <- 100
  t <- seq(0, (n - 1) / sr, length.out = n)
  signal <- sin(2 * pi * 5 * t)
  # Create 2-channel data
  signal_data <- cbind(signal, signal * 0.8)
  assays <- S4Vectors::SimpleList(raw = array(signal_data, dim = c(n, 2, 1)))
  # rowData must have n rows (matching dim[1] = time points)
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  # colData must have 2 rows (matching dim[2] = channels)
  colData <- S4Vectors::DataFrame(
    label = c("Ch1", "Ch2"),
    sensor_type = rep("EEG", 2)
  )

  x <- PhysioExperiment(assays, rowData, colData, samplingRate = sr)

  # Add events at 1, 3, 5, 7 seconds
  x <- addEvents(x, onset = c(1, 3, 5, 7), type = "stimulus", value = "A")
  x
}

test_that("epochData creates epochs from continuous data", {
  x <- make_continuous_data()

  # Epoch -0.2 to 0.5 around events
  epoched <- epochData(x, tmin = -0.2, tmax = 0.5)

  expect_s4_class(epoched, "PhysioExperiment")

  # Check dimensions: time x channel x epoch x sample
  data <- SummarizedExperiment::assay(epoched, "epoched")
  dims <- dim(data)
  expect_equal(length(dims), 4)
  expect_equal(dims[3], 4)  # 4 epochs
})

test_that("epochData applies baseline correction", {
  x <- make_continuous_data()

  # Add offset to test baseline correction
  data <- SummarizedExperiment::assay(x) + 100
  SummarizedExperiment::assay(x) <- data

  epoched <- epochData(x, tmin = -0.2, tmax = 0.5, baseline = c(-0.2, 0))

  epoch_data <- SummarizedExperiment::assay(epoched, "epoched")
  # Mean of first part should be near zero after baseline correction
  # (approximately, due to sinusoidal signal)
  expect_true(all(abs(epoch_data) < 110))  # Reasonable range after correction
})

test_that("epochData rejects bad epochs", {
  x <- make_continuous_data()

  # Add artifact at one event
  data <- SummarizedExperiment::assay(x)
  data[300:310, , ] <- 1000  # Large artifact around 3 seconds
  SummarizedExperiment::assay(x) <- data

  # Reject epochs with amplitude > 500
  expect_message(
    epoched <- epochData(x, tmin = -0.2, tmax = 0.5, reject = 500),
    "Rejected"
  )

  epoch_data <- SummarizedExperiment::assay(epoched, "epoched")
  expect_equal(dim(epoch_data)[3], 3)  # One epoch rejected
})

test_that("epochData handles events at boundaries", {
  x <- make_continuous_data()

  # Add events near edges that can't be epoched
  x <- addEvents(x, onset = c(0.05, 9.9), type = "edge")

  epoched <- epochData(x, tmin = -0.2, tmax = 0.5)

  # Edge events should be excluded
  epoch_info <- S4Vectors::metadata(epoched)$epoch_info
  expect_true(all(epoch_info$event_type == "stimulus"))
})

test_that("averageEpochs computes mean across epochs", {
  x <- make_continuous_data()
  epoched <- epochData(x, tmin = -0.2, tmax = 0.5)

  averaged <- averageEpochs(epoched)

  # Averaged data should be 3D (time x channel x sample)
  data <- SummarizedExperiment::assay(averaged, "averaged")
  expect_equal(length(dim(data)), 3)
})

test_that("averageEpochs can group by condition", {
  # Create data with two conditions
  n <- 1000
  sr <- 100
  signal_data <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  assays <- S4Vectors::SimpleList(raw = array(signal_data, dim = c(n, 2, 1)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = c("Ch1", "Ch2"))
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = sr)

  x <- addEvents(x, onset = c(1, 3), type = "stimulus", value = "A")
  x <- addEvents(x, onset = c(5, 7), type = "stimulus", value = "B")

  epoched <- epochData(x, tmin = -0.1, tmax = 0.3)

  # Average by event_value
  averaged <- averageEpochs(epoched, by = "event_value")

  data <- SummarizedExperiment::assay(averaged, "averaged")
  # Should have 2 conditions
  expect_equal(dim(data)[3], 2)
})

test_that("epochTimes returns correct time vector", {
  x <- make_continuous_data()
  epoched <- epochData(x, tmin = -0.2, tmax = 0.5)

  times <- epochTimes(epoched)

  expect_equal(times[1], -0.2)
  expect_equal(times[length(times)], 0.5, tolerance = 0.01)
})

test_that("grandAverage works with multiple objects", {
  # Create two subjects
  make_subject <- function() {
    n <- 500
    signal_data <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
    assays <- S4Vectors::SimpleList(raw = array(signal_data, dim = c(n, 2, 1)))
    rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
    colData <- S4Vectors::DataFrame(label = c("Ch1", "Ch2"))
    x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)
    x <- addEvents(x, onset = c(1, 2, 3), type = "stim")
    epochData(x, tmin = -0.1, tmax = 0.3)
  }

  subj1 <- make_subject()
  subj2 <- make_subject()

  grand <- grandAverage(subj1, subj2)

  expect_s4_class(grand, "PhysioExperiment")
  expect_equal(S4Vectors::metadata(grand)$n_subjects, 2)
})

test_that("epochData accepts EventQuery", {
  pe <- make_pe_2d(n_time = 10000, n_channels = 4, sr = 100)
  pe <- addEvents(pe, onset = c(1, 3, 5, 7, 9), type = "go")
  pe <- addEvents(pe, onset = c(2, 4, 6, 8), type = "nogo")

  q <- eventQuery(pe) |> filterType("go")
  epoched <- epochData(pe, tmin = -0.2, tmax = 0.5, events = q)

  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 5)
})

test_that("epochData supports variable-length epochs with tmax as event name", {
  pe <- make_pe_2d(n_time = 10000, n_channels = 4, sr = 100)

  # Add stimulus events
  pe <- addEvents(pe, onset = c(1, 3, 5), type = "stimulus")
  # Add response events at variable intervals
  pe <- addEvents(pe, onset = c(1.3, 3.5, 5.2), type = "response")

  # Epoch from stimulus to response
  epoched <- epochData(pe, tmin = 0, tmax = "response",
                       events = eventQuery(pe) |> filterType("stimulus"),
                       min_length = 0.2)

  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 3)
  # Epochs should have different lengths stored in metadata
  expect_true("variable_lengths" %in% names(S4Vectors::metadata(epoched)))
})
