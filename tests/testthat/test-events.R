library(testthat)
library(PhysioExperiment)

test_that("PhysioEvents can be created", {
  events <- PhysioEvents(
    onset = c(1.0, 2.5, 4.0),
    duration = c(0.5, 0.5, 0.5),
    type = c("stim", "resp", "stim"),
    value = c("A", "B", "A")
  )

  expect_s4_class(events, "PhysioEvents")
  expect_equal(nEvents(events), 3)
})

test_that("PhysioEvents validates input lengths", {
  expect_error(
    PhysioEvents(onset = 1:3, duration = 1:2),
    "length"
  )
})

test_that("setEvents and getEvents work", {
  n <- 100
  n_ch <- 5
  n_samples <- 2
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n * n_ch * n_samples), dim = c(n, n_ch, n_samples)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = paste0("Ch", seq_len(n_ch)))
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  events <- PhysioEvents(
    onset = c(0.1, 0.5, 0.9),
    type = "stimulus",
    value = c("A", "B", "C")
  )

  x <- setEvents(x, events)
  retrieved <- getEvents(x)

  expect_equal(nEvents(retrieved), 3)
})

test_that("addEvents appends to existing", {
  n <- 100
  n_ch <- 5
  n_samples <- 2
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n * n_ch * n_samples), dim = c(n, n_ch, n_samples)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = paste0("Ch", seq_len(n_ch)))
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  x <- addEvents(x, onset = c(0.1, 0.2), type = "stim")
  x <- addEvents(x, onset = c(0.5), type = "resp")

  expect_equal(nEvents(x), 3)
})

test_that("getEvents filters by type", {
  n <- 100
  n_ch <- 5
  n_samples <- 2
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n * n_ch * n_samples), dim = c(n, n_ch, n_samples)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = paste0("Ch", seq_len(n_ch)))
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  x <- addEvents(x, onset = c(0.1, 0.2), type = "stim")
  x <- addEvents(x, onset = c(0.5), type = "resp")

  stim_events <- getEvents(x, type = "stim")
  expect_equal(nEvents(stim_events), 2)

  resp_events <- getEvents(x, type = "resp")
  expect_equal(nEvents(resp_events), 1)
})

test_that("removeEvents removes by type", {
  n <- 100
  n_ch <- 5
  n_samples <- 2
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n * n_ch * n_samples), dim = c(n, n_ch, n_samples)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = paste0("Ch", seq_len(n_ch)))
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  x <- addEvents(x, onset = c(0.1, 0.2), type = "stim")
  x <- addEvents(x, onset = c(0.5), type = "resp")

  x <- removeEvents(x, type = "resp")
  expect_equal(nEvents(x), 2)
})

test_that("timeToSamples and samplesToTime convert correctly", {
  n <- 1000
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n), dim = c(n, 1, 1)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = "Ch1")
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  # 0.5 seconds at 100 Hz = sample 51 (1-indexed)
  samples <- timeToSamples(x, 0.5)
  expect_equal(samples, 51L)

  # Round trip
  times <- samplesToTime(x, samples)
  expect_equal(times, 0.5)
})

test_that("setEvents accepts data.frame", {
  n <- 100
  n_ch <- 5
  n_samples <- 2
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n * n_ch * n_samples), dim = c(n, n_ch, n_samples)))
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  colData <- S4Vectors::DataFrame(label = paste0("Ch", seq_len(n_ch)))
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 100)

  events_df <- data.frame(
    onset = c(0.1, 0.5),
    duration = c(0.1, 0.1),
    type = c("a", "b"),
    value = c("x", "y")
  )

  x <- setEvents(x, events_df)
  expect_equal(nEvents(x), 2)
})
