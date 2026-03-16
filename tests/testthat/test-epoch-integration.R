# tests/testthat/test-epoch-integration.R
library(testthat)
library(PhysioExperiment)

test_that("full epoch workflow with EventQuery", {
  # Create realistic data
  pe <- make_pe_2d(n_time = 60000, n_channels = 32, sr = 1000)

  # Add mixed events
  pe <- addEvents(pe, onset = seq(1, 59, by = 2), type = "go", value = "correct")
  pe <- addEvents(pe, onset = seq(2, 58, by = 4), type = "go", value = "incorrect")
  pe <- addEvents(pe, onset = seq(1.5, 59, by = 3), type = "nogo")

  # Complex query
  q <- eventQuery(pe) |>
    filterType("go") |>
    filterValue("correct")

  # Epoch with baseline
  epoched <- epochData(pe, tmin = -0.2, tmax = 0.8,
                       events = q, baseline = c(-0.2, 0))

  expect_s4_class(epoched, "PhysioExperiment")
  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 30)

  # Verify dimensions
  data <- SummarizedExperiment::assay(epoched, "epoched")
  expect_equal(length(dim(data)), 4)  # time x channel x epoch x sample
  expect_equal(dim(data)[2], 32)      # 32 channels
  expect_equal(dim(data)[3], 30)      # 30 epochs

  # Average epochs
  averaged <- averageEpochs(epoched)
  expect_s4_class(averaged, "PhysioExperiment")
})

test_that("sliding window followed by averaging", {
  pe <- make_pe_2d(n_time = 10000, n_channels = 8, sr = 500)

  # Create sliding windows
  epoched <- epochSliding(pe, window = 1.0, step = 0.5)

  expect_s4_class(epoched, "PhysioExperiment")
  expect_true(S4Vectors::metadata(epoched)$n_epochs > 0)

  # Average all windows
  averaged <- averageEpochs(epoched)
  expect_s4_class(averaged, "PhysioExperiment")
})

test_that("variable-length epochs work end-to-end", {
  pe <- make_pe_2d(n_time = 10000, n_channels = 4, sr = 100)

  # Add stimulus-response pairs
  pe <- addEvents(pe, onset = c(1, 5, 10, 15, 20), type = "stimulus")
  pe <- addEvents(pe, onset = c(1.5, 5.8, 10.3, 15.9, 20.4), type = "response")

  # Epoch from stimulus to response
  epoched <- epochData(pe, tmin = 0, tmax = "response",
                       events = eventQuery(pe) |> filterType("stimulus"),
                       min_length = 0.2)

  expect_s4_class(epoched, "PhysioExperiment")
  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 5)
  expect_true("variable_lengths" %in% names(S4Vectors::metadata(epoched)))

  # Variable lengths should match our input
  var_lengths <- S4Vectors::metadata(epoched)$variable_lengths
  expect_equal(length(var_lengths), 5)
  expect_true(all(var_lengths >= 0.2))  # All above min_length
})

test_that("chained EventQuery filters work correctly", {
  pe <- make_pe_2d(n_time = 10000, n_channels = 4, sr = 100)

  # Complex event structure
  pe <- addEvents(pe, onset = c(1, 5), type = "target", value = "left")
  pe <- addEvents(pe, onset = c(2, 6), type = "target", value = "right")
  pe <- addEvents(pe, onset = c(3, 7), type = "distractor", value = "left")
  pe <- addEvents(pe, onset = c(4, 8), type = "distractor", value = "right")

  # Chain multiple filters
  q <- eventQuery(pe) |>
    filterType("target") |>
    filterValue("left")

  events <- resolveQuery(q)
  expect_equal(nrow(events), 2)
  expect_true(all(events$type == "target"))
  expect_true(all(events$value == "left"))

  # Use in epoching
  epoched <- epochData(pe, tmin = -0.1, tmax = 0.5, events = q)
  expect_equal(S4Vectors::metadata(epoched)$n_epochs, 2)
})
