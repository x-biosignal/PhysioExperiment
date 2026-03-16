# tests/testthat/test-epoch-performance.R
library(testthat)
library(PhysioExperiment)

test_that("epochData handles large datasets efficiently", {
  skip_on_cran()

  # Create large dataset: 100000 samples, 64 channels
  n <- 100000
  n_channels <- 64
  sr <- 1000

  data <- matrix(rnorm(n * n_channels), nrow = n, ncol = n_channels)
  pe <- PhysioExperiment(
    assays = list(raw = data),
    samplingRate = sr
  )

  # Add 500 events
  event_times <- seq(1, 99, by = 0.2)
  pe <- addEvents(pe, onset = event_times, type = "stimulus")

  # Benchmark epoching
  time <- system.time({
    epoched <- epochData(pe, tmin = -0.2, tmax = 0.5)
  })

  # Should complete in under 10 seconds
  expect_lt(time["elapsed"], 10)

  # Verify correctness
  expect_s4_class(epoched, "PhysioExperiment")
  expect_equal(S4Vectors::metadata(epoched)$n_epochs, length(event_times))
})

test_that("baseline correction is vectorized", {
  n <- 10000
  n_channels <- 32
  sr <- 500

  data <- matrix(rnorm(n * n_channels), nrow = n, ncol = n_channels)
  pe <- PhysioExperiment(
    assays = list(raw = data),
    samplingRate = sr
  )
  pe <- addEvents(pe, onset = seq(1, 19, by = 0.5), type = "stim")

  time <- system.time({
    epoched <- epochData(pe, tmin = -0.2, tmax = 0.5, baseline = c(-0.2, 0))
  })

  # Baseline correction should be fast
  expect_lt(time["elapsed"], 5)
})
