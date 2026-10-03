library(testthat)
library(PhysioExperiment)

test_that("PhysioExperiment basic structure works", {
  n <- 100
  n_ch <- 5
  n_samples <- 2
  assays <- S4Vectors::SimpleList(raw = array(rnorm(n * n_ch * n_samples), dim = c(n, n_ch, n_samples)))
  # rowData must have n rows (matching dim[1] = time points)
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(n))
  # colData must have n_ch rows (matching dim[2] = channels)
  colData <- S4Vectors::DataFrame(
    label = paste0("Ch", seq_len(n_ch)),
    sensor_type = rep("EMG", n_ch)
  )
  x <- PhysioExperiment(assays, rowData, colData, samplingRate = 1000)

  expect_s4_class(x, "PhysioExperiment")
  expect_equal(samplingRate(x), 1000)
  expect_true(is.numeric(timeIndex(x)))
})
