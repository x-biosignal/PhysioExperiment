# Tests for topographic map visualization

test_that("plotTopomap works with electrode positions", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
    samplingRate = 100
  )

  # Apply montage to get positions

pe <- applyMontage(pe, "10-20")

  # Should work with default time point
  p <- plotTopomap(pe)
  expect_s3_class(p, "ggplot")
})

test_that("plotTopomap works with custom values", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
    samplingRate = 100
  )
  pe <- applyMontage(pe, "10-20")

  # Should work with custom values
  p <- plotTopomap(pe, values = c(1, 0.5, -0.5, -1))
  expect_s3_class(p, "ggplot")
})

test_that("plotTopomap works with specified time point", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
    samplingRate = 100
  )
  pe <- applyMontage(pe, "10-20")

  p <- plotTopomap(pe, time = 0.5)
  expect_s3_class(p, "ggplot")
})

test_that("plotTopomap errors without electrode positions", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
    samplingRate = 100
  )

  expect_error(plotTopomap(pe), "positions")
})

test_that("plotTopomap respects visual options", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
    samplingRate = 100
  )
  pe <- applyMontage(pe, "10-20")

  # Without contours
  p <- plotTopomap(pe, contours = FALSE)
  expect_s3_class(p, "ggplot")

  # Without head shape
  p <- plotTopomap(pe, head_shape = FALSE)
  expect_s3_class(p, "ggplot")

  # Without electrodes
  p <- plotTopomap(pe, electrodes = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plotTopomapSeries creates multiple plots", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
    samplingRate = 100
  )
  pe <- applyMontage(pe, "10-20")

  plots <- plotTopomapSeries(pe, times = c(0.1, 0.2, 0.3))

  expect_type(plots, "list")
  expect_length(plots, 3)
  expect_s3_class(plots[[1]], "ggplot")
})
