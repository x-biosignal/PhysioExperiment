# Tests for re-referencing functions

test_that("rereference with average reference works", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
    colData = S4Vectors::DataFrame(
      label = c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2"),
      type = rep("EEG", 10)
    ),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "average")

  # Check output assay exists
  expect_true("rereferenced" %in% SummarizedExperiment::assayNames(pe_ref))

  # Check metadata is updated
  expect_equal(getCurrentReference(pe_ref), "average")
  expect_true(isAverageReferenced(pe_ref))
})

test_that("rereference with single channel works", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
    colData = S4Vectors::DataFrame(
      label = c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2")
    ),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "channel", ref_channels = "C3")

  expect_true("rereferenced" %in% SummarizedExperiment::assayNames(pe_ref))
  expect_equal(getCurrentReference(pe_ref), "C3")

  # Reference channel should be all zeros
  reref_data <- SummarizedExperiment::assay(pe_ref, "rereferenced")
  c3_idx <- 5  # C3 is 5th channel
  expect_true(all(reref_data[, c3_idx] == 0))
})

test_that("rereference with multiple channels works", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
    colData = S4Vectors::DataFrame(
      label = c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2")
    ),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "channels", ref_channels = c("C3", "C4"))

  expect_true("rereferenced" %in% SummarizedExperiment::assayNames(pe_ref))
  expect_equal(getCurrentReference(pe_ref), "C3+C4")
})

test_that("rereference by channel index works", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "channel", ref_channels = 1)

  expect_true("rereferenced" %in% SummarizedExperiment::assayNames(pe_ref))
})

test_that("rereference errors with invalid channel", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10, ncol = 10)),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:10)),
    samplingRate = 256
  )

  expect_error(
    rereference(pe, ref_type = "channel", ref_channels = "NonExistent"),
    "not found"
  )
})

test_that("rereference with exclude works", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
    colData = S4Vectors::DataFrame(
      label = c("Fp1", "Fp2", "F3", "F4", "EOG1", "EOG2", "P3", "P4", "O1", "O2"),
      type = c(rep("EEG", 4), "EOG", "EOG", rep("EEG", 4))
    ),
    samplingRate = 256
  )

  # Exclude EOG channels from average reference calculation
  pe_ref <- rereference(pe, ref_type = "average", exclude = c("EOG1", "EOG2"))

  expect_true("rereferenced" %in% SummarizedExperiment::assayNames(pe_ref))
})

test_that("rereference with keep_ref=FALSE removes reference channels", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
    colData = S4Vectors::DataFrame(
      label = c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2")
    ),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "channel", ref_channels = "C3", keep_ref = FALSE)

  reref_data <- SummarizedExperiment::assay(pe_ref, "rereferenced")
  expect_equal(ncol(reref_data), 9)  # One channel removed
})

test_that("rereference works with 3D data", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(raw = array(rnorm(1000), dim = c(100, 10, 1))),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "average")

  reref_data <- SummarizedExperiment::assay(pe_ref, "rereferenced")
  expect_equal(dim(reref_data), c(100, 10, 1))
})

test_that("rereference works with 4D epoched data", {
  set.seed(123)
  pe <- PhysioExperiment(
    assays = list(epoched = array(rnorm(4000), dim = c(100, 10, 4, 1))),
    samplingRate = 256
  )

  pe_ref <- rereference(pe, ref_type = "average")

  reref_data <- SummarizedExperiment::assay(pe_ref, "rereferenced")
  expect_equal(dim(reref_data), c(100, 10, 4, 1))
})

test_that("getCurrentReference returns NULL when not set", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10, ncol = 10)),
    samplingRate = 100
  )

  expect_null(getCurrentReference(pe))
})

test_that("isAverageReferenced works correctly", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10, ncol = 10)),
    samplingRate = 100
  )

  expect_false(isAverageReferenced(pe))

  pe <- rereference(pe, ref_type = "average")
  expect_true(isAverageReferenced(pe))

  pe2 <- rereference(pe, ref_type = "channel", ref_channels = 1)
  expect_false(isAverageReferenced(pe2))
})
