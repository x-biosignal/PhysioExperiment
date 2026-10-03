# Tests for NA handling utilities

test_that("checkNA detects NA values", {
  data_with_na <- matrix(c(1, NA, 3, 4), nrow = 2)
  pe <- PhysioExperiment(
    assays = list(raw = data_with_na),
    samplingRate = 100
  )

  expect_warning(result <- checkNA(pe), "NA values")
  expect_true(result$has_na)
  expect_equal(result$n_na, 1)
  expect_equal(result$pct_na, 25)
})

test_that("checkNA works with no NA", {
  data_clean <- matrix(1:4, nrow = 2)
  pe <- PhysioExperiment(
    assays = list(raw = data_clean),
    samplingRate = 100
  )

  result <- checkNA(pe, action = "none")
  expect_false(result$has_na)
  expect_equal(result$n_na, 0)
})

test_that("checkNA errors when action is error", {
  data_with_na <- matrix(c(1, NA, 3, 4), nrow = 2)
  pe <- PhysioExperiment(
    assays = list(raw = data_with_na),
    samplingRate = 100
  )

  expect_error(checkNA(pe, action = "error"), "NA values")
})

test_that("handleNA interpolates correctly", {
  x <- c(1, NA, 3, NA, 5)

  result <- handleNA(x, method = "interpolate")

  expect_equal(result[1], 1)
  expect_equal(result[2], 2)  # Interpolated
  expect_equal(result[3], 3)
  expect_equal(result[4], 4)  # Interpolated
  expect_equal(result[5], 5)
})

test_that("handleNA replaces with mean", {
  x <- c(1, NA, 3, NA, 5)

  result <- handleNA(x, method = "mean")

  expected_mean <- mean(c(1, 3, 5))
  expect_equal(result[2], expected_mean)
  expect_equal(result[4], expected_mean)
})

test_that("handleNA replaces with zero", {
  x <- c(1, NA, 3)

  result <- handleNA(x, method = "zero")

  expect_equal(result, c(1, 0, 3))
})

test_that("handleNA does LOCF", {
  x <- c(1, NA, NA, 4)

  result <- handleNA(x, method = "locf")

  expect_equal(result, c(1, 1, 1, 4))
})

test_that("handleNA omits NA", {
  x <- c(1, NA, 3, NA, 5)

  result <- handleNA(x, method = "omit")

  expect_equal(result, c(1, 3, 5))
})

test_that("handleNA works with matrix", {
  m <- matrix(c(1, NA, 3, 4, NA, 6), nrow = 3)

  result <- handleNA(m, method = "interpolate")

  expect_false(any(is.na(result)))
  expect_equal(dim(result), dim(m))
})

test_that("fillEdgeNA extends values", {
  x <- c(NA, NA, 1, 2, 3, NA, NA)

  result <- fillEdgeNA(x, method = "extend")

  expect_equal(result[1], 1)
  expect_equal(result[2], 1)
  expect_equal(result[6], 3)
  expect_equal(result[7], 3)
})

test_that("fillEdgeNA fills with zero", {
  x <- c(NA, 1, 2, NA)

  result <- fillEdgeNA(x, method = "zero")

  expect_equal(result[1], 0)
  expect_equal(result[4], 0)
})

test_that("hasNA returns correct logical", {
  pe_clean <- PhysioExperiment(
    assays = list(raw = matrix(1:4, nrow = 2)),
    samplingRate = 100
  )

  pe_na <- PhysioExperiment(
    assays = list(raw = matrix(c(1, NA, 3, 4), nrow = 2)),
    samplingRate = 100
  )

  expect_false(hasNA(pe_clean))
  expect_true(hasNA(pe_na))
})

test_that("naSummary returns correct summary", {
  pe <- PhysioExperiment(
    assays = list(
      raw = matrix(c(1, NA, 3, 4), nrow = 2),
      filtered = matrix(1:4, nrow = 2)
    ),
    samplingRate = 100
  )

  result <- naSummary(pe)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$n_na[result$assay == "raw"], 1)
  expect_equal(result$n_na[result$assay == "filtered"], 0)
})

test_that("replaceNA creates new assay", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(c(1, NA, 3, NA, 5, 6), nrow = 3)),
    samplingRate = 100
  )

  pe_handled <- replaceNA(pe, method = "interpolate")

  expect_true("na_handled" %in% SummarizedExperiment::assayNames(pe_handled))
  expect_false(any(is.na(SummarizedExperiment::assay(pe_handled, "na_handled"))))
})

test_that("replaceNA preserves original assay", {
  original <- matrix(c(1, NA, 3, 4), nrow = 2)
  pe <- PhysioExperiment(
    assays = list(raw = original),
    samplingRate = 100
  )

  pe_handled <- replaceNA(pe, method = "mean")

  # Original should be unchanged
  expect_true(any(is.na(SummarizedExperiment::assay(pe_handled, "raw"))))
})

test_that("replaceNA rejects omit method", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(c(1, NA, 3, 4), nrow = 2)),
    samplingRate = 100
  )

  # "omit" is not allowed because it changes array dimensions
  expect_error(replaceNA(pe, method = "omit"), "not supported")
})

test_that("replaceNA rejects invalid methods", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(c(1, NA, 3, 4), nrow = 2)),
    samplingRate = 100
  )

  expect_error(replaceNA(pe, method = "invalid_method"), "not supported")
})

test_that("handleNA returns unchanged if no NA", {
  x <- c(1, 2, 3, 4)

  result <- handleNA(x, method = "interpolate")

  expect_equal(result, x)
})

test_that("handleNA with none method returns input", {
  x <- c(1, NA, 3)

  result <- handleNA(x, method = "none")

  expect_equal(result, x)
})
