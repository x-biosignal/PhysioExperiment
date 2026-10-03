library(testthat)

# --- fPCA tests ---

test_that("fPCA computes principal components from matrix", {

  set.seed(123)
  data <- matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
  result <- fPCA(data, n_components = 3)

  expect_s3_class(result, "fpca_result")
  expect_equal(ncol(result$scores), 3)
  expect_equal(nrow(result$scores), 20)
  expect_equal(nrow(result$loadings), 100)
  expect_equal(ncol(result$loadings), 3)
  expect_true(all(result$variance_explained >= 0))
  expect_true(all(result$variance_explained <= 1))
  expect_equal(length(result$mean_function), 100)
  expect_equal(result$n_components, 3)
  expect_equal(result$n_time, 100)
  expect_equal(result$n_obs, 20)
})

test_that("fPCA cumulative variance is monotonically increasing", {
  set.seed(42)
  data <- matrix(rnorm(50 * 15), nrow = 50, ncol = 15)
  result <- fPCA(data, n_components = 5)

  cum_var <- result$cumulative_variance
  expect_true(all(diff(cum_var) >= 0))
  expect_true(max(cum_var) <= 1.0 + 1e-10)
})

test_that("fPCA limits n_components to valid range", {
  set.seed(1)
  data <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  # Request more components than possible (min(n_obs-1, n_time))
  result <- fPCA(data, n_components = 100)
  expect_equal(result$n_components, 4)  # min(100, 5-1, 50) = 4
})

test_that("fPCA with smoothing works", {
  set.seed(123)
  data <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)

  result_smooth <- fPCA(data, n_components = 3, smooth = TRUE, smooth_param = 5)
  expect_s3_class(result_smooth, "fpca_result")
  expect_equal(ncol(result_smooth$scores), 3)
})

test_that("fPCA rejects invalid input", {
  expect_error(fPCA("not_a_matrix"), "Input must be a PhysioExperiment or matrix")
  expect_error(fPCA(list(a = 1)), "Input must be a PhysioExperiment or matrix")
})

test_that("print.fpca_result works", {
  set.seed(123)
  data <- matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
  result <- fPCA(data, n_components = 3)

  output <- capture.output(print(result))
  expect_true(any(grepl("Functional PCA Result", output)))
  expect_true(any(grepl("Observations: 20", output)))
  expect_true(any(grepl("Time points: 100", output)))
  expect_true(any(grepl("Components retained: 3", output)))
})

# --- reconstructFPCA tests ---

test_that("reconstructFPCA returns correct dimensions", {
  set.seed(123)
  data <- matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
  fpca_result <- fPCA(data, n_components = 5)

  recon <- reconstructFPCA(fpca_result)
  expect_equal(nrow(recon), 100)
  expect_equal(ncol(recon), 20)
})

test_that("reconstructFPCA with fewer components returns correct dims", {
  set.seed(123)
  data <- matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
  fpca_result <- fPCA(data, n_components = 5)

  recon <- reconstructFPCA(fpca_result, n_components = 2)
  expect_equal(nrow(recon), 100)
  expect_equal(ncol(recon), 20)
})

test_that("reconstructFPCA with subset of observations", {
  set.seed(123)
  data <- matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
  fpca_result <- fPCA(data, n_components = 5)

  recon <- reconstructFPCA(fpca_result, observation = 1:5)
  expect_equal(nrow(recon), 100)
  expect_equal(ncol(recon), 5)
})

test_that("reconstructFPCA rejects non-fpca_result input", {
  expect_error(reconstructFPCA(list(a = 1)), "Input must be an fpca_result object")
})

# --- registerCurves tests ---

test_that("continuous registration returns correct structure", {
  set.seed(123)
  t <- seq(0, 100, length.out = 100)
  data <- sapply(1:10, function(i) {
    shift <- rnorm(1, 0, 5)
    sin(2 * pi * (t + shift) / 100) * 30 + rnorm(100, 0, 1)
  })

  result <- registerCurves(data, method = "continuous")

  expect_s3_class(result, "registration_result")
  expect_equal(dim(result$registered), c(100, 10))
  expect_equal(dim(result$warping), c(100, 10))
  expect_equal(length(result$template), 100)
  expect_equal(result$method, "continuous")
  expect_equal(result$n_time, 100)
  expect_equal(result$n_obs, 10)
})

test_that("landmark registration works with provided landmarks", {
  set.seed(123)
  data <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)
  landmarks <- matrix(c(10, 50, 90,
                         12, 48, 88,
                         11, 52, 92,
                         9, 49, 91,
                         13, 51, 89), nrow = 3, ncol = 5)

  result <- registerCurves(data, method = "landmark", landmarks = landmarks)

  expect_s3_class(result, "registration_result")
  expect_equal(dim(result$registered), c(100, 5))
  expect_equal(result$method, "landmark")
})

test_that("landmark registration requires landmarks argument", {
  data <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)
  expect_error(
    registerCurves(data, method = "landmark"),
    "Landmarks required"
  )
})

test_that("registerCurves rejects invalid input", {
  expect_error(registerCurves("not_valid"), "Input must be a PhysioExperiment or matrix")
})
