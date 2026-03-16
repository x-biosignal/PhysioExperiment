library(testthat)
library(PhysioExperiment)

test_that("correlationMatrix computes channel correlations", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)

  result <- correlationMatrix(pe)

  # correlationMatrix returns a list
  expect_true(is.list(result))
  expect_true("correlation" %in% names(result))

  cor_mat <- result$correlation
  expect_true(is.matrix(cor_mat))
  expect_equal(nrow(cor_mat), 4)
  expect_equal(ncol(cor_mat), 4)
  # Diagonal should be 1
  expect_equal(unname(diag(cor_mat)), rep(1, 4), tolerance = 0.001)
})

test_that("nodeDegree computes network degrees", {
  # Create adjacency matrix
  adj <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), nrow = 4, byrow = TRUE)

  degrees <- nodeDegree(adj)
  expect_true(is.numeric(degrees))
  expect_equal(length(degrees), 4)
})

test_that("clusteringCoefficient computes coefficients", {
  # Create adjacency matrix
  adj <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), nrow = 4, byrow = TRUE)

  cc <- clusteringCoefficient(adj)
  expect_true(is.numeric(cc))
  expect_equal(length(cc), 4)
})

test_that("globalEfficiency computes network efficiency", {
  adj <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), nrow = 4, byrow = TRUE)

  eff <- globalEfficiency(adj)
  expect_true(is.numeric(eff))
  expect_true(eff >= 0 && eff <= 1)
})

test_that("thresholdNetwork thresholds adjacency matrix", {
  adj <- matrix(c(0, 0.8, 0.3, 0.1,
                  0.8, 0, 0.6, 0.2,
                  0.3, 0.6, 0, 0.9,
                  0.1, 0.2, 0.9, 0), nrow = 4, byrow = TRUE)

  # thresholdNetwork uses density parameter (proportion of edges to keep)
  thresh_adj <- thresholdNetwork(adj, density = 0.3)
  expect_true(is.matrix(thresh_adj))
  expect_equal(nrow(thresh_adj), 4)
})
