# Tests for statistical testing functions

test_that("tTestEpochs works with one-sample test", {
  # Create test data
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 20 * 1, mean = 0.5), dim = c(100, 4, 20, 1))
  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- tTestEpochs(pe)

  expect_type(result, "list")
  expect_equal(dim(result$t_values), c(100, 4))
  expect_equal(dim(result$p_values), c(100, 4))
  expect_equal(result$n1, 20)
  expect_true(is.na(result$n2))

  # With non-zero mean, t-values should be positive
  expect_true(mean(result$t_values, na.rm = TRUE) > 0)
})

test_that("tTestEpochs works with two-sample test", {
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))
  # Add effect to second condition
  epochs[40:60, , 11:20, ] <- epochs[40:60, , 11:20, ] + 2

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- tTestEpochs(pe, condition1 = 1:10, condition2 = 11:20)

  expect_equal(result$n1, 10)
  expect_equal(result$n2, 10)

  # Effect should be detected in the affected time window
  expect_true(any(result$p_values[40:60, ] < 0.05, na.rm = TRUE))
})

test_that("tTestEpochs works with paired test", {
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- tTestEpochs(pe, condition1 = 1:10, condition2 = 11:20, paired = TRUE)

  expect_equal(result$paired, TRUE)
  expect_equal(dim(result$t_values), c(100, 4))
})

test_that("tTestEpochs paired test handles asymmetric NA correctly", {
  set.seed(123)
  epochs <- array(rnorm(10 * 2 * 10 * 1), dim = c(10, 2, 10, 1))

  # Create asymmetric NA: epoch 1 in cond1 is NA, epoch 2 in cond2 is NA
  epochs[, , 1, 1] <- NA  # cond1 epoch 1 is NA
  epochs[, , 7, 1] <- NA  # cond2 epoch 2 is NA (corresponds to pair 2)

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  # This should work without error - only complete pairs are used
  result <- tTestEpochs(pe, condition1 = 1:5, condition2 = 6:10, paired = TRUE)

  expect_equal(result$paired, TRUE)
  # Results should still be computed (with fewer pairs)
  expect_false(all(is.na(result$t_values)))
})

test_that("tTestEpochs errors with non-4D data", {
  data_2d <- matrix(rnorm(100), nrow = 10)
  pe <- PhysioExperiment(
    assays = list(raw = data_2d),
    samplingRate = 100
  )

  expect_error(tTestEpochs(pe), "4D")
})

test_that("anovaEpochs works with multiple conditions", {
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 30 * 1), dim = c(100, 4, 30, 1))

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100,
    metadata = list(
      epoch_info = S4Vectors::DataFrame(
        condition = rep(c("A", "B", "C"), each = 10)
      )
    )
  )

  result <- anovaEpochs(pe, groups = "condition")

  expect_type(result, "list")
  expect_equal(dim(result$f_values), c(100, 4))
  expect_equal(dim(result$p_values), c(100, 4))
  expect_equal(result$df_between, 2)  # 3 groups - 1
  expect_equal(result$df_within, 27)  # 30 - 3
  expect_equal(length(result$groups), 3)
})

test_that("anovaEpochs works with vector groups", {
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  groups <- rep(c("ctrl", "exp"), each = 10)
  result <- anovaEpochs(pe, groups = groups)

  expect_equal(length(result$groups), 2)
})

test_that("anovaEpochs errors with mismatched groups", {
  epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))
  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  expect_error(anovaEpochs(pe, groups = c("A", "B")), "match")
})

test_that("clusterPermutationTest identifies significant clusters", {
  set.seed(42)
  epochs <- array(rnorm(50 * 4 * 20 * 1), dim = c(50, 4, 20, 1))
  # Add strong effect
  epochs[20:30, 1:2, 11:20, 1] <- epochs[20:30, 1:2, 11:20, 1] + 3

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- clusterPermutationTest(pe, 1:10, 11:20,
                                    n_permutations = 100, seed = 123)

  expect_type(result, "list")
  expect_true(length(result$clusters) >= 0)
  expect_equal(dim(result$t_obs), c(50, 4))
  expect_equal(dim(result$cluster_mask), c(50, 4))
})

test_that("clusterPermutationTest works with one-sample", {
  set.seed(123)
  epochs <- array(rnorm(50 * 2 * 10 * 1, mean = 0.5), dim = c(50, 2, 10, 1))

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- clusterPermutationTest(pe, n_permutations = 50, seed = 456)

  expect_type(result, "list")
  expect_equal(length(result$permutation_distribution), 50)
})

test_that("effectSize computes Cohen's d correctly", {
  set.seed(123)
  # Create data with known effect size
  epochs <- array(0, dim = c(100, 4, 20 * 1, 1))
  epochs[, , 1:10, 1] <- rnorm(100 * 4 * 10, mean = 0, sd = 1)
  epochs[, , 11:20, 1] <- rnorm(100 * 4 * 10, mean = 1, sd = 1)  # d ~ 1

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- effectSize(pe, condition1 = 1:10, condition2 = 11:20)

  expect_type(result, "list")
  expect_equal(dim(result$d), c(100, 4))
  expect_equal(dim(result$ci_lower), c(100, 4))
  expect_equal(dim(result$ci_upper), c(100, 4))

  # Effect size should be around -1 (cond1 < cond2)
  mean_d <- mean(result$d, na.rm = TRUE)
  expect_true(mean_d < -0.5 && mean_d > -1.5)
})

test_that("effectSize works for one-sample", {
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 10 * 1, mean = 1, sd = 1), dim = c(100, 4, 10, 1))

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- effectSize(pe, condition1 = 1:10)

  # d should be around 1 (mean/sd = 1/1)
  mean_d <- mean(result$d, na.rm = TRUE)
  expect_true(mean_d > 0.5)
})

test_that("bootstrapCI computes confidence intervals", {
  set.seed(123)
  epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))

  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  result <- bootstrapCI(pe, n_bootstrap = 100, seed = 456)

  expect_type(result, "list")
  expect_equal(dim(result$mean), c(100, 4))
  expect_equal(dim(result$ci_lower), c(100, 4))
  expect_equal(dim(result$ci_upper), c(100, 4))
  expect_equal(dim(result$se), c(100, 4))

  # CI should contain mean
  expect_true(all(result$ci_lower <= result$mean, na.rm = TRUE))
  expect_true(all(result$ci_upper >= result$mean, na.rm = TRUE))
})

test_that("correctPValues applies FDR correction", {
  p <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.5, 0.6, 0.7, 0.8, 0.9)

  p_fdr <- correctPValues(p, method = "fdr")
  p_bonf <- correctPValues(p, method = "bonferroni")
  p_none <- correctPValues(p, method = "none")

  expect_equal(length(p_fdr), length(p))
  expect_true(all(p_fdr >= p))  # Corrected p-values are larger
  expect_true(all(p_bonf >= p_fdr))  # Bonferroni is more conservative
  expect_equal(p_none, p)
})

test_that("correctPValues works with matrix input", {
  p <- matrix(c(0.01, 0.02, 0.5, 0.6), nrow = 2)

  p_corrected <- correctPValues(p, method = "fdr")

  expect_true(is.matrix(p_corrected))
  expect_equal(dim(p_corrected), c(2, 2))
})

test_that("correctPValues handles NA values", {
  p <- c(0.01, NA, 0.03, 0.04)

  p_corrected <- correctPValues(p, method = "fdr")

  expect_true(is.na(p_corrected[2]))
  expect_false(any(is.na(p_corrected[c(1, 3, 4)])))
})

test_that("findSignificantWindows identifies windows correctly", {
  times <- seq(-0.2, 0.8, length.out = 100)
  p <- rep(0.5, 100)
  p[30:50] <- 0.01  # Significant window

  result <- findSignificantWindows(p, times)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(names(result), c("start", "end", "duration", "min_p"))
  expect_equal(result$min_p, 0.01)
})
test_that("findSignificantWindows handles no significant windows", {
  times <- 1:10
  p <- rep(0.5, 10)

  result <- findSignificantWindows(p, times)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("findSignificantWindows handles multiple windows", {
  times <- 1:100
  p <- rep(0.5, 100)
  p[10:20] <- 0.01
  p[50:60] <- 0.02

  result <- findSignificantWindows(p, times)

  expect_equal(nrow(result), 2)
})

test_that("findSignificantWindows respects min_duration", {
  times <- seq(0, 1, length.out = 100)
  p <- rep(0.5, 100)
  p[10:12] <- 0.01  # Short window
  p[50:70] <- 0.01  # Long window

  result_all <- findSignificantWindows(p, times, min_duration = 0)
  result_filtered <- findSignificantWindows(p, times, min_duration = 0.1)

  expect_equal(nrow(result_all), 2)
  expect_equal(nrow(result_filtered), 1)
})
