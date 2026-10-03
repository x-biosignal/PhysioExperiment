test_that("wpliEstimate matches the analytic Vinck (2011) Eq. 6 formula", {
  set.seed(1)
  im <- rnorm(300)
  est <- wpliEstimate(im)
  raw <- abs(sum(im)) / sum(abs(im))
  deb <- (sum(im)^2 - sum(im^2)) / (sum(abs(im))^2 - sum(im^2))
  expect_equal(est$wpli, raw, tolerance = 1e-12)
  expect_equal(est$wpli_debiased, deb, tolerance = 1e-12)
  expect_equal(est$n, 300L)
})

test_that("wpliEstimate debiased of independent signals distributes around 0", {
  # unbiasedness: over 200 independent complex cross-spectra the debiased wPLI
  # mean is within 2 SE of 0 (acceptance criterion)
  set.seed(42)
  n_windows <- 40
  vals <- vapply(seq_len(200), function(k) {
    z1 <- rnorm(n_windows) + 1i * rnorm(n_windows)
    z2 <- rnorm(n_windows) + 1i * rnorm(n_windows)
    wpliEstimate(Im(z1 * Conj(z2)))$wpli_debiased
  }, numeric(1))
  m <- mean(vals)
  se <- stats::sd(vals) / sqrt(length(vals))
  expect_lt(abs(m), 2 * se)
  # not clamped: both signs occur
  expect_true(any(vals < 0) && any(vals > 0))
})

test_that("wpliEstimate raw wPLI is in [0, 1] and 1 for a constant-sign imag", {
  expect_gte(wpliEstimate(rnorm(100))$wpli, 0)
  expect_lte(wpliEstimate(rnorm(100))$wpli, 1)
  expect_equal(wpliEstimate(rep(0.3, 50))$wpli, 1)          # all same sign
})

test_that("wpliEstimate handles edge cases", {
  expect_true(is.na(wpliEstimate(rnorm(10), debiased = FALSE)$wpli_debiased))
  expect_true(is.na(wpliEstimate(0.5, debiased = TRUE)$wpli_debiased))  # n < 2
  expect_equal(wpliEstimate(rep(0, 20))$wpli, 0)           # degenerate (all zero)
})
