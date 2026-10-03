# A known 3-node VAR(2) generator.
.sim_var2 <- function(n, seed) {
  set.seed(seed)
  A1 <- matrix(c(0.4, 0, 0, 0.3, 0.5, 0, 0, 0.2, 0.4), 3, 3, byrow = TRUE)
  A2 <- matrix(c(-0.2, 0, 0, 0, -0.2, 0, 0.1, 0, -0.2), 3, 3, byrow = TRUE)
  Lc <- chol(diag(c(1, 0.8, 1.2)))
  X <- matrix(0, n, 3)
  for (t in 3:n) {
    X[t, ] <- A1 %*% X[t - 1, ] + A2 %*% X[t - 2, ] +
      as.numeric(t(Lc) %*% rnorm(3))
  }
  list(X = X, A1 = A1, A2 = A2)
}

test_that(".fitMVAR recovers a known VAR(2) within 5% for all methods", {
  sim <- .sim_var2(10000, seed = 1)
  tol <- 0.05 * max(abs(c(sim$A1, sim$A2)))
  for (mth in c("ols", "yulewalker", "nuttall-strand")) {
    fit <- .fitMVAR(sim$X, 2, method = mth)
    expect_equal(dim(fit$A), c(3L, 3L, 2L))
    err <- max(abs(fit$A[, , 1] - sim$A1), abs(fit$A[, , 2] - sim$A2))
    expect_lt(err, tol)
    # residual covariance is symmetric positive definite
    expect_true(all(eigen(fit$Sigma, symmetric = TRUE, only.values = TRUE)$values > 0))
  }
})

test_that(".mvarSpectral: H = inv(A) and S = H Sigma H* to < 1e-8", {
  sim <- .sim_var2(8000, seed = 2)
  fit <- .fitMVAR(sim$X, 2)
  freqs <- seq(1, 50, by = 1)
  sp <- .mvarSpectral(fit$A, fit$Sigma, freqs, sr = 200)
  expect_equal(dim(sp$H), c(3L, 3L, length(freqs)))

  err <- 0
  for (fi in seq_along(freqs)) {
    err <- max(err, max(Mod(sp$H[, , fi] %*% sp$A[, , fi] - diag(3))))
    Srec <- sp$H[, , fi] %*% fit$Sigma %*% Conj(t(sp$H[, , fi]))
    err <- max(err, max(Mod(Srec - sp$S[, , fi])))
  }
  expect_lt(err, 1e-8)
})

test_that("the spectral density is Hermitian at each frequency", {
  sim <- .sim_var2(4000, seed = 3)
  fit <- .fitMVAR(sim$X, 2)
  sp <- .mvarSpectral(fit$A, fit$Sigma, freqs = c(10, 20, 30), sr = 200)
  for (fi in 1:3) {
    expect_lt(max(Mod(sp$S[, , fi] - Conj(t(sp$S[, , fi])))), 1e-8)
  }
})

test_that("mvarOrderSelect picks the true order (2) for >= 90% of seeded runs", {
  n_runs <- 20
  hits <- vapply(seq_len(n_runs), function(s) {
    sim <- .sim_var2(2500, seed = 300 + s)
    mvarOrderSelect(sim$X, max_order = 6, criterion = "bic")$order
  }, numeric(1))
  expect_gte(mean(hits == 2), 0.9)
})

test_that("mvarFit auto-selects the order and mvarTransfer returns spectra", {
  sim <- .sim_var2(4000, seed = 4)
  fit <- mvarFit(sim$X)
  expect_s3_class(fit, "mvar")
  expect_equal(fit$order, 2L)
  expect_equal(fit$n_channels, 3L)

  fit2 <- mvarFit(sim$X, order = 3, method = "yulewalker")
  expect_equal(fit2$order, 3L)

  tr <- mvarTransfer(fit, freqs = seq(1, 40, by = 1), sr = 100)
  expect_equal(dim(tr$H), c(3L, 3L, 40L))
  expect_named(tr, c("H", "S", "A", "frequencies"))
})

test_that(".fitMVAR validates inputs", {
  expect_error(.fitMVAR(matrix(rnorm(30), ncol = 3), order = 20),
               "Too few samples")
  expect_error(.fitMVAR(matrix(rnorm(300), ncol = 3), order = 2,
                        method = "bogus"))
})
