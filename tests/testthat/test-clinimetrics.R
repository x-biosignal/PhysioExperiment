test_that("SEM/MDC relationship: mdc = qnorm(0.975) * sqrt(2) * sem (~1.96 * sqrt(2))", {
  set.seed(1)
  x <- rnorm(50, 10, 2)
  s <- sem(x, reliability = 0.9)
  expect_true(is.numeric(s) && s > 0)
  # SEM = SD * sqrt(1 - reliability)
  expect_equal(s, stats::sd(x) * sqrt(1 - 0.9), tolerance = 1e-9)
  # MDC = z * sqrt(2) * SEM
  expect_equal(mdc(s), stats::qnorm(0.975) * sqrt(2) * s, tolerance = 1e-12)
  expect_equal(mdc(s) / s, 1.96 * sqrt(2), tolerance = 1e-2)
})

test_that("icc returns a value in [-1, 1] for a two-way model", {
  ratings <- matrix(c(9, 2, 5, 8,
                      6, 1, 3, 6,
                      8, 4, 6, 7), ncol = 3, byrow = FALSE)
  r <- icc(ratings, model = "twoway")
  expect_true(is.list(r) && is.numeric(r$icc))
  expect_true(r$icc >= -1 && r$icc <= 1)
})

test_that("blandAltman bias sits between the limits of agreement", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1.1, 1.9, 3.2, 3.8, 5.1)
  ba <- blandAltman(x, y)
  expect_equal(ba$bias, mean(x - y), tolerance = 1e-9)
  expect_lt(ba$lower_loa, ba$bias)
  expect_gt(ba$upper_loa, ba$bias)
})

test_that("cohensD reports value and interpretation", {
  set.seed(3)
  d <- cohensD(rnorm(50, mean = 2), rnorm(50, mean = 0))
  expect_true(is.numeric(d$d) && d$d > 0)
  expect_true(d$interpretation %in% c("negligible", "small", "medium", "large"))
})

test_that("etaSquared is in [0, 1]", {
  set.seed(4)
  g <- factor(rep(letters[1:3], each = 20))
  y <- rnorm(60) + as.numeric(g)
  e <- etaSquared(y, g)
  val <- if (is.list(e)) e[[grep("eta", names(e), ignore.case = TRUE)[1]]] else e
  expect_true(is.numeric(val) && val >= 0 && val <= 1)
})
