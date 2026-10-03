# Inferential circular statistics, verified against known circular structure.

test_that("circular mean wraps around 0/360 correctly", {
  s <- circularSummary(c(10, 20, 350, 355))
  expect_lt(min(abs(s$mean - c(0, 360))), 5)          # ~ 0, not ~ 184
  expect_gt(s$rbar, 0.9)                              # tightly clustered
  # uniform angles -> rbar ~ 0
  set.seed(1); u <- circularSummary(runif(2000, 0, 360))
  expect_lt(u$rbar, 0.1)
})

test_that("Rayleigh test rejects uniformity for clustered, not for uniform", {
  set.seed(2)
  clustered <- rayleighTest(rnorm(50, mean = 40, sd = 12))
  uniform   <- rayleighTest(runif(200, 0, 360))
  expect_lt(clustered$p.value, 0.001)
  expect_gt(uniform$p.value, 0.05)
  expect_equal(unname(clustered$estimate), 40, tolerance = 6)
})

test_that("Watson-Williams separates different mean directions", {
  set.seed(1)
  g1 <- rnorm(30, 20, 8); g2 <- rnorm(30, 70, 8); g3 <- rnorm(30, 20, 8)
  diff <- watsonWilliamsTest(list(g1, g2))
  same <- watsonWilliamsTest(list(g1, g3))
  expect_lt(diff$p.value, 0.001)
  expect_gt(same$p.value, 0.05)
  expect_equal(unname(diff$parameter["df1"]), 1)
  # vector + group interface agrees with list interface
  vt <- watsonWilliamsTest(c(g1, g2), group = rep(c("a", "b"), each = 30))
  expect_equal(vt$statistic, diff$statistic, tolerance = 1e-8)
})

test_that("Watson-Williams warns when concentration is low", {
  set.seed(4)
  expect_warning(watsonWilliamsTest(list(runif(40, 0, 360), runif(40, 0, 360))))
})

test_that("Mardia circular-linear correlation detects an angle-linked variable", {
  set.seed(5)
  th <- runif(80, 0, 360)
  y  <- 2 * cos(th * pi / 180) + rnorm(80, 0, 0.3)     # linear var driven by angle
  cl <- circularLinearCorrelation(th, y)
  expect_gt(unname(cl$estimate), 0.7)
  expect_lt(cl$p.value, 0.001)
  # unrelated linear variable -> low correlation, non-significant
  y0 <- rnorm(80)
  cl0 <- circularLinearCorrelation(th, y0)
  expect_lt(unname(cl0$estimate), 0.4)
  expect_gt(cl0$p.value, 0.05)
})

test_that("radians units path matches degrees", {
  ang_deg <- c(10, 20, 30, 40)
  a <- circularSummary(ang_deg, units = "degrees")
  b <- circularSummary(ang_deg * pi / 180, units = "radians")
  expect_equal(a$mean, b$mean * 180 / pi, tolerance = 1e-8)
  expect_equal(a$rbar, b$rbar, tolerance = 1e-12)
})
