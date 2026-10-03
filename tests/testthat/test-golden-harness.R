test_that("golden harness round-trips bit-stably and enforces tolerance", {
  d <- tempfile("golden")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  set.seed(42)
  v <- rnorm(100)
  write_golden(v, "selftest", source = "seeded rnorm(42)", dir = d)

  # manifest recorded
  expect_true(file.exists(file.path(d, "selftest.dcf")))
  man <- read.dcf(file.path(d, "selftest.dcf"))
  expect_equal(unname(man[, "Source"]), "seeded rnorm(42)")

  # exact (bit-stable) round-trip
  expect_success(expect_equal_golden(v, "selftest", tol = 0, dir = d))
  # within tolerance passes
  expect_success(expect_equal_golden(v + 1e-10, "selftest", tol = 1e-8, dir = d))
  # beyond tolerance fails
  expect_failure(expect_equal_golden(v + 1e-3, "selftest", tol = 1e-8, dir = d))
})

test_that("a missing golden skips rather than errors (fresh checkout stays green)", {
  d <- tempfile("empty_golden")
  expect_condition(
    expect_equal_golden(1, "does-not-exist", dir = d),
    class = "skip"
  )
})
