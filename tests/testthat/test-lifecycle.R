library(testthat)
library(PhysioExperiment)

# lifecycle silences repeat warnings within a session; force them for testing.
local_verbose <- function(env = parent.frame()) {
  old <- options(lifecycle_verbosity = "warning")
  withr::defer(options(old), envir = env)
}

test_that("deprecate_physio warns by default and names the replacement", {
  if (!requireNamespace("withr", quietly = TRUE)) skip("withr not installed")
  local_verbose()
  old_fn <- function() deprecate_physio("0.3.0", "old_fn()", with = "new_fn()")
  expect_warning(old_fn(), class = "lifecycle_warning_deprecated")
  expect_warning(old_fn(), "deprecated")
  expect_warning(old_fn(), "new_fn")
})

test_that("deprecate_physio with severity = 'stop' raises a defunct error", {
  gone <- function() deprecate_physio("0.3.0", "gone()", with = "keep()",
                                      severity = "stop")
  expect_error(gone(), class = "lifecycle_error_deprecated")
  expect_error(gone(), "keep")
})

test_that("deprecate_physio validates its inputs", {
  # stopifnot preserves the expression text (locale-independent); match.arg's
  # message is localized, so assert only that it errors.
  expect_error(deprecate_physio(when = 0.3, what = "f()"), "is.character")
  expect_error(deprecate_physio(when = "0.3.0", what = NA_character_), "is.na")
  expect_error(deprecate_physio("0.3.0", "f()", severity = "boom"))
})

test_that("deprecate_physio returns invisibly", {
  if (!requireNamespace("withr", quietly = TRUE)) skip("withr not installed")
  local_verbose()
  f <- function() deprecate_physio("0.3.0", "f()")
  expect_invisible(suppressWarnings(f()))
})
