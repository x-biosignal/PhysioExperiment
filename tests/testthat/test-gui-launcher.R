library(testthat)
library(PhysioExperiment)

# --- Function existence ---

test_that("launchGUI function exists", {
  expect_true(is.function(launchGUI))
})

test_that("startAPIServer function exists", {
  expect_true(is.function(startAPIServer))
})

test_that("checkGUIDependencies function exists", {
  expect_true(is.function(checkGUIDependencies))
})

# --- checkGUIDependencies ---

test_that("checkGUIDependencies returns a list", {
  result <- checkGUIDependencies()
  expect_true(is.list(result))
})

test_that("checkGUIDependencies contains expected elements", {
  result <- checkGUIDependencies()

  expect_true("plumber" %in% names(result))
  expect_true("jsonlite" %in% names(result))
  expect_true("later" %in% names(result))
  expect_true("callr" %in% names(result))
  expect_true("gui_built" %in% names(result))
  expect_true("electron" %in% names(result))
  expect_true("nodejs" %in% names(result))
  expect_true("npm" %in% names(result))
})

test_that("checkGUIDependencies returns logical values", {
  result <- checkGUIDependencies()

  for (name in names(result)) {
    expect_true(is.logical(result[[name]]),
                info = paste("Element", name, "should be logical"))
  }
})

test_that("checkGUIDependencies correctly detects jsonlite", {
  result <- checkGUIDependencies()

  # jsonlite is a common dependency that should be installed
  has_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)
  expect_equal(result$jsonlite, has_jsonlite)
})

test_that("checkGUIDependencies returns invisible", {
  # The function should return invisibly
  expect_invisible(checkGUIDependencies())
})

# --- launchGUI input validation ---

test_that("launchGUI errors without plumber package", {
  # If plumber is not installed, launchGUI should error
  # We test the error message pattern regardless
  skip_if(requireNamespace("plumber", quietly = TRUE),
          "plumber is installed, cannot test missing-package error")

  expect_error(launchGUI(), "plumber")
})

test_that("launchGUI has expected default parameters", {
  # Check that the function signature has expected defaults
  args <- formals(launchGUI)

  expect_equal(args$port, 8000L)
  expect_equal(args$host, "127.0.0.1")
  expect_false(args$desktop)
  expect_true(args$browser)
  expect_false(args$quiet)
})

# --- startAPIServer input validation ---

test_that("startAPIServer errors without plumber package", {
  skip_if(requireNamespace("plumber", quietly = TRUE),
          "plumber is installed, cannot test missing-package error")

  expect_error(startAPIServer(), "plumber")
})

test_that("startAPIServer errors without callr package", {
  skip_if(!requireNamespace("plumber", quietly = TRUE),
          "plumber is not installed, will error on plumber first")
  skip_if(requireNamespace("callr", quietly = TRUE),
          "callr is installed, cannot test missing-package error")

  expect_error(startAPIServer(), "callr")
})

test_that("startAPIServer has expected default parameters", {
  args <- formals(startAPIServer)

  expect_equal(args$port, 8000L)
  expect_equal(args$host, "127.0.0.1")
  expect_false(args$quiet)
})
