library(testthat)
library(PhysioExperiment)

test_that("connectDatabase creates in-memory connection", {
  skip_if_not_installed("duckdb")

  con <- connectDatabase()
  expect_true(DBI::dbIsValid(con))
  disconnectDatabase(con)
})

test_that("initPhysioSchema creates tables", {
  skip_if_not_installed("duckdb")

  con <- connectDatabase()
  initPhysioSchema(con)

  tables <- DBI::dbListTables(con)
  expect_true("experiments" %in% tables)

  disconnectDatabase(con)
})

test_that("registerExperiment and loadExperiment roundtrip", {
  skip_if_not_installed("duckdb")

  pe <- make_pe_2d(n_time = 100, n_channels = 4, sr = 250)

  con <- connectDatabase()
  initPhysioSchema(con)

  # Register
  exp_id <- registerExperiment(con, pe,
    subject_id = "sub-01",
    task = "test"
  )
  expect_true(is.character(exp_id) || is.numeric(exp_id))

  # Query
  results <- queryExperiments(con)
  expect_true(nrow(results) >= 1)

  disconnectDatabase(con)
})

test_that("dbStats returns statistics", {
  skip_if_not_installed("duckdb")

  con <- connectDatabase()
  initPhysioSchema(con)

  stats <- dbStats(con)
  expect_true(is.list(stats))

  disconnectDatabase(con)
})
