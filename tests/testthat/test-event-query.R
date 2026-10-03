# tests/testthat/test-event-query.R
library(testthat)
library(PhysioExperiment)

test_that("eventQuery creates EventQuery object", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)
  pe <- addEvents(pe, onset = c(1, 2, 3), type = "stimulus")

  q <- eventQuery(pe)

  expect_s4_class(q, "EventQuery")
})

test_that("filterType filters events by type", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)
  pe <- addEvents(pe, onset = c(1, 3, 5), type = "stimulus")
  pe <- addEvents(pe, onset = c(2, 4), type = "response")

  q <- eventQuery(pe) |> filterType("stimulus")
  events <- resolveQuery(q)

  expect_equal(nrow(events), 3)
  expect_true(all(events$type == "stimulus"))
})

test_that("filterType accepts multiple types", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)
  pe <- addEvents(pe, onset = c(1, 3), type = "go")
  pe <- addEvents(pe, onset = c(2, 4), type = "nogo")
  pe <- addEvents(pe, onset = c(5), type = "other")

  q <- eventQuery(pe) |> filterType(c("go", "nogo"))
  events <- resolveQuery(q)

  expect_equal(nrow(events), 4)
})

test_that("filterValue filters events by value", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)
  pe <- addEvents(pe, onset = c(1, 2), type = "trial", value = "correct")
  pe <- addEvents(pe, onset = c(3, 4), type = "trial", value = "incorrect")

  q <- eventQuery(pe) |> filterValue("correct")
  events <- resolveQuery(q)

  expect_equal(nrow(events), 2)
  expect_true(all(events$value == "correct"))
})
