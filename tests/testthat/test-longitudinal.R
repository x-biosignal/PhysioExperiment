library(testthat)
library(PhysioExperiment)

.mk_sess <- function(sr = 250, n = 100, ch = 2) {
  PhysioExperiment(
    assays = S4Vectors::SimpleList(raw = matrix(rnorm(n * ch), n, ch)),
    samplingRate = sr
  )
}

.mk_4visit <- function() {
  PhysioLongitudinal(
    baseline = .mk_sess(), mid = .mk_sess(),
    discharge = .mk_sess(), followup = .mk_sess(),
    design = S4Vectors::DataFrame(
      session_id = c("baseline", "mid", "discharge", "followup"),
      visit_label = c("baseline", "mid", "discharge", "followup"),
      days_from_baseline = c(0, 21, 42, 90)),
    subject = S4Vectors::DataFrame(id = "sub-01", dx = "stroke", side = "L")
  )
}

test_that("a 4-visit subject exposes sessions and a chronological design", {
  pl <- .mk_4visit()
  expect_s4_class(pl, "PhysioLongitudinal")
  expect_equal(length(pl), 4L)
  expect_equal(length(sessions(pl)), 4L)

  d <- design(pl)
  expect_equal(nrow(d), 4L)                               # one row per visit
  expect_equal(d$visit_label, c("baseline", "mid", "discharge", "followup"))
  expect_true(!is.unsorted(d$days_from_baseline))         # chronological
  expect_equal(subjectData(pl)$id, "sub-01")
})

test_that("design is re-sorted chronologically even if built out of order", {
  pl <- PhysioLongitudinal(
    discharge = .mk_sess(), baseline = .mk_sess(),
    design = S4Vectors::DataFrame(
      session_id = c("discharge", "baseline"),
      visit_label = c("discharge", "baseline"),
      days_from_baseline = c(42, 0)))
  expect_equal(design(pl)$visit_label, c("baseline", "discharge"))
  expect_equal(names(sessions(pl)), c("baseline", "discharge"))
})

test_that("session() retrieves the correct PhysioExperiment by visit label", {
  pl <- .mk_4visit()
  s <- session(pl, "discharge")
  expect_s4_class(s, "PhysioExperiment")
  expect_identical(s, sessions(pl)[["discharge"]])
  expect_error(session(pl, "nonexistent"), "no session")
})

test_that("addSession appends and re-sorts by days_from_baseline", {
  pl <- .mk_4visit()
  pl2 <- addSession(pl, "week2", .mk_sess(), days_from_baseline = 14)
  expect_equal(length(pl2), 5L)
  # the day-14 visit is inserted between baseline (0) and mid (21)
  expect_equal(design(pl2)$visit_label,
               c("baseline", "week2", "mid", "discharge", "followup"))
  expect_true(!is.unsorted(design(pl2)$days_from_baseline))
  expect_error(addSession(pl, "dup", .mk_sess(), 5, session_id = "baseline"),
               "already exists")
})

test_that("[ subsets sessions and design together", {
  pl <- .mk_4visit()
  sub <- pl[c("baseline", "discharge")]
  expect_s4_class(sub, "PhysioLongitudinal")
  expect_equal(length(sub), 2L)
  expect_equal(design(sub)$visit_label, c("baseline", "discharge"))
})

test_that("default design is built from session names when omitted", {
  pl <- PhysioLongitudinal(baseline = .mk_sess(), followup = .mk_sess())
  d <- design(pl)
  expect_equal(nrow(d), 2L)
  expect_setequal(d$visit_label, c("baseline", "followup"))
  expect_true(all(c("session_id", "days_from_baseline", "condition") %in% names(d)))
})

test_that("provenance aggregates across sessions with a session column", {
  b <- logStep(.mk_sess(), "importBaseline")
  d <- logStep(.mk_sess(), "importDischarge")
  pl <- PhysioLongitudinal(baseline = b, discharge = d)
  p <- provenance(pl)
  expect_true("session" %in% names(p))
  expect_setequal(p$session, c("baseline", "discharge"))
  expect_setequal(p$activity, c("importBaseline", "importDischarge"))
})

test_that("invalid inputs are rejected", {
  expect_error(PhysioLongitudinal(.mk_sess()), "named")          # unnamed session
  expect_error(PhysioLongitudinal(a = 42), "PhysioExperiment")   # not a session
  expect_error(
    PhysioLongitudinal(baseline = .mk_sess(),
                       design = S4Vectors::DataFrame(
                         session_id = "other", visit_label = "x",
                         days_from_baseline = 0)),
    "match")
})

# ---- regression tests for adversarial-review findings ----------------------

test_that("session() is NA-safe when another visit_label is NA", {
  pl <- PhysioLongitudinal(
    a = .mk_sess(), b = .mk_sess(),
    design = S4Vectors::DataFrame(
      session_id = c("a", "b"), visit_label = c(NA_character_, "b"),
      days_from_baseline = c(0, 10)))
  expect_identical(session(pl, "b"), sessions(pl)[["b"]])   # not NULL
  expect_identical(session(pl, "a"), sessions(pl)[["a"]])   # id fallback still works
})

test_that("[ keeps sessions aligned to the (sorted) design even when reordered", {
  pl <- .mk_4visit()
  sub <- pl[c("discharge", "baseline")]        # requested out of order
  expect_equal(names(sessions(sub)), design(sub)$session_id)   # aligned
  expect_equal(design(sub)$visit_label, c("baseline", "discharge"))  # chronological
  expect_true(validObject(sub))
})

test_that("duplicate session ids are rejected", {
  expect_error(
    PhysioLongitudinal(
      baseline = .mk_sess(),
      design = S4Vectors::DataFrame(
        session_id = c("baseline", "baseline"), visit_label = c("a", "b"),
        days_from_baseline = c(0, 1))),
    "unique")
})

test_that("a design missing days_from_baseline errors clearly", {
  expect_error(
    PhysioLongitudinal(
      baseline = .mk_sess(),
      design = S4Vectors::DataFrame(session_id = "baseline", visit_label = "baseline")),
    "missing required column")
  expect_error(
    PhysioLongitudinal(
      baseline = .mk_sess(),
      design = S4Vectors::DataFrame(
        session_id = "baseline", visit_label = "baseline",
        days_from_baseline = NA_real_)),
    "must not contain NA")
})

test_that("addSession preserves extra design columns", {
  pl <- PhysioLongitudinal(
    baseline = .mk_sess(),
    design = S4Vectors::DataFrame(
      session_id = "baseline", visit_label = "baseline",
      days_from_baseline = 0, site = "clinicA"))
  pl2 <- addSession(pl, "followup", .mk_sess(), days_from_baseline = 90)
  expect_true("site" %in% names(design(pl2)))
  expect_equal(design(pl2)$site, c("clinicA", NA))
})

test_that("design<- re-sorts chronologically and realigns sessions", {
  pl <- .mk_4visit()
  d <- as.data.frame(design(pl))
  d$days_from_baseline <- c(0, 21, 100, 42)   # reorder discharge after followup
  design(pl) <- S4Vectors::DataFrame(d)
  expect_equal(names(sessions(pl)), design(pl)$session_id)     # realigned
  expect_true(!is.unsorted(design(pl)$days_from_baseline))
  expect_true(validObject(pl))
})

test_that("as(x, 'MultiAssayExperiment') round-trips the session names", {
  skip_if_not_installed("MultiAssayExperiment")
  pl <- .mk_4visit()
  mae <- as(pl, "MultiAssayExperiment")
  expect_s4_class(mae, "MultiAssayExperiment")
  expect_setequal(names(MultiAssayExperiment::experiments(mae)),
                  names(sessions(pl)))
  back <- as(mae, "PhysioLongitudinal")
  expect_setequal(names(sessions(back)), names(sessions(pl)))
  expect_setequal(design(back)$visit_label, design(pl)$visit_label)
})
