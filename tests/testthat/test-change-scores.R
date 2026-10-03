library(testthat)
library(PhysioExperiment)

.mk_val <- function(m, n = 10, ch = 2) {
  PhysioExperiment(
    assays = S4Vectors::SimpleList(raw = matrix(m, n, ch)),
    samplingRate = 100)
}

# a 4-visit subject whose mean-of-raw metric is exactly 1, 2, 4, 3
.mk_subject <- function() {
  PhysioLongitudinal(
    baseline = .mk_val(1), mid = .mk_val(2),
    discharge = .mk_val(4), followup = .mk_val(3),
    design = S4Vectors::DataFrame(
      session_id = c("baseline", "mid", "discharge", "followup"),
      visit_label = c("baseline", "mid", "discharge", "followup"),
      days_from_baseline = c(0, 21, 42, 90)),
    subject = S4Vectors::DataFrame(id = "sub-01", dx = "stroke"))
}

.meanmetric <- function(e) mean(SummarizedExperiment::assay(e, "raw"))

test_that("changeScores returns the correct absolute deltas vs baseline", {
  cs <- changeScores(.mk_subject(), .meanmetric)
  expect_s4_class(cs, "DataFrame")
  expect_equal(nrow(cs), 4L)
  expect_equal(as.character(cs$visit),
               c("baseline", "mid", "discharge", "followup"))
  expect_equal(cs$value, c(1, 2, 4, 3))
  expect_equal(cs$delta_from_baseline, c(0, 1, 3, 2))
  expect_equal(cs$subject, rep("sub-01", 4))
  expect_equal(cs$days_from_baseline, c(0, 21, 42, 90))
})

test_that("percent-change vs baseline is correct", {
  cs <- changeScores(.mk_subject(), .meanmetric, method = "percent")
  expect_equal(cs$delta_from_baseline, c(0, 100, 300, 200))
})

test_that("z-change divides by the metric's across-visit SD", {
  cs <- changeScores(.mk_subject(), .meanmetric, method = "z")
  s <- stats::sd(c(1, 2, 4, 3))
  expect_equal(cs$delta_from_baseline, c(0, 1, 3, 2) / s)
})

test_that("baseline can be chosen by visit label", {
  cs <- changeScores(.mk_subject(), .meanmetric, baseline = "discharge")
  # deltas relative to discharge (value 4): -3, -2, 0, -1
  expect_equal(cs$delta_from_baseline, c(-3, -2, 0, -1))
})

test_that("a scalar mdc flags visits whose absolute change exceeds it", {
  cs <- changeScores(.mk_subject(), .meanmetric, mdc = 2.5)
  # |deltas| = 0,1,3,2 -> only discharge (3) exceeds 2.5
  expect_equal(cs$exceeds_mdc, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("metric_fn returning several named metrics tidies to one row per metric", {
  mf <- function(e) c(mean = mean(SummarizedExperiment::assay(e, "raw")),
                      sd = stats::sd(SummarizedExperiment::assay(e, "raw")))
  cs <- changeScores(.mk_subject(), mf)
  expect_setequal(unique(cs$metric), c("mean", "sd"))
  expect_equal(nrow(cs), 8L)                     # 4 visits x 2 metrics
})

test_that("exceeds_mdc matches an independent mskMinimalDetectableChange()", {
  skip_if_not_installed("PhysioMSKNet")
  skip_if_not("mskMinimalDetectableChange" %in%
                getNamespaceExports("PhysioMSKNet"))
  # a subject with a wider swing so some visits exceed MDC and some do not
  pl <- PhysioLongitudinal(
    baseline = .mk_val(10), mid = .mk_val(11),
    discharge = .mk_val(25), followup = .mk_val(18))

  tr <- asMSKTracker(pl, .meanmetric)
  expect_s3_class(tr, "MSKLongitudinalTracker")
  mdc <- PhysioMSKNet::mskMinimalDetectableChange(tr)$mdc_table$MDC[1]
  expect_true(is.finite(mdc))

  cs <- changeScores(pl, .meanmetric, mdc = mdc)
  base_val <- cs$value[cs$visit == "baseline"][1]
  expect_equal(cs$exceeds_mdc, abs(cs$value - base_val) > mdc)
  # the recovery-fit bridge also consumes the tracker
  traj <- PhysioMSKNet::mskRecoveryTrajectoryFit(tr, model = "linear")
  expect_s3_class(traj, "MSKRecoveryTrajectory")
})

test_that("invalid metric_fn output is rejected", {
  expect_error(changeScores(.mk_subject(), function(e) "notnumeric"),
               "numeric")
  expect_error(
    changeScores(.mk_subject(), function(e) {
      # inconsistent metric names across sessions
      if (mean(SummarizedExperiment::assay(e, "raw")) < 2) c(a = 1) else c(b = 1)
    }), "same metric name")
})

# ---- regression tests for adversarial-review findings ----------------------

test_that("duplicate metric names are rejected (no silent corruption)", {
  mf <- function(e) c(a = mean(SummarizedExperiment::assay(e, "raw")),
                      a = 2 * mean(SummarizedExperiment::assay(e, "raw")))
  expect_error(changeScores(.mk_subject(), mf), "uniquely named")
})

test_that("the same metrics in a different order across sessions are accepted", {
  mf <- function(e) {
    m <- mean(SummarizedExperiment::assay(e, "raw"))
    if (m < 2.5) c(mean = m, twice = 2 * m) else c(twice = 2 * m, mean = m)
  }
  cs <- changeScores(.mk_subject(), mf)
  # 'mean' metric values must be 1,2,4,3 regardless of return order
  mean_rows <- cs[cs$metric == "mean", ]
  expect_equal(mean_rows$value, c(1, 2, 4, 3))
})

test_that("a named mdc with an unmatched name warns", {
  expect_warning(
    changeScores(.mk_subject(), .meanmetric, mdc = c(wrongname = 1)),
    "not among metrics")
})

test_that("an unnamed multi-element mdc is rejected", {
  mf <- function(e) c(a = mean(SummarizedExperiment::assay(e, "raw")),
                      b = stats::sd(SummarizedExperiment::assay(e, "raw")))
  expect_error(changeScores(.mk_subject(), mf, mdc = c(1, 2)),
               "name `mdc` by metric")
})

test_that("metric_fn errors name the offending session", {
  bad <- function(e) if (mean(SummarizedExperiment::assay(e, "raw")) == 4)
    stop("boom") else 1
  expect_error(changeScores(.mk_subject(), bad), "session 'discharge'")
})
