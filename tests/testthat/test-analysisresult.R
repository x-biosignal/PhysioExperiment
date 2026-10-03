library(testthat)
library(PhysioExperiment)

test_that("AnalysisResult carries and round-trips estimate/uncertainty/estimand", {
  r <- AnalysisResult(
    "rom", estimate = 118, method = "conformal",
    uncertainty = list(type = "conformal", level = 0.9, lower = 104,
                       upper = 132),
    estimand = list(population = "post-op knee", summary_measure = "median"))
  expect_s4_class(r, "AnalysisResult")
  expect_equal(estimateOf(r), 118)
  expect_equal(uncertaintyOf(r)$type, "conformal")
  expect_equal(uncertaintyOf(r)$lower, 104)
  expect_equal(estimandOf(r)$population, "post-op knee")
  expect_equal(r@method, "conformal")
  out <- capture.output(show(r))
  expect_true(any(grepl("118", out)))            # estimate shown
  expect_true(any(grepl("conformal", out)))      # uncertainty type shown
  expect_true(any(grepl("population", out)))      # estimand shown
})

test_that("validity rejects malformed uncertainty or estimand lists", {
  expect_error(AnalysisResult("x", uncertainty = list(type = "bogus")),
               "uncertainty")
  expect_error(AnalysisResult("x", uncertainty = list(level = 0.9)),  # no type
               "uncertainty")
  expect_error(AnalysisResult("x", estimand = list("a", "b")),        # unnamed
               "estimand")
  expect_error(AnalysisResult("x", estimand = list(pop = "a", "b")),  # partial
               "estimand")
  expect_s4_class(AnalysisResult("x"), "AnalysisResult")               # empty ok
  # a valid uncertainty type is accepted
  expect_s4_class(AnalysisResult("x", uncertainty = list(type = "bayes")),
                  "AnalysisResult")
})

test_that("existing constructor and PhysioBiomarker are unchanged", {
  old <- AnalysisResult("hrv_time", result = list(sdnn = 42, rmssd = 30),
                        provenance = data.frame(a = 1))
  expect_equal(resultType(old), "hrv_time")
  expect_equal(resultValue(old)$sdnn, 42)
  expect_null(estimateOf(old))
  expect_length(uncertaintyOf(old), 0)
  expect_equal(provenanceOf(old), data.frame(a = 1))
  bm <- PhysioBiomarker("SDNN", 42, unit = "ms", ci = c(38, 46))
  expect_equal(biomarkerValue(bm), 42)
  expect_s4_class(bm, "AnalysisResult")           # subclass still valid
})

test_that("addProvenance appends without dropping entries and captures the seed", {
  pe <- PhysioExperiment(S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)),
                         samplingRate = 100)
  pe <- addProvenance(pe, "step1", seed = 1)
  pe <- addProvenance(pe, "step2", seed = 2)
  pe <- addProvenance(pe, "step3", params = list(alpha = 0.1))
  prov <- provenance(pe)
  expect_equal(NROW(prov), 3L)                    # all appended
  expect_equal(prov$step, c("step1", "step2", "step3"))   # order, none dropped
  expect_true(grepl("seed", prov$params_json[1]))
  # deterministic under a fixed seed: the captured content reproduces
  pe2 <- PhysioExperiment(S4Vectors::SimpleList(raw = matrix(0, 10, 2)),
                          samplingRate = 100)
  pe2 <- addProvenance(pe2, "step1", seed = 1)
  expect_equal(provenance(pe2)$params_json[1], prov$params_json[1])
  expect_error(addProvenance(list(), "x"), "PhysioExperiment")
})

test_that("the carrier accessors and class are exported and usable", {
  for (fn in c("estimateOf", "uncertaintyOf", "provenanceOf", "estimandOf",
               "addProvenance")) {
    expect_true(exists(fn, mode = "function"))
  }
  expect_false(isVirtualClass("AnalysisResult"))
  expect_true(methods::existsMethod("estimateOf", "AnalysisResult"))
  expect_true(methods::existsMethod("estimandOf", "AnalysisResult"))
})

# --- regression tests for adversarial-review findings (WS8-04) -----------------

test_that("accessors and show tolerate a pre-extension object missing new slots", {
  # simulate an object serialized before the carrier slots existed
  old <- AnalysisResult("hrv", result = list(sdnn = 42))
  for (s in c("estimate", "uncertainty", "method", "estimand")) {
    attr(old, s) <- NULL
  }
  expect_null(estimateOf(old))                 # no "no slot of name" error
  expect_length(uncertaintyOf(old), 0L)
  expect_length(estimandOf(old), 0L)
  expect_silent(capture.output(show(old)))
  expect_equal(resultValue(old)$sdnn, 42)      # old accessors still work
})

test_that("show tolerates a non-numeric or vector uncertainty level", {
  r1 <- new("AnalysisResult", type = "x",
            uncertainty = list(type = "bayes", level = "high"))
  expect_silent(capture.output(show(r1)))      # non-numeric level: no arithmetic error
  r2 <- new("AnalysisResult", type = "x",
            uncertainty = list(type = "bootstrap", level = c(0.9, 0.95),
                               lower = c(1, 2), upper = c(3, 4)))
  expect_silent(capture.output(show(r2)))      # length>1 fields: no format error
})

test_that("addProvenance rejects a non-integer seed", {
  pe <- PhysioExperiment(S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)),
                         samplingRate = 100)
  expect_error(addProvenance(pe, "s", seed = "oops"))
  expect_error(addProvenance(pe, "s", seed = 1.5))
  expect_s4_class(addProvenance(pe, "s", seed = 5), "PhysioExperiment")
})
