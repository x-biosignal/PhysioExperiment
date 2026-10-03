library(testthat)
library(PhysioExperiment)

.mk_pe <- function() {
  PhysioExperiment(
    assays = S4Vectors::SimpleList(raw = matrix(rnorm(20), nrow = 10, ncol = 2)),
    samplingRate = 100
  )
}

test_that("a fresh object reports an empty provenance log", {
  pe <- .mk_pe()
  prov <- provenance(pe)
  expect_s3_class(prov, "data.frame")
  expect_equal(nrow(prov), 0L)
  # back-compatible columns are preserved
  expect_true(all(c("step", "timestamp", "user", "package", "version", "params")
                  %in% names(prov)))
  # W3C PROV-O columns are present
  expect_true(all(c("activity", "entity", "used", "generated", "agent",
                    "startedAtTime", "endedAtTime", "params_json")
                  %in% names(prov)))
})

test_that("three chained ops give a 3-row append-only log with monotonic timestamps", {
  pe <- .mk_pe()
  pe <- logStep(pe, "filterSignals", params = list(low = 1, high = 40))
  pe <- logStep(pe, "epochData", params = list(n = 10))
  pe <- logStep(pe, "averageEpochs")
  prov <- provenance(pe)

  expect_equal(nrow(prov), 3L)
  expect_equal(prov$step, c("filterSignals", "epochData", "averageEpochs"))
  # monotonic (non-decreasing) timestamps
  expect_true(all(diff(as.numeric(prov$timestamp)) >= 0))
  expect_match(prov$params[1], "low=1")
  expect_match(prov$params[1], "high=40")
  expect_equal(prov$params[3], "")
  # user attributed
  expect_true(is.character(prov$user) && all(nzchar(prov$user) | is.na(prov$user)))
})

test_that("provenance is append-only and copy-on-write (earlier object unchanged)", {
  a <- logStep(.mk_pe(), "load")
  b <- logStep(a, "transform")
  expect_equal(provenance(a)$step, "load")            # a not mutated
  expect_equal(provenance(b)$step, c("load", "transform"))
})

test_that("objects without a provenance metadata key deserialize with empty log", {
  pe <- .mk_pe()
  # simulate an 'old' object round-tripped through serialization (no provenance key)
  tf <- tempfile(fileext = ".rds")
  saveRDS(pe, tf)
  restored <- readRDS(tf)
  unlink(tf)
  expect_equal(nrow(provenance(restored)), 0L)
  # and it can start logging normally
  restored <- logStep(restored, "reprocess")
  expect_equal(provenance(restored)$step, "reprocess")
})

test_that("withProvenance carries the source log onto a derived object", {
  a <- logStep(.mk_pe(), "load")
  b <- .mk_pe()  # a freshly derived object with no history
  b <- withProvenance(a, b, "resample", params = list(to = 250))
  expect_equal(provenance(b)$step, c("load", "resample"))
  expect_match(provenance(b)$params[2], "to=250")
})

test_that("provenance<- replaces the log", {
  pe <- logStep(.mk_pe(), "x")
  provenance(pe) <- list()
  expect_equal(nrow(provenance(pe)), 0L)
})

# ---- W3C PROV-O record shape (DMIO-01) -------------------------------------

test_that("appendProvenance records a full PROV-O activity from named fields", {
  pe <- appendProvenance(.mk_pe(), activity = "filterSignals",
                         params = list(low = 1, high = 40),
                         input_assay = "raw", output_assay = "filtered",
                         software_version = "1.2.3", package = "PhysioPreprocess")
  prov <- provenance(pe)
  expect_equal(nrow(prov), 1L)
  # all PROV-O required fields populated (not NA/empty)
  expect_equal(prov$activity, "filterSignals")
  expect_equal(prov$used, "raw")
  expect_equal(prov$generated, "filtered")
  expect_true(nzchar(prov$entity))
  expect_true(!is.na(prov$agent))
  expect_false(is.na(prov$startedAtTime))
  expect_false(is.na(prov$endedAtTime))
  expect_true(prov$endedAtTime >= prov$startedAtTime)
  expect_equal(prov$version, "1.2.3")
  # params serialized as JSON
  expect_match(prov$params_json, '"low":1')
  expect_match(prov$params_json, '"high":40')
})

test_that("appendProvenance accepts an activity name positionally", {
  pe <- appendProvenance(.mk_pe(), "epochData", params = list(n = 5))
  expect_equal(provenance(pe)$activity, "epochData")
  expect_match(provenance(pe)$params_json, '"n":5')
})

test_that("recordActivity times an operation and appends one activity", {
  scale2 <- function(p) {
    SummarizedExperiment::assay(p, "raw") <-
      SummarizedExperiment::assay(p, "raw") * 2
    p
  }
  pe <- logStep(.mk_pe(), "import")
  out <- recordActivity(pe, "rescale", scale2(pe), params = list(gain = 2))
  prov <- provenance(out)
  # the prior log is carried and the new activity appended
  expect_equal(prov$activity, c("import", "rescale"))
  expect_true(prov$endedAtTime[2] >= prov$startedAtTime[2])
  # the operation actually ran on the returned object
  expect_equal(SummarizedExperiment::assay(out, "raw"),
               SummarizedExperiment::assay(pe, "raw") * 2)
})

test_that("recordActivity errors if expr does not return a PhysioExperiment", {
  pe <- .mk_pe()
  expect_error(recordActivity(pe, "bad", 42), "PhysioExperiment")
})

test_that("constructor seeds a wasDerivedFrom import record from a source id", {
  pe <- PhysioExperiment(
    assays = S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)),
    samplingRate = 100, provenance = "sub-01_task-rest_eeg.edf"
  )
  prov <- provenance(pe)
  expect_equal(nrow(prov), 1L)
  expect_equal(prov$activity, "import")
  expect_match(prov$params_json, "wasDerivedFrom")
  expect_equal(prov$used, "sub-01_task-rest_eeg.edf")
})

test_that("an enriched PROV-O log round-trips through serialization", {
  pe <- appendProvenance(.mk_pe(), activity = "filter",
                         params = list(low = 1), input_assay = "raw")
  tf <- tempfile(fileext = ".rds")
  saveRDS(pe, tf); restored <- readRDS(tf); unlink(tf)
  rp <- provenance(restored)
  expect_equal(rp$activity, "filter")
  expect_equal(rp$used, "raw")
  expect_match(rp$params_json, '"low":1')
})

test_that("show() reports the number of provenance steps", {
  pe <- logStep(logStep(.mk_pe(), "a"), "b")
  expect_output(show(pe), "provenance: 2 steps")
  # a fresh object prints no provenance line
  expect_output(show(.mk_pe()), "class: PhysioExperiment")
  expect_false(any(grepl("provenance", capture.output(show(.mk_pe())))))
})

test_that("provenanceHash is deterministic and changes with steps/params", {
  build <- function(high) {
    pe <- logStep(.mk_pe(), "filterSignals", params = list(low = 1, high = high))
    logStep(pe, "epochData", params = list(window = 2))
  }
  Sys.sleep(0.02); h1 <- provenanceHash(build(40))
  Sys.sleep(0.02); h2 <- provenanceHash(build(40))
  expect_identical(h1, h2)                                # same content, later time
  expect_false(identical(h1, provenanceHash(build(30))))  # a param changed
  pe_step <- logStep(logStep(.mk_pe(), "filterSignals",
                             params = list(low = 1, high = 40)),
                     "detectArtifacts", params = list(window = 2))
  expect_false(identical(h1, provenanceHash(pe_step)))     # a step changed
  # an empty log hashes stably
  expect_identical(provenanceHash(.mk_pe()), provenanceHash(.mk_pe()))
  expect_type(h1, "character")
  # structural hashing: field values containing delimiters cannot collide
  s1 <- provenanceHash(logStep(.mk_pe(), "a|b", params = list(x = "1;;2")))
  s2 <- provenanceHash(logStep(.mk_pe(), "a", params = list(x = "b|1;;2")))
  expect_false(identical(s1, s2))
})
