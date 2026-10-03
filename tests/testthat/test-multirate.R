library(testthat)
library(PhysioExperiment)

.mk_stream <- function(n, ch, sr) {
  PhysioExperiment(
    assays = S4Vectors::SimpleList(raw = matrix(rnorm(n * ch), n, ch)),
    samplingRate = sr
  )
}

test_that("container reports per-stream rates and a t=0 aligned clock", {
  kin <- .mk_stream(100, 3, 100)      # 100 Hz kinematics
  force <- .mk_stream(1000, 6, 1000)  # 1000 Hz analog force
  emg <- .mk_stream(2000, 2, 2000)    # 2000 Hz EMG
  mr <- MultiRatePhysioExperiment(kinematics = kin, force = force, emg = emg)

  expect_s4_class(mr, "MultiRatePhysioExperiment")
  expect_equal(unname(streamRates(mr)), c(100, 1000, 2000))
  expect_equal(streamNames(mr), c("kinematics", "force", "emg"))
  expect_equal(nStreams(mr), 3L)
  expect_equal(length(mr), 3L)

  cl <- commonClock(mr)
  expect_equal(cl$t0, 0)
  expect_equal(cl$reference_rate, 2000)          # defaults to the highest rate
  expect_true(all(cl$offsets == 0))              # every stream aligned at t=0
  expect_equal(streamTimeIndex(mr, "emg")[1], 0)
  expect_equal(streamTimeIndex(mr, "kinematics")[1], 0)
})

test_that("resampleToCommon yields equal-length, common-rate aligned assays", {
  mr <- MultiRatePhysioExperiment(
    kinematics = .mk_stream(100, 3, 100),
    force = .mk_stream(1000, 6, 1000),
    emg = .mk_stream(2000, 2, 2000)
  )
  aligned <- resampleToCommon(mr, 1000)

  expect_s4_class(aligned, "PhysioExperiment")
  a <- SummarizedExperiment::assay(aligned, "aligned")
  expect_equal(ncol(a), 11L)                     # 3 + 6 + 2 channels merged
  expect_equal(samplingRate(aligned), 1000)
  expect_equal(nrow(a), 1000L)                   # single common length (~1 s)
  # colData tracks the originating stream + native rate
  cd <- SummarizedExperiment::colData(aligned)
  expect_setequal(unique(cd$stream), c("kinematics", "force", "emg"))
  # the alignment is recorded in provenance
  expect_true("resampleToCommon" %in% provenance(aligned)$activity)
})

test_that("a stream start offset is preserved within one sample", {
  mr <- MultiRatePhysioExperiment(
    kinematics = .mk_stream(100, 2, 100),   # 1 s at t0
    emg = .mk_stream(1000, 2, 1000),        # 1 s, starting 0.25 s late
    offsets = c(kinematics = 0, emg = 0.25)   # every stream, explicitly
  )
  aligned <- resampleToCommon(mr, 1000)
  a <- SummarizedExperiment::assay(aligned, "aligned")
  emg_cols <- grep("^emg\\.", colnames(a))
  first_valid <- which(!is.na(a[, emg_cols[1]]))[1]
  # 0.25 s * 1000 Hz = sample 250 (0-indexed) -> row 251 (1-indexed)
  expect_lte(abs(first_valid - 251L), 1L)
})

test_that("saveRDS round-trip preserves per-stream rates and the clock", {
  mr <- MultiRatePhysioExperiment(
    kin = .mk_stream(100, 2, 100), emg = .mk_stream(2000, 2, 2000),
    offsets = c(kin = 0, emg = 0.1)           # every stream, explicitly
  )
  tf <- tempfile(fileext = ".rds")
  saveRDS(mr, tf); back <- readRDS(tf); unlink(tf)

  expect_equal(streamRates(back), streamRates(mr))
  expect_equal(commonClock(back), commonClock(mr))
  expect_equal(streamNames(back), streamNames(mr))
  expect_s4_class(back[["emg"]], "PhysioExperiment")
})

test_that("alignStreams uses the reference rate; accessors and dim work", {
  mr <- MultiRatePhysioExperiment(
    kin = .mk_stream(100, 2, 100), emg = .mk_stream(1000, 2, 1000)
  )
  al <- alignStreams(mr)
  expect_equal(samplingRate(al), 1000)           # reference rate = max

  expect_s4_class(mr[["kin"]], "PhysioExperiment")
  expect_equal(samplingRate(mr[["emg"]]), 1000)
  d <- dim(mr)
  expect_equal(rownames(d), c("kin", "emg"))
  expect_equal(unname(d[, "nsamples"]), c(100L, 1000L))
})

test_that("provenance aggregates across streams with a stream column", {
  kin <- logStep(.mk_stream(100, 2, 100), "importKin")
  emg <- logStep(.mk_stream(1000, 2, 1000), "importEmg")
  mr <- MultiRatePhysioExperiment(kin = kin, emg = emg)

  p <- provenance(mr)
  expect_true("stream" %in% names(p))
  expect_setequal(p$stream, c("kin", "emg"))
  expect_setequal(p$activity, c("importKin", "importEmg"))
})

test_that("empty container and validity checks behave", {
  empty <- MultiRatePhysioExperiment()
  expect_equal(nStreams(empty), 0L)
  expect_equal(length(streamRates(empty)), 0L)
  expect_error(
    MultiRatePhysioExperiment(.mk_stream(10, 2, 100)),  # unnamed stream
    "named"
  )
  expect_error(
    MultiRatePhysioExperiment(a = 42),                  # not a PhysioExperiment
    "PhysioExperiment"
  )
})
