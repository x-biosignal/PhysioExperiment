# Acceptance tests for the unified multimodal container
# (CORE-CONTAINER-UNIFY-01 section 7). Each block maps to one row of that table.

mk <- function(n, sr, nch = 2) {
  PhysioExperiment(assays = list(raw = matrix(seq_len(n * nch) * 1.0, n, nch)),
                   samplingRate = sr)
}

# ---- construction and pure migration ---------------------------------------

test_that("construction keeps signals, rates and dimensions untouched", {
  a <- mk(100, 100); b <- mk(1000, 1000); c3 <- mk(2000, 2000)
  m <- MultiPhysioExperiment(streams = list(a = a, b = b, c = c3))

  expect_s4_class(m, "MultiPhysioExperiment")
  expect_equal(streamNames(m), c("a", "b", "c"))
  expect_equal(unname(streamRates(m)), c(100, 1000, 2000))
  # no implicit resampling: every stream still has its own length
  expect_equal(unname(dim(m)[, "nsamples"]), c(100, 1000, 2000))
  expect_identical(SummarizedExperiment::assay(m[["b"]], "raw"),
                   SummarizedExperiment::assay(b, "raw"))
})

test_that("streams that share a rate use the same class", {
  m <- MultiPhysioExperiment(x = mk(50, 250), y = mk(50, 250))
  expect_s4_class(m, "MultiPhysioExperiment")
  expect_equal(unname(streamRates(m)), c(250, 250))
})

test_that("an omitted clock records that simultaneity was assumed", {
  m <- MultiPhysioExperiment(a = mk(10, 100), b = mk(10, 100))
  expect_true(commonClock(m)$offsets_assumed)
  expect_equal(unname(commonClock(m)$offsets), c(0, 0))

  m2 <- MultiPhysioExperiment(streams = list(a = mk(10, 100), b = mk(10, 100)),
                              offsets = c(a = 0, b = 0.25))
  expect_false(commonClock(m2)$offsets_assumed)
})

# ---- the shared clock -------------------------------------------------------

test_that("sample times match an independent calculation at several rates", {
  rates <- c(100, 1000, 2000)
  offs <- c(a = 0.5, b = -0.25, c = 0)
  m <- MultiPhysioExperiment(
    streams = list(a = mk(20, rates[1]), b = mk(30, rates[2]), c = mk(40, rates[3])),
    t0 = 1234.5, offsets = offs)

  for (i in seq_along(rates)) {
    nm <- names(offs)[i]
    n <- c(20, 30, 40)[i]
    expected <- offs[[nm]] + (seq_len(n) - 1) / rates[i]
    expect_equal(streamTimeIndex(m, nm), expected, tolerance = 1e-12, info = nm)
  }
})

test_that("streamTimeIndex is relative to t0 and never adds it", {
  m <- MultiPhysioExperiment(streams = list(a = mk(10, 100)),
                             t0 = 9999, offsets = c(a = 0.5))
  expect_equal(streamTimeIndex(m, "a")[1], 0.5)
  expect_equal(commonClock(m)$t0, 9999)
})

test_that("a missing absolute origin stays missing", {
  m <- MultiPhysioExperiment(streams = list(a = mk(10, 100)), t0 = NA_real_)
  expect_true(is.na(commonClock(m)$t0))
  expect_true(methods::validObject(m))
})

# ---- invalid input ----------------------------------------------------------

test_that("bad streams and bad clocks are rejected", {
  a <- mk(10, 100)
  expect_error(MultiPhysioExperiment(streams = list(a, a)), "must be named")
  expect_error(MultiPhysioExperiment(streams = list(a = a, a = a)), "unique")
  expect_error(MultiPhysioExperiment(streams = list(a = a, b = "not a PE")),
               "PhysioExperiment objects")
  # an incomplete clock must not be silently completed with zeros
  expect_error(
    MultiPhysioExperiment(streams = list(a = a, b = mk(10, 100)), offsets = c(a = 0)),
    "incomplete")
  expect_error(
    MultiPhysioExperiment(streams = list(a = a), offsets = c(zz = 0)),
    "do not exist")
  expect_error(
    MultiPhysioExperiment(streams = list(a = a), offsets = c(a = Inf)),
    "finite")
})

test_that("validity rejects a clock that does not describe the streams", {
  m <- MultiPhysioExperiment(streams = list(a = mk(10, 100), b = mk(10, 100)))
  bad <- m; bad@clock$offsets <- c(a = 0)
  expect_error(methods::validObject(bad), "no entry for")
  bad2 <- m; bad2@clock$offsets <- c(a = 0, b = 0, zz = 0)
  expect_error(methods::validObject(bad2), "unknown streams")
  bad3 <- m; bad3@clock$t0 <- c(1, 2)
  expect_error(methods::validObject(bad3), "numeric scalar")
  bad4 <- m; bad4@clock$offsets <- NULL
  expect_error(methods::validObject(bad4), "missing")
})

test_that("a legacy alignment that contradicts its own streams is rejected", {
  m_streams <- list(a = mk(10, 100), b = mk(10, 250))
  good <- S4Vectors::DataFrame(modality = c("a", "b"),
                               samplingRate = c(100, 250), offset = c(0, 0.1))
  expect_s4_class(MultiPhysioExperiment(streams = m_streams, alignment = good),
                  "MultiPhysioExperiment")
  wrong <- S4Vectors::DataFrame(modality = c("a", "b"),
                                samplingRate = c(100, 999), offset = c(0, 0.1))
  expect_error(MultiPhysioExperiment(streams = m_streams, alignment = wrong),
               "disagrees with the streams")
})

test_that("new and legacy spellings may not disagree silently", {
  a <- mk(10, 100)
  expect_error(MultiPhysioExperiment(streams = list(a = a), experiments = list(b = a)),
               "not both")
  expect_error(MultiPhysioExperiment(streams = list(a = a), offsets = c(a = 0),
                                     alignment = S4Vectors::DataFrame(
                                       modality = "a", offset = 0)),
               "not both")
})

# ---- time windows -----------------------------------------------------------

test_that("a window selects by actual time, honouring offsets", {
  # b starts 0.2 s late; a window at 0.3-0.5 s must not take b's first samples
  m <- MultiPhysioExperiment(streams = list(a = mk(100, 100), b = mk(250, 250)),
                             offsets = c(a = 0, b = 0.2))
  w <- timeWindow(m, 0.3, 0.5)

  ta <- streamTimeIndex(w, "a"); tb <- streamTimeIndex(w, "b")
  expect_gte(min(ta), 0.3 - 1e-9); expect_lte(max(ta), 0.5 + 1e-9)
  expect_gte(min(tb), 0.3 - 1e-9); expect_lte(max(tb), 0.5 + 1e-9)
  # closed interval: endpoints that land on samples are included
  expect_equal(min(ta), 0.30); expect_equal(max(ta), 0.50)
  # the clock survives and the offsets now point at the retained first samples
  expect_equal(commonClock(w)$t0, commonClock(m)$t0)
  expect_equal(unname(commonClock(w)$offsets[["a"]]), 0.30)
  expect_false(commonClock(w)$offsets_assumed)
})

test_that("windows on endpoints, between endpoints and partially overlapping", {
  m <- MultiPhysioExperiment(streams = list(a = mk(11, 10)))  # times 0, .1 ... 1.0
  expect_equal(streamTimeIndex(timeWindow(m, 0.2, 0.4), "a"), c(0.2, 0.3, 0.4))
  # bounds strictly between samples select the samples inside them
  expect_equal(streamTimeIndex(timeWindow(m, 0.25, 0.45), "a"), c(0.3, 0.4))
  # a window running past the end keeps what exists
  expect_equal(max(streamTimeIndex(timeWindow(m, 0.9, 5), "a")), 1.0)
})

test_that("a non-overlapping stream fails loudly and can be dropped on purpose", {
  m <- MultiPhysioExperiment(streams = list(a = mk(11, 10), b = mk(11, 10)),
                             offsets = c(a = 0, b = 10))
  expect_error(timeWindow(m, 0, 0.5), "\\bb\\b")
  w <- timeWindow(m, 0, 0.5, drop_empty = TRUE)
  expect_equal(streamNames(w), "a")
  expect_true("b" %in% names(commonClock(w)$dropped_streams))
})

test_that("windowing does not modify the original object", {
  m <- MultiPhysioExperiment(streams = list(a = mk(11, 10)))
  before <- streamTimeIndex(m, "a")
  invisible(timeWindow(m, 0.2, 0.4))
  expect_equal(streamTimeIndex(m, "a"), before)
})

test_that("bad windows are rejected", {
  m <- MultiPhysioExperiment(streams = list(a = mk(11, 10)))
  expect_error(timeWindow(m, 0.5, 0.2), "must not be greater")
  expect_error(timeWindow(m, NA_real_, 1), "finite numeric")
  expect_error(timeWindow(m, 0, 1, streams = "zz"), "unknown stream")
})

test_that("`[` keeps its documented meaning", {
  m <- MultiPhysioExperiment(streams = list(a = mk(11, 10), b = mk(11, 10)))
  expect_equal(streamNames(m[, "b"]), "b")
  expect_equal(streamTimeIndex(m[c(0.2, 0.4), ], "a"), c(0.2, 0.3, 0.4))
  expect_equal(streamNames(m[c(0.2, 0.4), "a"]), "a")
  expect_error(m[c(1, 2, 3), ], "length 2")
})

# ---- time alignment onto a common grid -------------------------------------

test_that("interpolation reproduces a hand-calculable result", {
  # a constant stream must stay constant; a linear ramp must stay linear
  const <- PhysioExperiment(assays = list(raw = matrix(7, 11, 1)), samplingRate = 10)
  ramp  <- PhysioExperiment(assays = list(raw = matrix(seq(0, 10, by = 1), 11, 1)),
                            samplingRate = 10)
  m <- MultiPhysioExperiment(streams = list(k = const, r = ramp))
  out <- resampleToCommon(m, 20)
  a <- SummarizedExperiment::assay(out, "aligned")
  expect_true(all(a[, "k.ch1"] == 7))
  # the ramp is 1 unit per 0.1 s, so 0.5 units per 0.05 s grid step
  expect_equal(as.numeric(a[, "r.ch1"]), seq(0, 10, by = 0.5), tolerance = 1e-9)
})

test_that("grid positions outside a stream's coverage are NA, and offsets shift it", {
  a <- PhysioExperiment(assays = list(raw = matrix(1, 11, 1)), samplingRate = 10)
  b <- PhysioExperiment(assays = list(raw = matrix(2, 11, 1)), samplingRate = 10)
  m <- MultiPhysioExperiment(streams = list(a = a, b = b), offsets = c(a = 0, b = 1))
  out <- resampleToCommon(m, 10)
  mat <- SummarizedExperiment::assay(out, "aligned")
  # grid runs 0..2 s; b only covers 1..2 s
  expect_true(all(is.na(mat[1:10, "b.ch1"])))
  expect_true(all(!is.na(mat[11:21, "b.ch1"])))
  expect_true(all(is.na(mat[12:21, "a.ch1"])))
})

# ---- legacy API -------------------------------------------------------------

test_that("the legacy CrossModal accessors return their original types", {
  m <- MultiPhysioExperiment(streams = list(EEG = mk(250, 250), EMG = mk(1000, 1000)),
                             offsets = c(EEG = 0, EMG = 0.05))
  expect_type(experiments(m), "list")
  expect_false(methods::is(experiments(m), "SimpleList"))
  expect_equal(modalities(m), c("EEG", "EMG"))
  expect_equal(samplingRates(m), c(EEG = 250, EMG = 1000))
  expect_equal(nModalities(m), 2L)

  al <- alignment(m)
  expect_s4_class(al, "DataFrame")
  expect_equal(as.character(al$modality), c("EEG", "EMG"))
  expect_equal(as.numeric(al$offset), c(0, 0.05))
  expect_equal(as.numeric(al$samplingRate), c(250, 1000))
})

test_that("alignment is a view: assigning to it writes into the one clock", {
  m <- MultiPhysioExperiment(streams = list(a = mk(10, 100), b = mk(10, 250)))
  alignment(m) <- S4Vectors::DataFrame(modality = c("a", "b"),
                                       samplingRate = c(100, 250),
                                       offset = c(0, 0.75))
  expect_equal(unname(commonClock(m)$offsets[["b"]]), 0.75)
  expect_equal(as.numeric(alignment(m)$offset), c(0, 0.75))
  expect_false(commonClock(m)$offsets_assumed)
  # and it refuses a table that contradicts the streams
  expect_error(
    `alignment<-`(m, S4Vectors::DataFrame(modality = c("a", "b"),
                                          samplingRate = c(100, 1), offset = c(0, 0))),
    "disagrees with the streams")
})

test_that("the legacy constructor and class still work", {
  mr <- MultiRatePhysioExperiment(a = mk(10, 100), b = mk(10, 250))
  expect_s4_class(mr, "MultiRatePhysioExperiment")
  expect_true(methods::is(mr, "MultiPhysioExperiment"))
  expect_equal(nStreams(mr), 2L)
  expect_equal(modalities(mr), c("a", "b"))
  # legacy `experiments =` spelling
  m2 <- MultiPhysioExperiment(experiments = list(a = mk(10, 100)))
  expect_equal(streamNames(m2), "a")
})

test_that("round-tripping through RDS preserves data and time", {
  m <- MultiPhysioExperiment(streams = list(a = mk(10, 100), b = mk(25, 250)),
                             t0 = 500, offsets = c(a = 0, b = -0.3))
  f <- tempfile(fileext = ".rds"); saveRDS(m, f); back <- readRDS(f)
  expect_true(methods::validObject(back))
  expect_equal(commonClock(back)$t0, 500)
  expect_equal(unname(commonClock(back)$offsets), c(0, -0.3))
  expect_identical(SummarizedExperiment::assay(back[["b"]], "raw"),
                   SummarizedExperiment::assay(m[["b"]], "raw"))
  expect_equal(streamTimeIndex(back, "b"), streamTimeIndex(m, "b"))
})

# ---- connection to the hierarchical containers ------------------------------

test_that("the hierarchy accepts the canonical container and the legacy one", {
  canonical <- MultiPhysioExperiment(a = mk(10, 100))
  legacy <- MultiRatePhysioExperiment(a = mk(10, 100))
  pl <- PhysioLongitudinal(
    sessions = list(s1 = canonical, s2 = legacy),
    design = S4Vectors::DataFrame(session_id = c("s1", "s2"),
                                  visit_label = c("baseline", "followup"),
                                  days_from_baseline = c(0, 90)))
  expect_equal(length(sessions(pl)), 2L)
  coh <- PhysioCohort(subjects = list(P01 = pl))
  expect_equal(nSubjects(coh), 1L)
  expect_true(methods::validObject(coh))
})

test_that("same session ids under different subjects are not confused", {
  mkses <- function(v) PhysioLongitudinal(
    sessions = list(base = MultiPhysioExperiment(
      a = PhysioExperiment(assays = list(raw = matrix(v, 10, 1)), samplingRate = 10))),
    design = S4Vectors::DataFrame(session_id = "base", visit_label = "baseline",
                                  days_from_baseline = 0))
  coh <- PhysioCohort(subjects = list(P01 = mkses(1), P02 = mkses(2)))
  s1 <- sessions(subject(coh, "P01"))[["base"]]
  s2 <- sessions(subject(coh, "P02"))[["base"]]
  expect_equal(SummarizedExperiment::assay(s1[["a"]], "raw")[1, 1], 1)
  expect_equal(SummarizedExperiment::assay(s2[["a"]], "raw")[1, 1], 2)
})

# ---- migration of pre-unification containers --------------------------------
# These build the legacy SLOT LAYOUT directly. The end-to-end check against a
# real file saved by the old code lives in
# publication/scripts/prove_container_migration.R, run against frozen fixtures.

as_legacy_layout <- function(streams, alignment, sampleMap, cache) {
  o <- MultiPhysioExperiment(streams = streams)
  attributes(o) <- list(class = attr(o, "class"),
                        experiments = streams, alignment = alignment,
                        sampleMap = sampleMap, couplingResults = cache)
  o
}

test_that("a legacy container is detected and converted without loss", {
  s <- list(EMG = mk(100, 1000), Motion = mk(20, 100))
  al <- S4Vectors::DataFrame(modality = c("EMG", "Motion"),
                             samplingRate = c(1000, 100), offset = c(0, 0.137))
  sm <- S4Vectors::DataFrame(assay = "EMG", primary = "s1", colname = "c1")
  old <- as_legacy_layout(s, al, sm, list(key1 = list(method = "coherence")))

  m <- migrateContainer(old, verbose = FALSE)
  expect_s4_class(m, "MultiPhysioExperiment")
  expect_true(methods::validObject(m))
  expect_equal(streamNames(m), c("EMG", "Motion"))
  expect_equal(unname(commonClock(m)$offsets), c(0, 0.137))
  expect_identical(SummarizedExperiment::assay(m[["EMG"]], "raw"),
                   SummarizedExperiment::assay(s$EMG, "raw"))
})

test_that("migration refuses to invent an origin and to reuse the old cache", {
  s <- list(a = mk(10, 100))
  al <- S4Vectors::DataFrame(modality = "a", samplingRate = 100, offset = 0)
  old <- as_legacy_layout(s, al, S4Vectors::DataFrame(x = 1),
                          list(k = list(v = 1)))
  m <- migrateContainer(old, verbose = FALSE)

  expect_true(is.na(commonClock(m)$t0))          # not fabricated as 0
  mf <- commonClock(m)$migrated_from
  expect_equal(NROW(mf$sampleMap), 1L)           # set aside, not dropped
  expect_equal(length(mf$couplingResults), 1L)
  expect_match(mf$cache_reuse, "not reused")
})

test_that("a legacy alignment that contradicts the data is reported", {
  s <- list(a = mk(10, 100))
  al <- S4Vectors::DataFrame(modality = "a", samplingRate = 999, offset = 0)
  m <- migrateContainer(as_legacy_layout(s, al, S4Vectors::DataFrame(),
                                         list()), verbose = FALSE)
  expect_match(commonClock(m)$migrated_from$rate_mismatch, "table says 999")
})

test_that("updateObject dispatches and current objects pass through unchanged", {
  s <- list(a = mk(10, 100))
  al <- S4Vectors::DataFrame(modality = "a", samplingRate = 100, offset = 0)
  old <- as_legacy_layout(s, al, S4Vectors::DataFrame(), list())
  expect_s4_class(updateObject(old), "MultiPhysioExperiment")

  current <- MultiPhysioExperiment(a = mk(10, 100))
  expect_identical(migrateContainer(current, verbose = FALSE), current)
  expect_identical(updateObject(current), current)
})

test_that("readPhysioRDS migrates on read and never rewrites the file", {
  s <- list(a = mk(10, 100))
  al <- S4Vectors::DataFrame(modality = "a", samplingRate = 100, offset = 0.25)
  old <- as_legacy_layout(s, al, S4Vectors::DataFrame(), list())
  f <- tempfile(fileext = ".rds"); saveRDS(old, f)
  before <- tools::md5sum(f)

  m <- readPhysioRDS(f, verbose = FALSE)
  expect_s4_class(m, "MultiPhysioExperiment")
  expect_equal(unname(commonClock(m)$offsets), 0.25)
  expect_equal(tools::md5sum(f), before)         # the source file is untouched

  # opting out must warn rather than hand back an invalid object quietly
  expect_warning(readPhysioRDS(f, migrate = FALSE), "pre-unification")
})

# ---- regressions for the 2026-09-26 review ---------------------------------
# Each block reproduces a defect the existing suite did not detect.

test_that("duplicate offset names are rejected before reordering hides them", {
  a <- mk(10, 100); b <- mk(10, 100)
  expect_error(
    MultiPhysioExperiment(streams = list(a = a, b = b), offsets = c(a = 0, a = 1, b = 0)),
    "names a stream more than once")
  expect_error(
    MultiPhysioExperiment(streams = list(a = a, b = b),
                          alignment = S4Vectors::DataFrame(
                            modality = c("a", "a", "b"), offset = c(0, 1, 0))),
    "more than one row for")
})

test_that("a window does not turn an assumed start into a measured one", {
  m <- MultiPhysioExperiment(a = mk(100, 100))
  expect_true(commonClock(m)$offsets_assumed)
  expect_true(commonClock(timeWindow(m, 0.1, 0.2))$offsets_assumed)

  measured <- MultiPhysioExperiment(streams = list(a = mk(100, 100)), offsets = c(a = 0))
  expect_false(commonClock(timeWindow(measured, 0.1, 0.2))$offsets_assumed)
})

test_that("selection keeps what the clock carried beyond its core fields", {
  m <- MultiPhysioExperiment(a = mk(100, 100), b = mk(100, 100))
  m@clock$source_id <- "rec-1"
  m@clock$migrated_from <- list(note = "kept")
  expect_equal(commonClock(timeWindow(m, 0.1, 0.2))$source_id, "rec-1")
  expect_equal(commonClock(m[, "a"])$source_id, "rec-1")
  expect_equal(commonClock(m[, "a"])$migrated_from$note, "kept")
  expect_equal(unname(commonClock(timeWindow(m, 0.1, 0.2))$selected_window), c(0.1, 0.2))
})

test_that("measured sample times are authoritative, not reconstructed", {
  # a stream with a gap: samples 4 and 5 are a second later than a regular
  # grid would put them
  pe <- PhysioExperiment(assays = list(raw = matrix(1:5, ncol = 1)),
                         samplingRate = 100,
                         rowData = S4Vectors::DataFrame(
                           time_from_t0 = c(0, 0.01, 0.02, 1.03, 1.04)))
  m <- MultiPhysioExperiment(a = pe)
  expect_true(hasMeasuredTimes(m, "a"))
  expect_equal(streamTimeIndex(m, "a"), c(0, 0.01, 0.02, 1.03, 1.04))
  # the window 0.03-0.04 contains no sample; it must not return the 1.03 ones
  expect_error(timeWindow(m, 0.03, 0.04), "fall in")
  expect_equal(streamTimeIndex(timeWindow(m, 1.0, 1.1), "a"), c(1.03, 1.04))

  regular <- MultiPhysioExperiment(a = mk(10, 100))
  expect_false(hasMeasuredTimes(regular, "a"))
})

test_that("events follow the window and the originals are kept as history", {
  pe <- setEvents(mk(400, 100),
                  PhysioEvents(onset = c(0.5, 1.5, 3), duration = c(0, 0, 0),
                               type = rep("stim", 3), value = rep("v", 3)))
  w <- timeWindow(MultiPhysioExperiment(a = pe), 1, 2)
  kept <- w[["a"]]
  expect_equal(nEvents(kept), 1L)
  # the retained event was at 1.5 s; the window starts at 1.0 s
  expect_equal(as.data.frame(getEvents(kept)@events)$onset, 0.5)
  hist <- S4Vectors::metadata(kept)$events_before_window
  expect_equal(nrow(hist$events), 3L)
  expect_equal(hist$window, c(1, 2))
})

test_that("migration refuses an incomplete legacy clock instead of zero-filling", {
  s <- list(a = mk(10, 100), b = mk(10, 100))
  full <- S4Vectors::DataFrame(modality = c("a", "b"),
                               samplingRate = c(100, 100), offset = c(0, 0.137))
  ok <- migrateContainer(as_legacy_layout(s, full, S4Vectors::DataFrame(), list()),
                         verbose = FALSE)
  expect_equal(unname(commonClock(ok)$offsets), c(0, 0.137))

  partial <- full[full$modality != "b", ]
  expect_error(
    migrateContainer(as_legacy_layout(s, partial, S4Vectors::DataFrame(), list()),
                     verbose = FALSE),
    "does not describe its own streams")
  dup <- S4Vectors::DataFrame(modality = c("a", "a", "b"),
                              samplingRate = c(100, 100, 100), offset = c(0, 1, 0))
  expect_error(
    migrateContainer(as_legacy_layout(s, dup, S4Vectors::DataFrame(), list()),
                     verbose = FALSE),
    "more than one row for")
})

test_that("resampleToCommon refuses a stream it cannot place on a regular grid", {
  # it interpolates from real times, so an irregular stream is handled by its
  # measured times rather than a fabricated grid
  pe <- PhysioExperiment(assays = list(raw = matrix(as.numeric(1:5), ncol = 1)),
                         samplingRate = 100,
                         rowData = S4Vectors::DataFrame(
                           time_from_t0 = c(0, 0.01, 0.02, 1.03, 1.04)))
  m <- MultiPhysioExperiment(a = pe)
  out <- resampleToCommon(m, 100)
  expect_true(nrow(SummarizedExperiment::assay(out, "aligned")) > 100)
})
