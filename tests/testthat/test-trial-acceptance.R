# Acceptance tests for CORE-TRIAL-01, written BEFORE the implementation.
#
# Each block encodes one row of the spec's acceptance table (section 4) and the
# decisions settled in publication/results/package_reorganization/
# TRIAL_EVALUATION.md. They are the contract the implementation has to satisfy,
# so they are written against the intended API rather than against whatever
# exists.
#
# Until that API lands the whole file skips, and each test turns on by itself as
# the function it needs appears. The guards name the missing function so a
# partial implementation reports precisely what is still absent.

have <- function(...) {
  fns <- c(...)
  missing <- fns[!vapply(fns, exists, logical(1), where = asNamespace("PhysioExperiment"))]
  if (length(missing)) {
    skip(paste("not implemented yet:", paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

# ---- fixtures ---------------------------------------------------------------

# a regular stream: 10 Hz, 0 .. 9.9 s
reg_stream <- function(n = 100, sr = 10) {
  PhysioExperiment(assays = list(raw = matrix(seq_len(n) * 1.0, ncol = 1)),
                   samplingRate = sr)
}

# a stream with measured times and a one-second hole between 1.9 and 3.0
gapped_stream <- function() {
  tt <- c(seq(0, 1.9, by = 0.1), seq(3.0, 4.9, by = 0.1))
  PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0, ncol = 1)),
                   samplingRate = 10,
                   rowData = S4Vectors::DataFrame(time_from_t0 = tt))
}

one_recording <- function() {
  MultiPhysioExperiment(streams = list(emg = reg_stream(), motion = reg_stream(50, 5)),
                        offsets = c(emg = 0, motion = 0))
}

# ---- coverage: observation vs derivation (TRIAL_EVALUATION 2.2) -------------

test_that("coverage separates what is observed from what is derived", {
  have("streamCoverage")
  m <- MultiPhysioExperiment(a = gapped_stream())
  cv <- streamCoverage(m, "a", 0, 4.9)

  # observed
  expect_equal(cv$n_present, 40L)
  # derived under the stated rule: floor((end - start) * rate) + 1
  expect_equal(cv$n_expected, 50L)
  expect_equal(cv$coverage, 40 / 50)
  expect_equal(cv$rule$name, "nominal")
  expect_equal(cv$rule$gap_factor, 1.5)
  # the hole is reported as an interval, not merely as a count shortfall
  expect_equal(nrow(cv$gaps), 1L)
  expect_equal(cv$gaps$start, 1.9)
  expect_equal(cv$gaps$end, 3.0)
})

test_that("an undecidable expected count is NA, not a guess", {
  have("streamCoverage")
  m <- MultiPhysioExperiment(a = gapped_stream())
  cv <- streamCoverage(m, "a", 0, 4.9, rule = "unknown")
  expect_true(is.na(cv$n_expected))
  expect_true(is.na(cv$coverage))
  expect_equal(cv$n_present, 40L)      # still observed
})

test_that("a regular stream reports no gaps, and says why", {
  have("streamCoverage")
  cv <- streamCoverage(MultiPhysioExperiment(a = reg_stream()), "a", 0, 9.9)
  expect_equal(nrow(cv$gaps), 0L)
  # "no gaps" here follows from the reconstruction rule; it is not a measurement
  expect_match(cv$rule$basis, "reconstructed")
})

test_that("coverage is not served stale after the data moves under it", {
  have("streamCoverage", "trialIntervals")
  m <- MultiPhysioExperiment(a = gapped_stream())
  before <- streamCoverage(m, "a", 0, 4.9)
  w <- timeWindow(m, 3.0, 4.9)
  after <- streamCoverage(w, "a", 3.0, 4.9)
  expect_lt(after$n_present, before$n_present)
  expect_equal(nrow(after$gaps), 0L)   # the hole is outside the window now
})

# ---- the three tables and their keys (TRIAL_EVALUATION 2.1) -----------------

test_that("one trial can be recorded by several devices", {
  have("trials", "trials<-", "recordingMap", "recordingMap<-")
  m <- one_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), condition = c("fast", "slow"),
                          observed = TRUE, stringsAsFactors = FALSE)
  # the same trial on two devices: two rows, and this must be accepted
  recordingMap(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T1"),
    recording_id = c("R_emg", "R_motion"),
    time_correspondence = "measured", reference = "clock",
    offset = c(0, 0), stringsAsFactors = FALSE)

  expect_equal(nrow(trials(m)), 2L)
  expect_equal(sum(recordingMap(m)$trial_id == "T1"), 2L)
})

test_that("the trial table stays unique on the composite key", {
  have("trials<-")
  m <- one_recording()
  dup <- data.frame(subject_id = "P1", session_id = "S1", trial_id = c("T1", "T1"),
                    observed = TRUE, stringsAsFactors = FALSE)
  expect_error(`trials<-`(m, dup), "unique|duplicate")
})

test_that("trial_id is not assumed globally unique", {
  have("trials<-")
  m <- one_recording()
  # the same trial label under two subjects is legitimate
  ok <- data.frame(subject_id = c("P1", "P2"), session_id = "S1",
                   trial_id = c("T1", "T1"), observed = TRUE,
                   stringsAsFactors = FALSE)
  expect_silent(trials(m) <- ok)
  expect_equal(nrow(trials(m)), 2L)
})

# ---- storable vs invalid (TRIAL_EVALUATION 2.3) -----------------------------

test_that("a trial that contains a gap is stored, not rejected", {
  have("trials<-", "trialIntervals<-", "streamCoverage")
  m <- MultiPhysioExperiment(a = gapped_stream())
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                          observed = TRUE, stringsAsFactors = FALSE)
  # 0 .. 4.9 spans the hole; it happened, so it is recorded
  expect_silent(trialIntervals(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0, end = 4.9,
    stringsAsFactors = FALSE))
  expect_equal(nrow(trialIntervals(m)), 1L)
  expect_equal(nrow(streamCoverage(m, "a", 0, 4.9)$gaps), 1L)
})

test_that("invalid intervals are refused", {
  have("trialIntervals<-")
  m <- one_recording()
  base <- data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                     recording_id = "R1", stream = "emg", start = 0, end = 1,
                     stringsAsFactors = FALSE)
  bad <- function(f) { d <- base; d <- f(d); d }

  expect_error(`trialIntervals<-`(m, value = bad(function(d) { d$start <- 2; d })),
               "start|greater")                                   # reversed
  expect_error(`trialIntervals<-`(m, value = bad(function(d) { d$end <- Inf; d })),
               "finite")                                          # non-finite
  expect_error(`trialIntervals<-`(m, value = bad(function(d) { d$stream <- "zz"; d })),
               "unknown|exist")                                   # dangling stream
  expect_error(`trialIntervals<-`(m, value = rbind(base, base)),
               "unique|duplicate")                                # duplicate key
  expect_error(`trialIntervals<-`(m, value = bad(function(d) { d$start <- 100; d$end <- 101; d })),
               "outside|range")                                   # wholly outside
})

test_that("a trial outside the recording is carried as unobserved, not as an interval", {
  have("trials<-")
  m <- one_recording()
  expect_silent(trials(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T0",
    observed = FALSE, stringsAsFactors = FALSE))
  expect_false(trials(m)$observed[1])
})

# ---- extraction and what happens to the tables (spec 3.6) -------------------

test_that("trial extraction agrees with the equivalent timeWindow call", {
  have("trials<-", "trialIntervals<-", "trial")
  m <- one_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                          observed = TRUE, stringsAsFactors = FALSE)
  trialIntervals(m) <- data.frame(subject_id = "P1", session_id = "S1",
                                  trial_id = "T1", recording_id = "R1",
                                  stream = "emg", start = 2, end = 4,
                                  stringsAsFactors = FALSE)
  got <- trial(m, "T1")
  ref <- timeWindow(m, 2, 4)
  expect_equal(streamTimeIndex(got, "emg"), streamTimeIndex(ref, "emg"))
  expect_equal(commonClock(got)$t0, commonClock(m)$t0)
})

test_that("a window keeps partial trials by default, truncated and marked", {
  have("trials<-", "trialIntervals<-", "trialIntervals")
  m <- one_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2", "T3"), observed = TRUE,
                          stringsAsFactors = FALSE)
  trialIntervals(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2", "T3"),
    recording_id = "R1", stream = "emg",
    start = c(0, 2.5, 8), end = c(1, 4.5, 9), stringsAsFactors = FALSE)

  w <- timeWindow(m, 3, 6)                    # cuts T2, excludes T1 and T3
  tb <- trialIntervals(w)
  expect_equal(tb$trial_id, "T2")
  expect_equal(tb$start, 3)                   # truncated to the window
  expect_true(tb$truncated_start)
  expect_false(tb$truncated_end)
  expect_equal(tb$original_start, 2.5)        # the fact is not erased
  expect_true(all(c("T1", "T3") %in% commonClock(w)$dropped_trials$trial_id))
})

test_that("complete_only keeps whole trials and records what it dropped", {
  have("trials<-", "trialIntervals<-", "trialIntervals")
  m <- one_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), observed = TRUE,
                          stringsAsFactors = FALSE)
  trialIntervals(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = "R1", stream = "emg", start = c(4.0, 2.5), end = c(5.0, 3.5),
    stringsAsFactors = FALSE)

  w <- timeWindow(m, 3, 6, complete_only = TRUE)
  expect_equal(trialIntervals(w)$trial_id, "T1")
  excl <- commonClock(w)$dropped_trials
  expect_true("T2" %in% excl$trial_id)
  expect_match(excl$reason[excl$trial_id == "T2"], "partial|incomplete")
})

test_that("a stale trial table is not carried through an operation that voids it", {
  have("trials<-", "trialIntervals<-", "trialIntervals")
  m <- one_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                          observed = TRUE, stringsAsFactors = FALSE)
  trialIntervals(m) <- data.frame(subject_id = "P1", session_id = "S1",
                                  trial_id = "T1", recording_id = "R1",
                                  stream = "emg", start = 0, end = 3,
                                  stringsAsFactors = FALSE)
  # this was the decisive finding of the evaluation: ancillary information is
  # preserved blindly, so the table must be updated, not merely carried
  w <- timeWindow(m, 5, 9)
  expect_equal(nrow(trialIntervals(w)), 0L)
  expect_true("T1" %in% commonClock(w)$dropped_trials$trial_id)
})

# ---- time correspondence between recordings (spec 3.3) ---------------------

test_that("measured needs a reference and an offset, not just the state name", {
  have("recordingMap<-", "elapsedBetweenTrials")
  m <- one_recording()
  incomplete <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = c("R1", "R2"), time_correspondence = "measured",
    reference = NA_character_, offset = NA_real_, stringsAsFactors = FALSE)
  recordingMap(m) <- incomplete
  expect_error(elapsedBetweenTrials(m, "T1", "T2"), "reference|offset")
})

test_that("measured with a reference and offset yields the elapsed time", {
  have("recordingMap<-", "elapsedBetweenTrials")
  m <- one_recording()
  recordingMap(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = c("R1", "R2"), time_correspondence = "measured",
    reference = "trigger", offset = c(0, 12.5), stringsAsFactors = FALSE)
  expect_equal(elapsedBetweenTrials(m, "T1", "T2"), 12.5)
})

test_that("order_only and unknown refuse elapsed time, with a reason", {
  have("recordingMap<-", "elapsedBetweenTrials")
  m <- one_recording()
  for (state in c("order_only", "unknown")) {
    recordingMap(m) <- data.frame(
      subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
      recording_id = c("R1", "R2"), time_correspondence = state,
      reference = NA_character_, offset = NA_real_, stringsAsFactors = FALSE)
    expect_error(elapsedBetweenTrials(m, "T1", "T2"), state, info = state)
  }
})

test_that("recordings on different references are refused unless convertible", {
  have("recordingMap<-", "elapsedBetweenTrials")
  m <- one_recording()
  recordingMap(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = c("R1", "R2"), time_correspondence = "measured",
    reference = c("trigger_a", "trigger_b"), offset = c(0, 5),
    stringsAsFactors = FALSE)
  expect_error(elapsedBetweenTrials(m, "T1", "T2"), "reference")
})

test_that("order is never turned into elapsed time", {
  have("recordingMap<-", "elapsedBetweenTrials")
  m <- one_recording()
  recordingMap(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2", "T3"),
    recording_id = c("R1", "R2", "R3"), time_correspondence = "order_only",
    reference = NA_character_, offset = NA_real_, stringsAsFactors = FALSE)
  # no 0, 1, 2 may be invented from the row order
  expect_error(elapsedBetweenTrials(m, "T1", "T3"), "order_only")
})

test_that("an absent absolute origin does not block relative extraction", {
  # already true today; the trial work must not regress it
  m <- MultiPhysioExperiment(streams = list(a = reg_stream()), t0 = NA_real_)
  w <- timeWindow(m, 1, 2)
  expect_true(is.na(commonClock(w)$t0))
  expect_equal(length(streamTimeIndex(w, "a")), 11L)
})

# ---- aggregation (TRIAL_EVALUATION 2.4) ------------------------------------

test_that("partial trials are not silently averaged with complete ones", {
  have("aggregateTrials")
  m <- one_recording()
  # per-trial scalar features live on the trial table
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), observed = TRUE,
                          amplitude = c(10, 30), stringsAsFactors = FALSE)
  trialIntervals(m) <- data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = "R1", stream = "emg",
    start = c(4.0, 2.5), end = c(5.0, 3.5), stringsAsFactors = FALSE)
  # the window leaves T1 whole and cuts T2, so T2 becomes partial. The spans do
  # not overlap each other: overlap is refused unless asked for, and is tested
  # separately in test-trial-remaining.R.
  w <- timeWindow(m, 3, 6)

  out <- aggregateTrials(w, level = "session", features = "amplitude")
  expect_equal(out$counts$n_included, out$counts$n_complete)
  expect_gt(out$counts$n_excluded, 0L)
  expect_match(out$excluded$reason[1], "partial")
  # the default average is of the complete trial alone
  expect_equal(out$aggregate$data$amplitude, 10)

  both <- aggregateTrials(w, level = "session", features = "amplitude",
                          include = c("complete", "partial"))
  expect_gt(both$counts$n_included, out$counts$n_included)
  expect_equal(both$aggregate$data$amplitude, 20)   # (10 + 30) / 2
})

test_that("the existing aggregation is untouched by any of this", {
  # the numbers, the weighting and the checksum of aggregatePhysioFeatures() are
  # not part of this change
  x <- data.frame(subject_id = c("P1", "P1", "P1", "P2"),
                  session_id = c("S1", "S1", "S2", "S1"),
                  trial_id = c("T1", "T2", "T1", "T1"),
                  cycle_id = "C1", amplitude = c(1, 3, 5, 9))
  y <- aggregatePhysioFeatures(x, "session", "amplitude")
  expect_equal(y$data$amplitude, c(2, 5, 9))
  expect_equal(y$source_rows[[1]], 1:2)
  expect_false(is.null(y$checksum))
})
