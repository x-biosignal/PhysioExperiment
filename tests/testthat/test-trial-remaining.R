# Acceptance tests for the three items CORE-TRIAL-01 left open, written before
# the implementation. TRIAL_SCOPE.md "What remains":
#   * overlap validation between intervals, and cross-stream checking of a span
#   * trial-aware queries on PhysioLongitudinal / PhysioCohort
#   * how several independent recordings are held together
#
# Waveform averaging stays out: the spec excludes it as its own problem.

have2 <- function(...) {
  fns <- c(...)
  miss <- fns[!vapply(fns, exists, logical(1), where = asNamespace("PhysioExperiment"))]
  if (length(miss)) skip(paste("not implemented yet:", paste(miss, collapse = ", ")))
  invisible(TRUE)
}

rs <- function(n = 100, sr = 10) {
  PhysioExperiment(assays = list(raw = matrix(seq_len(n) * 1.0, ncol = 1)),
                   samplingRate = sr)
}
gapped <- function() {
  tt <- c(seq(0, 1.9, by = 0.1), seq(3.0, 4.9, by = 0.1))
  PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0, ncol = 1)),
                   samplingRate = 10,
                   rowData = S4Vectors::DataFrame(time_from_t0 = tt))
}
two_streams <- function() {
  MultiPhysioExperiment(streams = list(emg = rs(), motion = gapped()),
                        offsets = c(emg = 0, motion = 0))
}
iv <- function(...) {
  base <- data.frame(subject_id = "P1", session_id = "S1", recording_id = "R1",
                     stringsAsFactors = FALSE)
  d <- data.frame(..., stringsAsFactors = FALSE)
  cbind(base[rep(1, nrow(d)), , drop = FALSE], d, row.names = NULL)
}

# ---- 1. overlap validation --------------------------------------------------

test_that("overlapping intervals on the same stream are refused by default", {
  have2("trialIntervals<-")
  m <- two_streams()
  overlapping <- iv(trial_id = c("T1", "T2"), stream = "emg",
                    start = c(0, 1.5), end = c(2, 3))
  expect_error(`trialIntervals<-`(m, value = overlapping), "overlap")
})

test_that("overlap is allowed when the caller says so, and the reason is kept", {
  have2("trialIntervals<-", "trialIntervals")
  m <- two_streams()
  overlapping <- iv(trial_id = c("T1", "T2"), stream = "emg",
                    start = c(0, 1.5), end = c(2, 3))
  trialIntervals(m, allow_overlap = "trials share a settling period") <- overlapping
  expect_equal(nrow(trialIntervals(m)), 2L)
  expect_match(attr(trialIntervals(m), "overlap_allowed"), "settling period")
})

test_that("intervals that merely touch are not an overlap", {
  have2("trialIntervals<-")
  m <- two_streams()
  # T1 ends exactly where T2 begins; a closed interval shares one instant, and
  # treating that as an overlap would make back-to-back trials unrepresentable
  touching <- iv(trial_id = c("T1", "T2"), stream = "emg",
                 start = c(0, 2), end = c(2, 4))
  expect_silent(trialIntervals(m) <- touching)
})

test_that("the same span on different streams or recordings is not an overlap", {
  have2("trialIntervals<-")
  m <- two_streams()
  same_span <- iv(trial_id = "T1", stream = c("emg", "motion"),
                  start = 0, end = 2)
  expect_silent(trialIntervals(m) <- same_span)
})

# ---- 2. cross-stream checking of a trial's span -----------------------------

test_that("a trial's coverage is reported for every stream it names", {
  have2("trialCoverage", "trialIntervals<-")
  m <- two_streams()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                          observed = TRUE, stringsAsFactors = FALSE)
  trialIntervals(m) <- iv(trial_id = "T1", stream = c("emg", "motion"),
                          start = 0, end = 4.9)
  cv <- trialCoverage(m, "T1")

  expect_setequal(cv$stream, c("emg", "motion"))
  # emg is regular and fully covers the span; motion has the one-second hole
  expect_equal(cv$n_gaps[cv$stream == "emg"], 0L)
  expect_equal(cv$n_gaps[cv$stream == "motion"], 1L)
  expect_lt(cv$coverage[cv$stream == "motion"], cv$coverage[cv$stream == "emg"])
})

test_that("cross-stream disagreement about a span is surfaced, not hidden", {
  have2("trialCoverage", "trialIntervals<-")
  m <- two_streams()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                          observed = TRUE, stringsAsFactors = FALSE)
  trialIntervals(m) <- iv(trial_id = "T1", stream = c("emg", "motion"),
                          start = 0, end = 4.9)
  cv <- trialCoverage(m, "T1")
  # one stream covering materially less of the same trial is the fact a caller
  # needs in order to decide whether a cross-modal comparison is defensible
  expect_true(any(cv$coverage < 1))
  expect_false(is.null(attr(cv, "consistent")))
  expect_false(attr(cv, "consistent"))
})

# ---- 3. trial-aware queries on the hierarchy --------------------------------

test_that("every trial of a subject can be listed across sessions", {
  have2("subjectTrials")
  mk <- function(tid) {
    m <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
    trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                            trial_id = tid, observed = TRUE,
                            stringsAsFactors = FALSE)
    trialIntervals(m) <- data.frame(subject_id = "P1", session_id = "S1",
                                    trial_id = tid, recording_id = "R1",
                                    stream = "emg", start = 0, end = 2,
                                    stringsAsFactors = FALSE)
    m
  }
  pl <- PhysioLongitudinal(
    sessions = list(base = mk("T1"), follow = mk("T2")),
    design = S4Vectors::DataFrame(session_id = c("base", "follow"),
                                  visit_label = c("baseline", "followup"),
                                  days_from_baseline = c(0, 90)))
  out <- subjectTrials(pl)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$trial_id, c("T1", "T2"))
  # the session each trial came from must be carried, or the rows are ambiguous
  expect_setequal(out$session, c("base", "follow"))
})

test_that("cohort-wide trials keep the subject they belong to", {
  have2("cohortTrials", "subjectTrials")
  mk <- function(sid, tid) {
    m <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
    trials(m) <- data.frame(subject_id = sid, session_id = "S1", trial_id = tid,
                            observed = TRUE, stringsAsFactors = FALSE)
    m
  }
  pl <- function(sid, tid) PhysioLongitudinal(
    sessions = list(base = mk(sid, tid)),
    design = S4Vectors::DataFrame(session_id = "base", visit_label = "baseline",
                                  days_from_baseline = 0))
  coh <- PhysioCohort(subjects = list(P01 = pl("P01", "T1"),
                                      P02 = pl("P02", "T1")))
  out <- cohortTrials(coh)
  expect_equal(nrow(out), 2L)
  # the same trial_id under two subjects must stay distinguishable
  expect_setequal(out$subject, c("P01", "P02"))
  expect_equal(length(unique(out$trial_id)), 1L)
})

test_that("a hierarchy with no trials returns an empty table, not an error", {
  have2("subjectTrials")
  pl <- PhysioLongitudinal(
    sessions = list(base = MultiPhysioExperiment(emg = rs())),
    design = S4Vectors::DataFrame(session_id = "base", visit_label = "baseline",
                                  days_from_baseline = 0))
  out <- subjectTrials(pl)
  expect_equal(nrow(out), 0L)
  expect_true(all(c("session", "trial_id") %in% names(out)))
})

# ---- 4. holding several independent recordings ------------------------------

test_that("independently recorded trials can be held together", {
  have2("trialSet", "recordings")
  a <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  b <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  ts <- trialSet(
    recordings = list(R1 = a, R2 = b),
    map = data.frame(subject_id = "P1", session_id = "S1",
                     trial_id = c("T1", "T2"), recording_id = c("R1", "R2"),
                     time_correspondence = "measured", reference = "trigger",
                     offset = c(0, 12.5), stringsAsFactors = FALSE))
  expect_equal(names(recordings(ts)), c("R1", "R2"))
  # the recordings are held, not merely named
  expect_s4_class(recordings(ts)[["R1"]], "MultiPhysioExperiment")
  expect_equal(nrow(recordingMap(ts)), 2L)
})

test_that("a map naming a recording that is not held is refused", {
  have2("trialSet")
  a <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  expect_error(
    trialSet(recordings = list(R1 = a),
             map = data.frame(subject_id = "P1", session_id = "S1",
                              trial_id = c("T1", "T2"),
                              recording_id = c("R1", "R_missing"),
                              time_correspondence = "measured",
                              reference = "trigger", offset = c(0, 1),
                              stringsAsFactors = FALSE)),
    "not held|R_missing")
})

test_that("elapsed time across held recordings uses their correspondence", {
  have2("trialSet", "elapsedBetweenTrials")
  a <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  b <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  ts <- trialSet(
    recordings = list(R1 = a, R2 = b),
    map = data.frame(subject_id = "P1", session_id = "S1",
                     trial_id = c("T1", "T2"), recording_id = c("R1", "R2"),
                     time_correspondence = "measured", reference = "trigger",
                     offset = c(0, 12.5), stringsAsFactors = FALSE))
  expect_equal(elapsedBetweenTrials(ts, "T1", "T2"), 12.5)

  ts2 <- trialSet(
    recordings = list(R1 = a, R2 = b),
    map = data.frame(subject_id = "P1", session_id = "S1",
                     trial_id = c("T1", "T2"), recording_id = c("R1", "R2"),
                     time_correspondence = "order_only",
                     reference = NA_character_, offset = NA_real_,
                     stringsAsFactors = FALSE))
  expect_error(elapsedBetweenTrials(ts2, "T1", "T2"), "order_only")
})

test_that("no concatenated time axis is fabricated across recordings", {
  have2("trialSet", "recordings")
  a <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  b <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  ts <- trialSet(
    recordings = list(R1 = a, R2 = b),
    map = data.frame(subject_id = "P1", session_id = "S1",
                     trial_id = c("T1", "T2"), recording_id = c("R1", "R2"),
                     time_correspondence = "order_only",
                     reference = NA_character_, offset = NA_real_,
                     stringsAsFactors = FALSE))
  # each recording keeps its own clock; the second does not start where the
  # first left off
  expect_equal(streamTimeIndex(recordings(ts)[["R1"]], "emg")[1], 0)
  expect_equal(streamTimeIndex(recordings(ts)[["R2"]], "emg")[1], 0)
})

test_that("a trial set round-trips through saveRDS", {
  have2("trialSet", "recordings")
  a <- MultiPhysioExperiment(streams = list(emg = rs()), offsets = c(emg = 0))
  ts <- trialSet(
    recordings = list(R1 = a),
    map = data.frame(subject_id = "P1", session_id = "S1", trial_id = "T1",
                     recording_id = "R1", time_correspondence = "measured",
                     reference = "trigger", offset = 0,
                     stringsAsFactors = FALSE))
  f <- tempfile(fileext = ".rds"); saveRDS(ts, f); back <- readRDS(f)
  expect_equal(names(recordings(back)), "R1")
  expect_equal(nrow(recordingMap(back)), 1L)
})

# ---------------------------------------------------------------------------
# A trial can be partial without any window having touched it: the interval may
# run past the end of the recording, or have a dropout inside it. Those were
# counted as complete and averaged in with whole trials, which is what this
# section fixes.
# ---------------------------------------------------------------------------

.ten_second_recording <- function(sr = 100) {
  p <- PhysioExperiment(assays = list(raw = matrix(seq_len(1000 * 2), 1000, 2)),
                        samplingRate = sr)
  MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))
}

test_that("an interval running past the end of the recording is partial", {
  m <- .ten_second_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), value = c(10, 20),
                          stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = "R1", stream = "a", start = c(0, 9.5), end = c(1, 12),
    stringsAsFactors = FALSE))

  # the package's own coverage report already says T2 is one fifth covered
  expect_lt(trialCoverage(m, "T2")$coverage, 0.25)
  expect_equal(trialCoverage(m, "T1")$coverage, 1)

  out <- aggregateTrials(m, level = "session", features = "value")
  expect_equal(out$counts$n_partial, 1L)
  expect_equal(out$counts$n_complete, 1L)
  expect_equal(out$counts$n_included, 1L)
  expect_match(out$excluded$reason[1], "partial")
  # the average is of the whole trial alone, not (10 + 20) / 2
  expect_equal(out$aggregate$data$value, 10)

  both <- aggregateTrials(m, level = "session", features = "value",
                          include = c("complete", "partial"))
  expect_equal(both$aggregate$data$value, 15)
  expect_false(identical(out$aggregate$checksum, both$aggregate$checksum))
})

test_that("a dropout inside the interval makes the trial partial", {
  tt <- c(seq(0, 1, by = 0.01), seq(2, 3, by = 0.01))
  p <- PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0,
                                                  length(tt), 1)),
                        samplingRate = 100,
                        rowData = S4Vectors::DataFrame(time_from_t0 = tt))
  m <- MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), value = c(10, 20),
                          stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = "R1", stream = "a", start = c(0, 0.5), end = c(0.5, 3),
    stringsAsFactors = FALSE))

  st <- aggregateTrials(m, level = "session", features = "value")$states
  expect_equal(st$state[st$trial_id == "T1"], "complete")
  expect_equal(st$state[st$trial_id == "T2"], "partial")
})

test_that("undecidable coverage is not reported as complete", {
  m <- .ten_second_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 10, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0, end = 1,
    stringsAsFactors = FALSE))

  # with no decidable sampling basis, coverage is NA -- so is completeness
  expect_true(is.na(streamCoverage(m, "a", 0, 1, rule = "unknown")$coverage))
  expect_error(aggregateTrials(m, level = "session", features = "value",
                               rule = "unknown"),
               "no trial satisfies")
  got <- aggregateTrials(m, level = "session", features = "value",
                         rule = "unknown", include = "unknown")
  expect_equal(got$counts$n_unknown, 1L)
  expect_equal(got$counts$n_complete, 0L)
})

test_that("a trial with no interval is counted as undescribed, not complete", {
  m <- .ten_second_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), value = c(10, 20),
                          stringsAsFactors = FALSE)

  # the default asks for demonstrated completeness, which no interval-less trial
  # can show; the decided rule is that they are taken only when named
  expect_error(aggregateTrials(m, level = "session", features = "value"),
               "no trial satisfies")
  out <- aggregateTrials(m, level = "session", features = "value",
                         include = "undescribed")
  expect_equal(out$counts$n_undescribed, 2L)
  expect_equal(out$counts$n_complete, 0L)
  expect_equal(out$aggregate$data$value, 15)
})

test_that("every trial gets exactly one state and the counts add up", {
  m <- .ten_second_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2", "T3"), value = c(1, 2, 3),
                          stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
    recording_id = "R1", stream = "a", start = c(0, 9.5), end = c(1, 12),
    stringsAsFactors = FALSE))

  out <- aggregateTrials(m, level = "session", features = "value",
                         include = c("complete", "partial", "undescribed"))
  expect_setequal(out$states$state, c("complete", "partial", "undescribed"))
  n <- out$counts
  expect_equal(n$n_complete + n$n_partial + n$n_unknown + n$n_undescribed, 3L)
  expect_equal(n$n_included + n$n_excluded, 3L)
})

# ---------------------------------------------------------------------------
# Two ways the completeness judgement was still wrong in 2.1.1. Both were found
# by reading the judgement against what streamCoverage() itself reports.
# ---------------------------------------------------------------------------

test_that("a gap counts even when the sample total does not reveal it", {
  # 10 Hz. One 0.2 s hole, and an extra off-grid sample at 0.45 that brings the
  # total back up to what a full second would hold. Counting samples alone
  # therefore says nothing is missing; the gap says otherwise.
  tt <- c(0, .1, .2, .4, .45, .5, .6, .7, .8, .9, 1)
  p <- PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0,
                                                  length(tt), 1)),
                        samplingRate = 10,
                        rowData = S4Vectors::DataFrame(time_from_t0 = tt))
  m <- MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))

  cv <- streamCoverage(m, "a", 0, 1)
  expect_equal(cv$n_present, 11L)
  expect_equal(nrow(cv$gaps), 1L)
  expect_equal(cv$gaps$start[1], 0.2)
  expect_equal(cv$gaps$end[1], 0.4)

  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0, end = 1,
    stringsAsFactors = FALSE))

  st <- aggregateTrials(m, level = "session", features = "value",
                        include = c("complete", "partial"))$states
  expect_equal(st$state, "partial")
  # and the default leaves it out
  expect_error(aggregateTrials(m, level = "session", features = "value"),
               "no trial satisfies")
})

test_that("the expected count follows the grid, not the span times the rate", {
  # A regular 10 Hz stream, 0 to 1 s, nothing missing. The window [0.01, 1.01]
  # holds ten of its samples, and ten is also how many grid points fall inside
  # it -- floor((end - start) * rate) + 1 would say eleven and call the stream
  # incomplete for being sampled off the window boundary.
  p <- PhysioExperiment(assays = list(raw = matrix(1:11 * 1.0, 11, 1)),
                        samplingRate = 10)
  m <- MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))

  cv <- streamCoverage(m, "a", 0.01, 1.01)
  expect_equal(cv$n_present, 10L)
  expect_equal(cv$n_expected, 10L)
  expect_equal(cv$coverage, 1)
  expect_equal(nrow(cv$gaps), 0L)

  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0.01, end = 1.01,
    stringsAsFactors = FALSE))
  expect_equal(aggregateTrials(m, level = "session",
                               features = "value")$states$state, "complete")

  # a window aligned on the samples still counts every one of them
  expect_equal(streamCoverage(m, "a", 0, 1)$n_expected, 11L)
  expect_equal(streamCoverage(m, "a", 0, 1)$coverage, 1)
  # and one that asks for more than the stream holds is still short
  short <- streamCoverage(m, "a", 0, 2)
  expect_equal(short$n_expected, 21L)
  expect_equal(short$n_present, 11L)
})

test_that("the reference grid behind the expected count is stated", {
  tt <- seq(0.37, 1.37, by = 0.1)
  p <- PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0,
                                                  length(tt), 1)),
                        samplingRate = 10,
                        rowData = S4Vectors::DataFrame(time_from_t0 = tt))
  m <- MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))
  cv <- streamCoverage(m, "a", 0.37, 1.37)
  # measured times: the assumed grid is named, anchored and stepped, so a caller
  # can see which grid the expected count was counted on
  expect_equal(cv$rule$anchor, 0.37)
  expect_equal(cv$rule$step, 0.1)
  expect_match(cv$rule$basis, "anchor")
  expect_equal(cv$n_expected, 11L)
  expect_equal(cv$coverage, 1)
})

test_that("more samples than the declared grid predicts is not completeness", {
  # two extra off-grid samples: nothing is missing, but the declared rate does
  # not describe this stream, so full coverage is not established either
  tt <- c(seq(0, 1, by = 0.1), 0.35, 0.65)
  tt <- sort(tt)
  p <- PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0,
                                                  length(tt), 1)),
                        samplingRate = 10,
                        rowData = S4Vectors::DataFrame(time_from_t0 = tt))
  m <- MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))
  cv <- streamCoverage(m, "a", 0, 1)
  expect_equal(cv$n_present, 13L)
  expect_equal(cv$n_expected, 11L)

  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0, end = 1,
    stringsAsFactors = FALSE))
  expect_equal(aggregateTrials(m, level = "session", features = "value",
                               include = "unknown")$states$state, "unknown")
})

test_that("the default is complete trials only, intervals required", {
  m <- .ten_second_recording()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), value = c(10, 20),
                          stringsAsFactors = FALSE)
  # no intervals: nothing describes the extent, so completeness is not shown
  err <- tryCatch(aggregateTrials(m, level = "session", features = "value"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "undescribed")
  expect_match(err, "aggregatePhysioFeatures")

  got <- aggregateTrials(m, level = "session", features = "value",
                         include = "undescribed")
  expect_equal(got$counts$n_undescribed, 2L)
  expect_equal(got$aggregate$data$value, 15)
})

# ---------------------------------------------------------------------------
# Gaps at the edge of a window. Selecting the window's samples and differencing
# those loses a dropout that straddles an edge: its outer bounding sample is
# outside the window, so the step across it is never formed. Detection therefore
# runs on the whole series and the window selects afterwards -- the order
# PhysioCrossModal's interpolation guard already used, and which the two now
# share rather than each implementing.
# ---------------------------------------------------------------------------

.gappy_stream <- function() {
  tt <- c(0, .1, .2, .4, .45, .5, .6, .7, .8, .9, 1)   # a 0.2 s hole, 0.2 -> 0.4
  p <- PhysioExperiment(assays = list(raw = matrix(seq_along(tt) * 1.0,
                                                  length(tt), 1)),
                        samplingRate = 10,
                        rowData = S4Vectors::DataFrame(time_from_t0 = tt))
  MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))
}

test_that("a gap straddling either edge of the window is found", {
  m <- .gappy_stream()

  left <- streamCoverage(m, "a", 0.25, 0.45)      # the gap crosses the left edge
  expect_equal(left$n_present, 2L)
  expect_equal(left$n_expected, 2L)               # the count alone says nothing
  expect_equal(nrow(left$gaps), 1L)
  expect_equal(left$gaps$start[1], 0.2)           # reported in the original series
  expect_equal(left$gaps$end[1], 0.4)

  right <- streamCoverage(m, "a", 0.10, 0.30)     # and the right edge
  expect_equal(nrow(right$gaps), 1L)

  inside <- streamCoverage(m, "a", 0.25, 0.35)    # a window wholly inside the gap
  expect_equal(inside$n_present, 0L)
  expect_equal(nrow(inside$gaps), 1L)
})

test_that("a gap outside the window, or touching it, is not in it", {
  m <- .gappy_stream()
  expect_equal(nrow(streamCoverage(m, "a", 0.50, 0.90)$gaps), 0L)  # elsewhere
  expect_equal(nrow(streamCoverage(m, "a", 0.40, 0.80)$gaps), 0L)  # touches at 0.4
  expect_equal(nrow(streamCoverage(m, "a", 0.00, 0.20)$gaps), 0L)  # touches at 0.2
})

test_that("a trial over a straddling gap is partial, not complete", {
  m <- .gappy_stream()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0.25, end = 0.45,
    stringsAsFactors = FALSE))
  expect_error(aggregateTrials(m, level = "session", features = "value"),
               "no trial satisfies")
  expect_equal(aggregateTrials(m, level = "session", features = "value",
                               include = "partial")$states$state, "partial")
})

test_that("timeWindow keeps the gap information it is about to make unfindable", {
  w <- timeWindow(.gappy_stream(), 0.25, 1.0)
  # the sample at 0.2 that bounded the gap from outside is gone
  expect_false(any(abs(streamTimeIndex(w, "a") - 0.2) < 1e-9))
  rec <- commonClock(w)$stream_gaps
  expect_equal(nrow(rec), 1L)
  expect_equal(rec$stream, "a")
  expect_equal(c(rec$start, rec$end), c(0.2, 0.4))

  cv <- streamCoverage(w, "a", 0.25, 0.45)
  expect_equal(nrow(cv$gaps), 1L)
  expect_equal(cv$rule$carried_gaps, 1L)
  expect_match(cv$rule$carried_basis, "record an earlier window left")

  # and a second window keeps it
  expect_equal(nrow(streamCoverage(timeWindow(w, 0.30, 0.60),
                                   "a", 0.30, 0.50)$gaps), 1L)
  # a window clear of the gap records nothing
  expect_null(commonClock(timeWindow(.gappy_stream(), 0.5, 1.0))$stream_gaps)
})

test_that("a carried gap that cannot be re-derived is reported, not dropped", {
  w <- timeWindow(.gappy_stream(), 0.25, 1.0)
  cv <- streamCoverage(w, "a", 0.25, 0.45, gap_factor = 3)
  expect_equal(cv$rule$carried_gaps, 0L)
  expect_equal(cv$rule$unusable_carried_gaps, 1L)
  expect_match(cv$rule$carried_basis, "cannot be re-derived")
})

test_that("dropping a stream drops the gaps recorded for it", {
  g <- .gappy_stream()
  flat <- PhysioExperiment(assays = list(raw = matrix(1:11 * 1.0, 11, 1)),
                           samplingRate = 10)
  m <- MultiPhysioExperiment(streams = list(a = g@streams$a, b = flat),
                             offsets = c(a = 0, b = 0))
  w <- timeWindow(m, 0.25, 1.0)
  expect_equal(unique(commonClock(w)$stream_gaps$stream), "a")
  expect_equal(nrow(commonClock(w[, "b"])$stream_gaps), 0L)
})

test_that("detectGaps is one definition, and the window only selects", {
  tt <- c(0, .1, .2, .4, .45, .5)
  all_g <- detectGaps(tt, rate = 10)
  expect_equal(nrow(all_g), 1L)
  expect_equal(c(all_g$start, all_g$end), c(0.2, 0.4))
  # selecting a window never invents or moves a gap, it only filters
  for (w in list(c(0.25, 0.45), c(0.1, 0.3), c(0.25, 0.35))) {
    sel <- detectGaps(tt, rate = 10, window = w)
    expect_equal(nrow(sel), 1L)
    expect_equal(c(sel$start, sel$end), c(0.2, 0.4))
  }
  expect_equal(nrow(detectGaps(tt, rate = 10, window = c(0.4, 0.5))), 0L)
  expect_equal(nrow(detectGaps(tt, rate = 10, window = c(0.0, 0.2))), 0L)
  # a regular series has none, whatever window is asked for
  expect_equal(nrow(detectGaps(seq(0, 1, by = 0.1), rate = 10,
                               window = c(0.25, 0.45))), 0L)
})

# ---------------------------------------------------------------------------
# Losing the evidence for a gap must not promote a trial to complete. The
# classification order is: shown to be short, then unable to tell, then shown to
# be whole -- a question that can no longer be answered is not an answer of yes.
# ---------------------------------------------------------------------------

.gappy_trial <- function() {
  m <- .gappy_stream()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0.25, end = 0.45,
    stringsAsFactors = FALSE))
}

.state_of <- function(obj, gf) {
  aggregateTrials(obj, level = "session", features = "value",
                  gap_factor = gf, include = .TRIAL_STATES)$states$state
}
.default_takes <- function(obj, gf) {
  !inherits(tryCatch(aggregateTrials(obj, level = "session", features = "value",
                                     gap_factor = gf),
                     error = function(e) e), "error")
}

test_that("a carried gap that cannot be re-derived makes the trial unknown", {
  w <- timeWindow(.gappy_trial(), 0.25, 1.0)
  cv <- streamCoverage(w, "a", 0.25, 0.45, gap_factor = 1.6)
  expect_equal(nrow(cv$gaps), 0L)              # 1.6 cannot re-derive it
  expect_equal(cv$rule$unusable_carried_gaps, 1L)

  expect_equal(.state_of(w, 1.6), "unknown")   # not complete
  expect_false(.default_takes(w, 1.6))         # and not taken by the default
  err <- tryCatch(aggregateTrials(w, level = "session", features = "value",
                                  gap_factor = 1.6),
                  error = function(e) conditionMessage(e))
  expect_match(err, "unknown")
})

test_that("losing the evidence never turns a partial trial complete", {
  # the same trial, seen through every combination of extraction and criterion.
  # Where the gap is still derivable it must read partial; where the record
  # cannot be reused it must read unknown; nowhere may it read complete.
  cases <- list(
    list(what = "original, matching criterion",   obj = .gappy_trial(),                          gf = 1.5),
    list(what = "original, changed criterion",    obj = .gappy_trial(),                          gf = 1.6),
    list(what = "windowed, matching criterion",   obj = timeWindow(.gappy_trial(), 0.25, 1.0),   gf = 1.5),
    list(what = "windowed, changed criterion",    obj = timeWindow(.gappy_trial(), 0.25, 1.0),   gf = 1.6),
    list(what = "twice windowed, matching",       obj = timeWindow(timeWindow(.gappy_trial(), 0.25, 1.0), 0.30, 0.60), gf = 1.5),
    list(what = "twice windowed, changed",        obj = timeWindow(timeWindow(.gappy_trial(), 0.25, 1.0), 0.30, 0.60), gf = 1.6),
    list(what = "windowed wide, changed",         obj = timeWindow(.gappy_trial(), 0.20, 1.0),   gf = 1.6)
  )
  for (cs in cases) {
    st <- .state_of(cs$obj, cs$gf)
    expect_true(st %in% c("partial", "unknown"), info = cs$what)
    expect_false(identical(st, "complete"), info = cs$what)
    expect_false(.default_takes(cs$obj, cs$gf), info = cs$what)
  }
})

test_that("a trial with no gap to lose is still complete after windowing", {
  # the guard must not turn every windowed trial into unknown: a stream with no
  # dropout records nothing, so there is nothing that cannot be re-derived
  p <- PhysioExperiment(assays = list(raw = matrix(1:11 * 1.0, 11, 1)),
                        samplingRate = 10)
  m <- MultiPhysioExperiment(streams = list(a = p), offsets = c(a = 0))
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0.5, end = 0.8,
    stringsAsFactors = FALSE))
  w <- timeWindow(m, 0.4, 1.0)
  expect_null(commonClock(w)$stream_gaps)
  expect_equal(.state_of(w, 1.6), "complete")
  expect_true(.default_takes(w, 1.6))
})

test_that("a partial trial stays partial rather than being softened to unknown", {
  # order matters in both directions: evidence this query CAN use outranks the
  # record it cannot, so a demonstrable shortfall is still reported as one
  m <- .gappy_stream()
  trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = "T1", value = 1, stringsAsFactors = FALSE)
  m <- `trialIntervals<-`(m, value = data.frame(
    subject_id = "P1", session_id = "S1", trial_id = "T1",
    recording_id = "R1", stream = "a", start = 0.9, end = 1.4,
    stringsAsFactors = FALSE))
  w <- timeWindow(m, 0.25, 1.4)
  expect_gt(nrow(commonClock(w)$stream_gaps), 0L)   # a record exists
  expect_equal(.state_of(w, 1.6), "partial")        # but the shortfall decides
})

test_that("trialCoverage reports what it could not re-derive", {
  w <- timeWindow(.gappy_trial(), 0.25, 1.0)
  cv <- trialCoverage(w, "T1", gap_factor = 1.6)
  expect_equal(cv$n_gaps, 0L)
  expect_equal(cv$n_unusable_carried_gaps, 1L)
  # the report and the judgement rest on the same fact
  expect_equal(.state_of(w, 1.6), "unknown")
  # and under the criterion the record was written with, both see the gap
  cv15 <- trialCoverage(w, "T1", gap_factor = 1.5)
  expect_equal(cv15$n_gaps, 1L)
  expect_equal(cv15$n_unusable_carried_gaps, 0L)
  expect_equal(.state_of(w, 1.5), "partial")
})
