wave_record <- function(subject = "P1", session = "S1", amplitudes = c(1, 9),
                        durations = rep(1, length(amplitudes)), rate = 10,
                        reverse = FALSE, phase = FALSE) {
  starts <- (seq_along(amplitudes) - 1) * 4
  tt <- seq(0, max(starts + durations), by = 1 / rate)
  v <- rep(0, length(tt))
  for (i in seq_along(starts)) {
    take <- tt >= starts[i] - 1e-10 & tt <= starts[i] + durations[i] + 1e-10
    v[take] <- amplitudes[i] + (tt[take] - starts[i]) /
      if (phase) durations[i] else 1
  }
  a <- cbind(A = v, B = 2 * v)
  if (reverse) a <- a[, c("B", "A")]
  pe <- PhysioExperiment(assays = list(raw = a), samplingRate = rate,
    rowData = S4Vectors::DataFrame(time_from_t0 = tt))
  m <- MultiPhysioExperiment(signal = pe)
  trials(m) <- data.frame(subject_id = subject, session_id = session,
    trial_id = paste0("T", seq_along(amplitudes)), condition = "task")
  trialIntervals(m) <- cbind(trials(m)[c("subject_id", "session_id", "trial_id")],
    recording_id = paste(subject, session, sep = "/"), stream = "signal",
    start = starts, end = starts + durations)
  m
}

test_that("shifted trials align by measured time and retain interpolation lineage", {
  z <- aggregateTrialWaveforms(wave_record(), "signal", c(0, .15, 1))
  expect_equal(z$data$value[z$data$channel == "A"], c(5, 5.15, 6))
  expect_equal(z$data$value[z$data$channel == "B"], 2 * c(5, 5.15, 6))
  expect_true(all(z$counts$n_trials == 2))
  mid <- z$sample_map[z$sample_map$grid_index == 2 & z$sample_map$channel == "A", ]
  expect_equal(mid$weight_right, c(.5, .5), tolerance = 1e-12)
  expect_equal(mid$right_sample - mid$left_sample, c(1L, 1L))
  expect_equal(nrow(z$aligned), 12L)
})

test_that("phase normalization and start alignment have distinct known answers", {
  m <- wave_record(durations = c(1, 2), phase = TRUE)
  p <- aggregateTrialWaveforms(m, "signal", c(0, .5, 1), align = "phase")
  t <- aggregateTrialWaveforms(m, "signal", c(0, .5, 1), align = "start")
  expect_equal(p$data$value[p$data$channel == "A"], c(5, 5.5, 6))
  expect_equal(t$data$value[t$data$channel == "A"], c(5, 5.375, 5.75))
})

test_that("hierarchy gives equal session and subject weights, matching names", {
  x <- list(a = wave_record(), b = wave_record(session = "S2", amplitudes = 13,
    reverse = TRUE, rate = 20), c = wave_record(subject = "P2", amplitudes = c(22, 30)))
  z <- aggregateTrialWaveforms(x, "signal", c(0, .5, 1), level = "cohort")
  expect_equal(z$data$value[z$data$channel == "A"], c(17.5, 18, 18.5))
  expect_equal(z$data$value[z$data$channel == "B"], 2 * c(17.5, 18, 18.5))
  expect_true(all(z$counts$n_trials == 5 & z$counts$n_used == 2))
  expect_equal(sort(unique(z$aligned$subject_id)), c("P1", "P2"))
  f <- tempfile(); saveRDS(z, f); expect_identical(readRDS(f), z)
})

test_that("conditions stay separate and counts show original trials", {
  m <- wave_record(); tr <- trials(m); tr$condition <- c("rest", "task"); trials(m) <- tr
  z <- aggregateTrialWaveforms(m, "signal", c(0, 1), strata = "condition")
  expect_equal(z$data$value[z$data$channel == "A"], c(1, 2, 9, 10))
  expect_true(all(z$counts$n_trials == 1))
})

test_that("no extrapolation or interpolation over missing values is hidden", {
  m <- wave_record(amplitudes = 1)
  expect_error(aggregateTrialWaveforms(m, "signal", c(0, 2)), "Missing aligned")
  z <- aggregateTrialWaveforms(m, "signal", c(0, 2), na.rm = TRUE)
  expect_true(all(is.na(z$data$value[z$data$grid_index == 2])))
  expect_true(all(z$counts$n_trials[z$data$grid_index == 2] == 0))
  e <- m@streams$signal; a <- SummarizedExperiment::assay(e); a[2, 1] <- NA
  SummarizedExperiment::assay(e) <- a; m@streams$signal <- e
  z <- aggregateTrialWaveforms(m, "signal", c(0, .05, .1, .15, .2), na.rm = TRUE)
  expect_equal(z$data$value[z$data$channel == "A"], c(1, NA, NA, NA, 1.2))
  expect_equal(z$counts$n_trials[z$data$channel == "A"], c(1, 0, 0, 0, 1))
})

test_that("gaps exclude trials by default and are never bridged when partial is allowed", {
  m <- wave_record()
  e <- m@streams$signal; m@streams$signal <- e[-4, ] # remove t=.3
  z <- aggregateTrialWaveforms(m, "signal", c(.2, .3, .4))
  expect_equal(z$data$value[z$data$channel == "A"], c(9.2, 9.3, 9.4))
  expect_equal(z$trials$included, c(FALSE, TRUE))
  p <- aggregateTrialWaveforms(m, "signal", c(.2, .3, .4),
    include = c("complete", "partial"), na.rm = TRUE)
  expect_equal(p$data$value[p$data$channel == "A"], c(5.2, 9.3, 5.4))
  expect_equal(p$counts$n_trials[p$data$channel == "A"], c(2, 1, 2))
  expect_true(any(p$sample_map$reason == "gap"))
})

test_that("bad identities and ambiguous selections are refused", {
  m <- wave_record()
  expect_error(aggregateTrialWaveforms(list(a = m, b = m), "signal", c(0, 1)), "Duplicate")
  expect_error(aggregateTrialWaveforms(m, "signal", c(1, 0)), "grid")
  expect_error(aggregateTrialWaveforms(m, "signal", c(0, 1.1), align = "phase"), "phase")
  expect_error(aggregateTrialWaveforms(m, "signal", c(0, 1), include = "unknown"), "arg")
  expect_error(aggregateTrialWaveforms(m, "signal", c(0, 1), channels = "absent"), "channel")
})

test_that("hierarchy containers and lists produce the same cohort means", {
  a <- wave_record(); b <- wave_record(session = "S2", amplitudes = 13)
  c <- wave_record(subject = "P2", amplitudes = c(22, 30))
  p1 <- PhysioLongitudinal(sessions = list(S1 = a, S2 = b),
    design = S4Vectors::DataFrame(session_id = c("S1", "S2"),
      visit_label = c("first", "second"), days_from_baseline = c(0, 14)))
  p2 <- PhysioLongitudinal(sessions = list(S1 = c))
  x <- PhysioCohort(subjects = list(P1 = p1, P2 = p2))
  z <- aggregateTrialWaveforms(x, "signal", c(0, .5, 1), level = "cohort")
  expect_equal(z$data$value[z$data$channel == "A"], c(17.5, 18, 18.5))
  z <- aggregateTrialWaveforms(p1, "signal", c(0, 1), level = "subject")
  expect_equal(z$data$value[z$data$channel == "A"], c(9, 10))
})

test_that("explicit anchors retain the original onset after cropping", {
  m <- wave_record(amplitudes = 1)
  w <- timeWindow(m, .2, 1)
  expect_error(aggregateTrialWaveforms(w, "signal", c(0, .2, 1),
    include = "partial", na.rm = TRUE), "anchors")
  anchors <- cbind(trials(m)[.TRIAL_KEYS], start = 0, end = 1)
  z <- aggregateTrialWaveforms(w, "signal", c(0, .2, 1), anchors = anchors,
    include = "partial", na.rm = TRUE)
  expect_equal(z$data$value[z$data$channel == "A"], c(NA, 1.2, 2))
  z <- aggregateTrialWaveforms(w, "signal", c(0, .5, 1), align = "phase",
    anchors = anchors, include = "partial", na.rm = TRUE)
  expect_equal(z$data$value[z$data$channel == "A"], c(NA, 1.5, 2))
  expect_error(aggregateTrialWaveforms(w, "signal", c(0, 1)), "No trial")
})

test_that("unusable carried gaps cannot enter waveform averaging", {
  m <- wave_record(amplitudes = 1)
  m@streams$signal <- m@streams$signal[-4, ]
  w <- timeWindow(m, .25, 1)
  anchors <- cbind(trials(m)[.TRIAL_KEYS], start = 0, end = 1)
  expect_error(aggregateTrialWaveforms(w, "signal", c(.3, .4),
    anchors = anchors, include = "partial", gap_factor = 1.6, na.rm = TRUE),
    "unusable retained")
  z <- aggregateTrialWaveforms(w, "signal", c(.3, .4), anchors = anchors,
    include = "partial", na.rm = TRUE)
  expect_equal(z$data$value[z$data$channel == "A"], c(NA, 1.4))
})

test_that("all streams use the same trial onset rather than their own first sample", {
  m <- wave_record(amplitudes = 1)
  # Late stream is observed only from .2; shared trial origin stays at zero.
  m@streams$late <- m@streams$signal[3:11, ]
  it <- trialIntervals(m)
  second <- it; second$stream <- "late"; second$start <- .2
  trialIntervals(m) <- rbind(it, second)
  z <- aggregateTrialWaveforms(m, "late", c(0, .2, 1), na.rm = TRUE)
  expect_equal(z$data$value[z$data$channel == "A"], c(NA, 1.2, 2))
  expect_equal(z$anchors$start, 0)
})

test_that("channel-specific omissions retain hierarchy weighting", {
  a <- wave_record(amplitudes = c(1, 9))
  e <- a@streams$signal; v <- SummarizedExperiment::assay(e); v[1, 1] <- NA
  SummarizedExperiment::assay(e) <- v; a@streams$signal <- e
  b <- wave_record(session = "S2", amplitudes = 13)
  z <- aggregateTrialWaveforms(list(a = a, b = b), "signal", c(0, 1),
    level = "subject", na.rm = TRUE)
  expect_equal(z$data$value[z$data$channel == "A"], c(11, 10))
  expect_equal(z$data$value[z$data$channel == "B"], c(18, 20))
  expect_equal(z$counts$n_trials[z$data$channel == "A"], c(2, 3))
  expect_true(all(z$counts$n_used == 2))
})

test_that("one sample, explicit events and invalid options are handled", {
  m <- wave_record(amplitudes = 1)
  it <- trialIntervals(m); it$end <- 0; trialIntervals(m) <- it
  z <- aggregateTrialWaveforms(m, "signal", 0)
  expect_equal(z$data$value, c(1, 2))
  expect_error(aggregateTrialWaveforms(m, "signal", 0, align = "phase"), "positive")
  m <- wave_record(amplitudes = 1)
  anchors <- cbind(trials(m)[.TRIAL_KEYS], start = .5, end = 1)
  z <- aggregateTrialWaveforms(m, "signal", c(-.5, 0, .5), anchors = anchors)
  expect_equal(z$data$value[z$data$channel == "A"], c(1, 1.5, 2))
  for (factor in c(NA, Inf, 0, 1))
    expect_error(aggregateTrialWaveforms(m, "signal", 0, gap_factor = factor), "gap_factor")
  expect_error(aggregateTrialWaveforms(m, "signal", 0, na.rm = NA), "na.rm")
  expect_error(aggregateTrialWaveforms(m, "signal", 0, strata = "value"), "strata")
  expect_error(aggregateTrialWaveforms(m, "signal", 0, anchors = anchors[0, ]), "anchor")
})

test_that("same channel sets can be selected explicitly and input stays unchanged", {
  m <- wave_record(); n <- wave_record(session = "S2")
  n@streams$signal <- n@streams$signal[, "A", drop = FALSE]
  expect_error(aggregateTrialWaveforms(list(m = m, n = n), "signal", 0), "channel")
  before <- serialize(m, NULL)
  z <- aggregateTrialWaveforms(list(m = m, n = n), "signal", 0, channels = "A")
  expect_equal(z$data$channel, c("A", "A"))
  expect_identical(serialize(m, NULL), before)
})

test_that("stale trial identities cannot override the enclosing hierarchy", {
  a <- wave_record(); b <- wave_record(session = "S2", amplitudes = 13)
  expect_error(aggregateTrialWaveforms(PhysioLongitudinal(X = a), "signal", 0),
    "session_id disagrees")
  wrong <- PhysioCohort(A = PhysioLongitudinal(S1 = a))
  expect_error(aggregateTrialWaveforms(wrong, "signal", 0, level = "cohort"),
    "subject_id disagrees")
  wrong <- PhysioLongitudinal(S1 = a,
    S2 = wave_record(subject = "P2", session = "S2"))
  expect_error(aggregateTrialWaveforms(wrong, "signal", 0), "different trial subject_id")
  wrong <- PhysioLongitudinal(S1 = a, subject = S4Vectors::DataFrame(id = "P2"))
  expect_error(aggregateTrialWaveforms(wrong, "signal", 0), "subject_id disagrees")
})

test_that("large clock origins do not snap interpolation targets to samples", {
  m <- wave_record(amplitudes = 1)
  base <- aggregateTrialWaveforms(m, "signal", c(.1005, .5005))
  shifted <- m
  rd <- SummarizedExperiment::rowData(shifted@streams$signal)
  rd$time_from_t0 <- rd$time_from_t0 + 1.7e9
  SummarizedExperiment::rowData(shifted@streams$signal) <- rd
  it <- trialIntervals(shifted); it$start <- it$start + 1.7e9; it$end <- it$end + 1.7e9
  trialIntervals(shifted) <- it
  z <- aggregateTrialWaveforms(shifted, "signal", c(.1005, .5005))
  # Accuracy is limited by representation of the translated input timestamps.
  expect_equal(z$data$value, base$data$value, tolerance = 1e-6)
  expect_true(all(z$sample_map$weight_right > 0))
})

test_that("gap lineage remains explicit and unusable other-stream evidence refuses inclusion", {
  m <- wave_record(amplitudes = 1)
  m@streams$signal <- m@streams$signal[-4, ]
  z <- aggregateTrialWaveforms(m, "signal", .3, include = "partial", na.rm = TRUE)
  expect_true(all(z$sample_map$reason == "gap"))
  expect_equal(z$sample_map$left_sample, c(3L, 3L))
  expect_equal(z$sample_map$right_sample, c(4L, 4L))
  expect_equal(z$sample_map$weight_right, c(.5, .5))
  m <- wave_record(amplitudes = 1)
  m@streams$other <- m@streams$signal[-4, ]
  it <- trialIntervals(m); other <- it; other$stream <- "other"
  trialIntervals(m) <- rbind(it, other)
  w <- timeWindow(m, .25, 1)
  anchors <- cbind(trials(m)[.TRIAL_KEYS], start = 0, end = 1)
  expect_error(aggregateTrialWaveforms(w, "signal", c(.4, .5), anchors = anchors,
    include = "partial", gap_factor = 1.6), "unusable retained")
})
