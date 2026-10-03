feature_record <- function(subject = "P1", session = "S1", values = c(1, 9),
                           reverse = FALSE) {
  start <- (seq_along(values) - 1) * 2
  tt <- seq(0, max(start) + 1, by = .1)
  y <- rep(0, length(tt))
  for (j in seq_along(values)) y[tt >= start[j] & tt <= start[j] + 1] <- values[j]
  a <- cbind(A = y, B = 2 * y)
  if (reverse) a <- a[, c("B", "A")]
  m <- MultiPhysioExperiment(sensor = PhysioExperiment(assays = list(raw = a),
    samplingRate = 10, rowData = S4Vectors::DataFrame(time_from_t0 = tt)))
  trials(m) <- data.frame(subject_id = subject, session_id = session,
    trial_id = paste0("T", seq_along(values)), condition = "task")
  trialIntervals(m) <- cbind(trials(m)[c("subject_id", "session_id", "trial_id")],
    recording_id = "R1", stream = "sensor", start = start, end = start + 1)
  m
}

feature_cohort <- function() {
  p1 <- PhysioLongitudinal(sessions = list(S1 = feature_record(),
    S2 = feature_record(session = "S2", values = 13, reverse = TRUE)))
  p2 <- PhysioLongitudinal(sessions = list(S1 = feature_record(subject = "P2", values = c(22, 30))))
  PhysioCohort(subjects = list(P1 = p1, P2 = p2))
}

test_that("one extraction replaces hierarchy traversal and preserves equal weights", {
  x <- feature_cohort()
  f <- extractTrialFeatures(x, "sensor", list(amplitude = mean, peak = max))
  z <- aggregatePhysioFeatures(f, "cohort")
  # Manual: sessions 5,13,26 -> people 9,26 -> cohort 17.5 (pooled trials = 15).
  expect_equal(z$data$amplitude, c(17.5, 35))
  expect_equal(z$data$peak, c(17.5, 35))
  expect_equal(z$steps$session$data$amplitude, c(5, 10, 13, 26, 26, 52))
  expect_equal(z$steps$subject$data$amplitude, c(9, 18, 26, 52))
  expect_equal(z$counts$n_used, rep(2L, 4))
  expect_equal(nrow(f$data), 10L)
  expect_equal(sum(f$trials$included), 5L)
  expect_identical(z$extraction, f)
  expect_identical(z$source, f$data)
  expect_equal(z$source_rows[[1]], c(1L, 3L, 5L, 7L, 9L))
  expect_identical(x, feature_cohort())
})

test_that("continuing aggregation retains original samples and detects edits", {
  f <- extractTrialFeatures(feature_cohort(), "sensor", list(amplitude = mean))
  s <- aggregatePhysioFeatures(f, "session")
  z <- aggregatePhysioFeatures(s, "cohort")
  expect_equal(z$data, aggregatePhysioFeatures(f, "cohort")$data)
  expect_identical(z$extraction$source_map, f$source_map)
  expect_identical(z$source_rows, aggregatePhysioFeatures(f, "cohort")$source_rows)
  altered <- f; altered$source_map[[1]]$samples[1] <- 3L
  expect_error(aggregatePhysioFeatures(altered, "cohort"), "modified")
  altered <- s; altered$extraction$trials$included[1] <- FALSE
  expect_error(aggregatePhysioFeatures(altered, "cohort"), "modified")
  expect_error(aggregatePhysioFeatures(f, "cohort", strata = character()), "strata")
  path <- tempfile(); on.exit(unlink(path)); saveRDS(s, path)
  expect_identical(aggregatePhysioFeatures(readRDS(path), "cohort"), z)
})

test_that("source references reconstruct features from original rows", {
  m <- feature_record()
  f <- extractTrialFeatures(m, "sensor", list(amplitude = mean))
  for (i in seq_len(nrow(f$data))) {
    ref <- f$source_map[[i]]
    expect_identical(ref$data_row, i)
    raw <- SummarizedExperiment::assay(streams(m)[[ref$stream]], ref$assay)
    expect_equal(mean(raw[ref$samples, ref$channel]), f$data$amplitude[i])
    expect_equal(ref$times, streamTimeIndex(m, "sensor")[ref$samples])
  }
  changed <- m
  a <- SummarizedExperiment::assay(changed@streams$sensor)
  a[1, 1] <- a[1, 1] + 1
  SummarizedExperiment::assay(changed@streams$sensor) <- a
  g <- extractTrialFeatures(changed, "sensor", list(amplitude = mean))
  expect_false(identical(f$source_hashes, g$source_hashes))
  expect_false(identical(f$checksum, g$checksum))
})

test_that("incomplete trials are excluded before callbacks run", {
  m <- feature_record(); m@streams$sensor <- m@streams$sensor[-4, ]
  calls <- 0L
  f <- extractTrialFeatures(m, "sensor", list(amplitude = function(v) {
    calls <<- calls + 1L; mean(v)
  }))
  expect_equal(calls, 2L) # only complete T2, two channels
  expect_identical(f$trials$included, c(FALSE, TRUE))
  expect_identical(f$trials$state, c("partial", "complete"))
  expect_equal(aggregatePhysioFeatures(f, "session")$data$amplitude, c(9, 18))
  g <- extractTrialFeatures(m, "sensor", list(amplitude = mean), include = c("complete", "partial"))
  expect_equal(aggregatePhysioFeatures(g, "session")$data$amplitude, c(5, 10))
  expect_length(g$source_map[[1]]$samples, 10L)
  expect_error(extractTrialFeatures(m, "sensor", list(amplitude = mean), include = "unknown"), "arg")
})

test_that("coverage of other trial streams and retained evidence are respected", {
  m <- feature_record()
  m@streams$other <- m@streams$sensor[-4, ]
  it <- trialIntervals(m); other <- it; other$stream <- "other"
  trialIntervals(m) <- rbind(it, other)
  f <- extractTrialFeatures(m, "sensor", list(amplitude = mean))
  expect_identical(f$trials$included, c(FALSE, TRUE))
  # Cropping removes the left gap bracket; a new criterion cannot reuse evidence.
  w <- timeWindow(m, .25, .45)
  expect_error(extractTrialFeatures(w, "sensor", list(amplitude = mean),
    include = "partial", gap_factor = 1.6), "unusable retained gap")
})

test_that("missing signal values remain explicit, distinct from time coverage", {
  m <- feature_record()
  a <- SummarizedExperiment::assay(m@streams$sensor); a[1, 1] <- NA
  SummarizedExperiment::assay(m@streams$sensor) <- a
  f <- extractTrialFeatures(m, "sensor", list(amplitude = mean))
  expect_true(is.na(f$data$amplitude[1]))
  expect_identical(f$source_map[[1]]$n_missing, 1L)
  expect_error(aggregatePhysioFeatures(f, "session"), "Missing feature")
  z <- aggregatePhysioFeatures(f, "session", na.rm = TRUE)
  expect_equal(z$data$amplitude, c(9, 10))
  expect_equal(z$counts$n_missing, c(1L, 0L))
  g <- extractTrialFeatures(m, "sensor", list(amplitude = mean), na.rm = TRUE)
  expect_equal(g$data$amplitude, c(1, 2, 9, 18))
  expect_identical(g$settings$arguments, list(na.rm = TRUE))
})

test_that("condition groups and requested channel order are preserved", {
  m <- feature_record(); tr <- trials(m); tr$condition <- c("rest", "task"); trials(m) <- tr
  f <- extractTrialFeatures(m, "sensor", list(amplitude = mean),
    strata = "condition", channels = c("B", "A"))
  z <- aggregatePhysioFeatures(f, "cohort")
  expect_equal(z$data$amplitude, c(2, 1, 18, 9))
  expect_identical(z$data$condition, c("rest", "rest", "task", "task"))
  expect_identical(z$data$channel, c("B", "A", "B", "A"))
})

test_that("inconsistent identities, channels and callbacks fail informatively", {
  m <- feature_record()
  expect_error(extractTrialFeatures(list(a = m, b = m), "sensor", list(amplitude = mean)), "Duplicate")
  p <- PhysioLongitudinal(sessions = list(wrong = m))
  expect_error(extractTrialFeatures(p, "sensor", list(amplitude = mean)), "session_id disagrees")
  expect_error(extractTrialFeatures(m, "sensor", list(amplitude = mean), channels = "absent"), "channel")
  expect_error(extractTrialFeatures(m, "sensor", list(amplitude = mean), assay = "absent"), "assay")
  expect_error(extractTrialFeatures(m, "sensor", mean), "named list")
  expect_error(extractTrialFeatures(m, "sensor", list(trial_id = mean)), "nonreserved")
  expect_error(extractTrialFeatures(m, "sensor", list(a = function(v) c(1, 2))), "one numeric")
  expect_error(extractTrialFeatures(m, "sensor", list(a = function(v) Inf)), "one numeric")
  expect_error(extractTrialFeatures(m, "sensor", list(a = function(v) stop("deliberate"))), "Feature a failed.*deliberate")
  expect_error(extractTrialFeatures(m, "sensor", list(a = mean), gap_factor = NA_real_), "gap_factor")
})

test_that("absent stream is audited when other recordings contribute", {
  a <- feature_record(); b <- feature_record(session = "S2")
  names(b@streams) <- "elsewhere"
  it <- trialIntervals(b); it$stream <- "elsewhere"; trialIntervals(b) <- it
  f <- extractTrialFeatures(list(a = a, b = b), "sensor", list(amplitude = mean))
  expect_identical(f$trials$included, c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(f$trials$reason[3:4], rep("requested stream absent", 2))
  expect_equal(nrow(f$source_hashes), 2L)
  expect_error(extractTrialFeatures(b, "sensor", list(amplitude = mean)), "No trial")
})
