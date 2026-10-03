# Waveform reliability: CMC, pointwise ICC / SEM / MDC, verified on synthetic
# data with a known reliability structure.

test_that("CMC is ~1 for near-identical repeated waveforms and drops with noise", {
  base <- sin(seq(0, 2 * pi, length.out = 101))
  set.seed(1)
  clean <- t(sapply(1:6, function(i) base + rnorm(101, 0, 0.01)))
  noisy <- t(sapply(1:6, function(i) base + rnorm(101, 0, 0.6)))
  cc <- waveformCMC(clean); cn <- waveformCMC(noisy)
  expect_gt(cc, 0.99)
  expect_lt(cn, cc)                                   # noise reduces repeatability
  expect_true(cc >= 0 && cc <= 1)
})

test_that("CMC is low (or NA) for a shapeless, pure-noise waveform set", {
  set.seed(11)
  flat <- matrix(rnorm(6 * 101, 0, 0.1), nrow = 6)    # no common waveform shape
  v <- suppressWarnings(waveformCMC(flat))            # radicand ~ 0 -> low or NA
  expect_true(is.na(v) || v < 0.5)
})

test_that("between-group CMC compares session-mean waveforms", {
  base <- sin(seq(0, 2 * pi, length.out = 101))
  set.seed(2)
  x <- rbind(t(sapply(1:3, function(i) base + rnorm(101, 0, 0.05))),
             t(sapply(1:3, function(i) base + rnorm(101, 0, 0.05))))
  g <- rep(c("day1", "day2"), each = 3)
  bc <- waveformCMC(x, groups = g)
  expect_gt(bc, 0.99)                                 # two days agree
})

test_that("pointwise ICC is high when subjects are separable, low when not", {
  set.seed(3)
  # subject offset IS the signal -> subjects separable -> high ICC
  sep <- lapply(1:10, function(s) {
    b <- 10 * s + sin(seq(0, 2 * pi, length.out = 41))
    t(sapply(1:3, function(k) b + rnorm(41, 0, 0.2)))
  })
  # no subject offset, pure noise -> subjects not separable -> low ICC
  noise <- lapply(1:10, function(s)
    t(sapply(1:3, function(k) rnorm(41, 0, 1))))
  wi_sep <- waveformICC(sep); wi_noi <- waveformICC(noise)
  expect_gt(wi_sep$mean_icc, 0.95)
  expect_lt(wi_noi$mean_icc, 0.5)
  expect_length(wi_sep$icc, 41)
  expect_s3_class(wi_sep, "waveform_icc")
})

test_that("waveformReliability integrates CMC + ICC/SEM/MDC curves consistently", {
  set.seed(4)
  subs <- lapply(1:10, function(s) {
    base <- 5 * s + 5 * sin(seq(0, 2 * pi, length.out = 51))
    t(sapply(1:3, function(k) base + rnorm(51, 0, 0.3)))
  })
  r <- waveformReliability(subs)
  expect_s3_class(r, "waveform_reliability")
  expect_length(r$icc, 51); expect_length(r$sem, 51); expect_length(r$mdc, 51)
  expect_gt(r$mean_icc, 0.95)                         # separable subjects
  expect_gt(r$mean_cmc, 0.9)                          # repeatable trials
  # MDC = z * sqrt(2) * SEM ; with default 95% -> ratio ~ 2.772
  expect_equal(r$mdc, qnorm(0.975) * sqrt(2) * r$sem, tolerance = 1e-8)
  # more trial noise -> larger SEM/MDC
  subs2 <- lapply(subs, function(m) m + matrix(rnorm(length(m), 0, 2), nrow = nrow(m)))
  r2 <- waveformReliability(subs2)
  expect_gt(r2$mean_sem, r$mean_sem)
  expect_output(print(r), "Waveform reliability")
})

test_that("accepts a [subject, trial, frame] array", {
  set.seed(5)
  arr <- array(0, dim = c(8, 3, 31))
  for (s in 1:8) for (k in 1:3)
    arr[s, k, ] <- 4 * s + sin(seq(0, 2 * pi, length.out = 31)) + rnorm(31, 0, 0.2)
  r <- waveformReliability(arr)
  expect_equal(r$n_subjects, 8); expect_equal(r$n_trials, 3); expect_equal(r$n_frames, 31)
  expect_gt(r$mean_icc, 0.95)
})
