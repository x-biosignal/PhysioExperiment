# Reliability and repeatability of movement WAVEFORMS.
#
# The ecosystem's icc()/sem()/mdc() (PhysioExperiment) quantify the reliability of a
# single SCALAR score. Movement analysis produces CURVES -- a joint angle,
# moment or EMG envelope over the gait cycle -- whose reliability is itself a
# curve. This module adds the waveform analogues used in gait-analysis
# repeatability studies:
#   * the coefficient of multiple correlation (CMC; Kadaba et al. 1989) for the
#     similarity of repeated waveforms (within-day) or session-mean waveforms
#     (between-day), and
#   * pointwise ICC / SEM / MDC CURVES, built frame-by-frame on the *same*
#     PhysioExperiment definitions, so you can see WHERE along the cycle a measurement
#     is reliable (stance is typically more repeatable than swing).
# NB: PhysioMoCap already re-exports a `runCMC` -- that is OpenSim Computed
# Muscle Control, an unrelated method; the waveform reliability CMC is here.
# Dependency-light base R (+ PhysioExperiment for the scalar ICC/SEM/MDC).

# Coerce input to a list of (trials x frames) matrices, one per subject.
.wf_as_subject_list <- function(x) {
  if (is.array(x) && length(dim(x)) == 3L) {
    d <- dim(x)
    out <- lapply(seq_len(d[1]), function(i) matrix(x[i, , ], nrow = d[2], ncol = d[3]))
  } else if (is.list(x) && !is.data.frame(x)) {
    out <- lapply(x, as.matrix)
  } else if (is.matrix(x) || is.data.frame(x)) {
    out <- list(as.matrix(x))
  } else {
    stop("`x` must be a trials x frames matrix, a list of them, or a ",
         "[subject, trial, frame] array.", call. = FALSE)
  }
  nf <- vapply(out, ncol, integer(1))
  if (length(unique(nf)) != 1L)
    stop("all subjects must have the same number of frames (columns).", call. = FALSE)
  out
}

#' Coefficient of multiple correlation for repeated waveforms (Kadaba CMC)
#'
#' The waveform analogue of a reliability coefficient (Kadaba et al. 1989): how
#' similar a set of repeated curves are, relative to the curve's own variation.
#' With `groups = NULL` the rows of `x` are repeated trials and the result is the
#' **within-day** repeatability; pass `groups` (e.g. a session/day label per row)
#' for the **between-day** CMC of the session-mean waveforms.
#'
#' @param x A `trials x frames` numeric matrix: each row one waveform (e.g. a
#'   joint angle over the time-normalised cycle), columns the cycle points.
#' @param groups Optional grouping (length `nrow(x)`); when given, the CMC is
#'   computed between the group-mean waveforms (between-day/-condition).
#' @return The CMC in \[0, 1] (higher = more repeatable), or `NA` with a warning
#'   when the radicand is negative (waveform variation too small for a defined
#'   CMC -- a known limitation for low-excursion curves).
#' @references Kadaba MP, et al. (1989) J Orthop Res 7:849-860.
#' @seealso [waveformICC()], [waveformReliability()]
#' @export
#' @examples
#' base <- sin(seq(0, 2 * pi, length.out = 101))
#' trials <- t(sapply(1:5, function(i) base + rnorm(101, 0, 0.02)))
#' waveformCMC(trials)                                  # ~ 1 (repeatable)
waveformCMC <- function(x, groups = NULL) {
  x <- as.matrix(x)
  if (nrow(x) < 2L || ncol(x) < 2L)
    stop("`x` needs >= 2 waveforms (rows) and >= 2 frames (columns).", call. = FALSE)
  F <- ncol(x)
  if (is.null(groups)) {
    M <- x                                        # rows are the repeated units
  } else {
    groups <- as.factor(groups)
    if (length(groups) != nrow(x))
      stop("`groups` must have length nrow(x).", call. = FALSE)
    if (nlevels(groups) < 2L)
      stop("`groups` needs >= 2 levels for a between-group CMC.", call. = FALSE)
    M <- do.call(rbind, lapply(levels(groups),
                               function(g) colMeans(x[groups == g, , drop = FALSE])))
  }
  T <- nrow(M)
  mean_f <- colMeans(M)                           # mean waveform across units
  grand  <- mean(M)
  num <- sum(sweep(M, 2L, mean_f)^2) / (F * (T - 1))     # residual (unit-to-unit)
  den <- sum((M - grand)^2) / (F * T - 1)               # total about grand mean
  radicand <- 1 - num / den
  if (!is.finite(radicand) || radicand < 0) {
    warning("CMC undefined (negative radicand: waveform variation too small); returning NA.",
            call. = FALSE)
    return(NA_real_)
  }
  sqrt(radicand)
}

#' Pointwise intraclass correlation across the movement cycle
#'
#' The reliability *curve*: the between-subject ICC computed frame-by-frame from
#' repeated trials, using the ecosystem's [icc()] at each cycle point.
#' It shows where along the movement a measurement is reliable.
#'
#' @param x A `[subject, trial, frame]` array, or a list of `trials x frames`
#'   matrices (one per subject). Every subject must have the same trials and
#'   frames.
#' @param model,type,unit Passed to [icc()] (default two-way,
#'   absolute agreement, single measure -- ICC(2,1)).
#' @return a `waveform_icc` list: `icc`, `ci_lower`, `ci_upper` (per frame),
#'   `mean_icc`, `min_icc`.
#' @seealso [waveformReliability()], [waveformCMC()]
#' @export
#' @examples
#' set.seed(1)
#' subs <- lapply(1:8, function(s) {
#'   base <- s + sin(seq(0, 2 * pi, length.out = 101))   # subject offset = signal
#'   t(sapply(1:3, function(k) base + rnorm(101, 0, 0.1)))
#' })
#' waveformICC(subs)$mean_icc                            # high (subjects separable)
waveformICC <- function(x, model = "twoway", type = "agreement", unit = "single") {
  subs <- .wf_as_subject_list(x)
  ns <- length(subs)
  if (ns < 2L) stop("need >= 2 subjects for a between-subject ICC curve.", call. = FALSE)
  k <- nrow(subs[[1]])
  if (k < 2L) stop("need >= 2 trials per subject for an ICC.", call. = FALSE)
  if (length(unique(vapply(subs, nrow, integer(1)))) != 1L)
    stop("all subjects must have the same number of trials.", call. = FALSE)
  F <- ncol(subs[[1]])
  icc_f <- ci_lo <- ci_hi <- numeric(F)
  for (f in seq_len(F)) {
    rat <- t(vapply(subs, function(m) m[, f], numeric(k)))     # subjects x trials
    r <- icc(rat, model = model, type = type, unit = unit)
    icc_f[f] <- r$icc; ci_lo[f] <- r$ci_lower; ci_hi[f] <- r$ci_upper
  }
  structure(list(icc = icc_f, ci_lower = ci_lo, ci_upper = ci_hi,
                 mean_icc = mean(icc_f, na.rm = TRUE),
                 min_icc = min(icc_f, na.rm = TRUE),
                 model = model, type = type, n_subjects = ns, n_trials = k),
            class = "waveform_icc")
}

#' @export
print.waveform_icc <- function(x, ...) {
  cat(sprintf("Waveform ICC curve (%s, %s) -- %d subjects x %d trials, %d frames\n",
              x$model, x$type, x$n_subjects, x$n_trials, length(x$icc)))
  cat(sprintf("  mean ICC = %.3f   min ICC = %.3f\n", x$mean_icc, x$min_icc))
  invisible(x)
}

#' Waveform reliability report: CMC, ICC / SEM / MDC curves
#'
#' The integrated movement-waveform reliability summary. It combines the
#' within-subject repeatability (per-subject Kadaba [waveformCMC()]) with the
#' between-subject pointwise ICC curve and the frame-by-frame standard error of
#' measurement and minimal detectable change (from [sem()] /
#' [mdc()], the same definitions as the scalar clinimetrics).
#'
#' @param x A `[subject, trial, frame]` array, or a list of `trials x frames`
#'   matrices (one per subject).
#' @param model,type Passed to [icc()] for the pointwise ICC.
#' @param confidence Confidence level for the MDC (default 0.95).
#' @return a `waveform_reliability` object with per-subject `cmc`, the `icc`,
#'   `sem`, `mdc` curves (+ ICC CIs), and scalar summaries (`mean_cmc`,
#'   `mean_icc`, `peak_mdc`, ...). The SEM curve is the between-subject SD at each
#'   frame times sqrt(1 - ICC); the MDC curve is the 95% minimal detectable
#'   change in the measurement's own units.
#' @references Kadaba MP, et al. (1989) J Orthop Res 7:849-860; Weir JP (2005)
#'   J Strength Cond Res 19:231-240 (SEM/MDC).
#' @seealso [waveformCMC()], [waveformICC()]
#' @export
#' @examples
#' set.seed(1)
#' subs <- lapply(1:10, function(s) {
#'   base <- s + 5 * sin(seq(0, 2 * pi, length.out = 101))
#'   t(sapply(1:3, function(k) base + rnorm(101, 0, 0.3)))
#' })
#' r <- waveformReliability(subs)
#' r$mean_cmc; r$mean_icc; r$peak_mdc
waveformReliability <- function(x, model = "twoway", type = "agreement",
                                confidence = 0.95) {
  subs <- .wf_as_subject_list(x)
  ns <- length(subs); F <- ncol(subs[[1]])
  cmc <- vapply(subs, function(m) if (nrow(m) >= 2L) waveformCMC(m) else NA_real_,
                numeric(1))
  wi <- waveformICC(x, model = model, type = type)
  sem_f <- mdc_f <- numeric(F)
  for (f in seq_len(F)) {
    subj_means_f <- vapply(subs, function(m) mean(m[, f]), numeric(1))
    r <- min(max(wi$icc[f], 0), 1)
    sem_f[f] <- sem(subj_means_f, icc_value = r)
    mdc_f[f] <- mdc(sem_f[f], confidence = confidence)
  }
  structure(list(
    cmc = cmc, mean_cmc = mean(cmc, na.rm = TRUE),
    icc = wi$icc, icc_ci_lower = wi$ci_lower, icc_ci_upper = wi$ci_upper,
    sem = sem_f, mdc = mdc_f,
    mean_icc = wi$mean_icc, min_icc = wi$min_icc,
    peak_sem = max(sem_f), mean_sem = mean(sem_f),
    peak_mdc = max(mdc_f), mean_mdc = mean(mdc_f),
    n_subjects = ns, n_trials = wi$n_trials, n_frames = F,
    confidence = confidence), class = "waveform_reliability")
}

#' @export
print.waveform_reliability <- function(x, ...) {
  cat(sprintf("Waveform reliability -- %d subjects x %d trials, %d frames\n",
              x$n_subjects, x$n_trials, x$n_frames))
  cat(sprintf("  within-subject CMC : mean %.3f (range %.3f-%.3f)\n",
              x$mean_cmc, min(x$cmc, na.rm = TRUE), max(x$cmc, na.rm = TRUE)))
  cat(sprintf("  between-subject ICC: mean %.3f (min %.3f)\n", x$mean_icc, x$min_icc))
  cat(sprintf("  SEM curve          : mean %.3g, peak %.3g\n", x$mean_sem, x$peak_sem))
  cat(sprintf("  MDC%2.0f%% curve       : mean %.3g, peak %.3g\n",
              100 * x$confidence, x$mean_mdc, x$peak_mdc))
  invisible(x)
}
