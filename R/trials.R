# Trials: the interval table, per-stream coverage, and extraction.
#
# Situation A of CORE-TRIAL-01 -- trials as intervals inside one continuous
# recording. The tables live under keys in the container's clock, but they are
# NOT ancillary information: the clock's extra keys are preserved blindly, and a
# window that excludes a trial would otherwise leave the table still asserting
# it. The operations here and in timeWindow() are responsible for keeping them
# true.

.TRIAL_KEYS <- c("subject_id", "session_id", "trial_id")
.TRIAL_TABLE_KEYS <- c("trials", "trial_intervals", "recording_map",
                       "dropped_trials")
# Clock keys the operations are responsible for keeping true, rather than
# carrying across a window unchanged.
.MAINTAINED_CLOCK_KEYS <- c(.TRIAL_TABLE_KEYS, "stream_gaps")

# ---- gaps -------------------------------------------------------------------

#' Gaps in a series of sample times
#'
#' A step between consecutive sample times larger than \code{gap_factor} times the
#' nominal period counts as a gap. The factor is an adopted criterion -- it
#' separates acquisition jitter from a dropped sample; it does not prove that no
#' gap exists -- and it is the same criterion \pkg{PhysioCrossModal} uses to refuse
#' interpolating across one.
#'
#' Detection runs on the whole series **before** any window is applied, and the
#' window then selects which gaps to report. Selecting samples first and
#' differencing afterwards loses exactly the gaps that matter most: a dropout
#' straddling a window's edge has one of its two bounding samples outside the
#' window, so the step across it is never formed and the window looks intact.
#'
#' A gap that only touches an endpoint of the window is not in it. On a closed
#' interval the two share a single instant, and the recording does cover that
#' instant -- the same reading of "touching is not overlapping" that
#' \code{\link{trialIntervals}} uses.
#'
#' @param times Sample times in seconds, ascending.
#' @param rate Nominal sampling rate in Hz.
#' @param gap_factor Multiple of the nominal period above which a step counts as
#'   a gap.
#' @param window Optional \code{c(start, end)}: report only gaps overlapping it.
#' @param tolerance Overlap tolerance; defaults to the tolerance
#'   \code{\link{timeWindow}} uses for the same bounds.
#' @return A data frame of \code{start} and \code{end} -- the sample times the gap
#'   lies between, in the original series, not clipped to \code{window}.
#' @seealso \code{\link{streamCoverage}}, \code{\link{timeWindow}}
#' @export
#' @examples
#' detectGaps(c(0, 0.1, 0.2, 0.4, 0.5), rate = 10)
#' # the same gap, found from a window whose edge cuts through it
#' detectGaps(c(0, 0.1, 0.2, 0.4, 0.5), rate = 10, window = c(0.25, 0.45))
detectGaps <- function(times, rate, gap_factor = 1.5, window = NULL,
                       tolerance = NULL) {
  none <- data.frame(start = numeric(0), end = numeric(0))
  times <- as.numeric(times)
  if (length(times) < 2L || !is.finite(rate) || rate <= 0) return(none)
  d <- diff(times)
  big <- which(d > gap_factor / rate)
  if (!length(big)) return(none)
  out <- data.frame(start = times[big], end = times[big + 1L])
  if (is.null(window)) return(out)
  stopifnot(length(window) == 2L)
  lo <- min(window); hi <- max(window)
  tol <- if (is.null(tolerance)) .window_tol(lo, hi) else tolerance
  # strict overlap: a gap ending exactly at the window's start, or starting
  # exactly at its end, lies outside it
  keep <- out$start < hi - tol & out$end > lo + tol
  out[keep, , drop = FALSE]
}

# ---- coverage ---------------------------------------------------------------

#' Coverage of a time span by a stream's samples
#'
#' Reports how much of \code{[start, end]} a stream actually covers. The
#' distinction the return value keeps is between what was **observed** and what
#' was **derived**: \code{n_present} is a count of real samples, while
#' \code{n_expected} and \code{coverage} depend on an assumed sampling scheme and
#' are \code{NA} when that scheme cannot decide them.
#'
#' Gaps are reported as intervals, not as a shortfall in a count, so a caller can
#' see *where* the recording stops. A step between measured sample times larger
#' than \code{gap_factor} times the nominal period counts as a gap; that factor
#' is an adopted criterion shared with \pkg{PhysioCrossModal}'s interpolation
#' guard, and it separates acquisition jitter from a dropped sample rather than
#' proving that no gap exists.
#'
#' For a stream with no measured times the sample times are reconstructed from
#' the start offset and the rate, so no gap can be found. That is a consequence
#' of the reconstruction, not a measurement, and \code{rule$basis} says so.
#'
#' \code{n_expected} counts the points of a reference grid that fall inside the
#' interval. It is not \code{(end - start) * rate}, which measures a span rather
#' than a grid and overstates the count whenever the bounds do not land on sample
#' times -- a whole, evenly sampled stream would then read as incomplete for being
#' asked about off its own sample times. The grid has step \code{1/rate} and is
#' anchored on the stream's first sample time; for a measured stream that anchor
#' is an assumption, and \code{rule$anchor}, \code{rule$step} and
#' \code{rule$basis} report which grid was used.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param stream Stream name.
#' @param start,end Bounds in seconds on the shared clock (closed interval, the
#'   same meaning and tolerance as \code{\link{timeWindow}}).
#' @param rule Name of the rule for \code{n_expected}. \code{"nominal"} counts
#'   the grid points of step \code{1/rate} inside the interval, anchored on the
#'   stream's first sample time and using the same tolerance as
#'   \code{\link{timeWindow}}. Any other value reports \code{n_expected} and
#'   \code{coverage} as \code{NA}.
#' @param gap_factor Multiple of the nominal period above which a step counts as
#'   a gap.
#' @return A list with \code{n_present}, \code{n_expected}, \code{coverage},
#'   \code{gaps} (a data frame of \code{start}/\code{end}) and \code{rule}
#'   (its \code{name}, \code{gap_factor}, the grid \code{anchor} and
#'   \code{step}, and the \code{basis} the expected count rests on).
#' @seealso \code{\link{timeWindow}}, \code{\link{hasMeasuredTimes}}
#' @export
#' @examples
#' pe <- PhysioExperiment(assays = list(raw = matrix(1:5, ncol = 1)),
#'                        samplingRate = 10,
#'                        rowData = S4Vectors::DataFrame(
#'                          time_from_t0 = c(0, 0.1, 0.2, 1.2, 1.3)))
#' streamCoverage(MultiPhysioExperiment(a = pe), "a", 0, 1.3)
streamCoverage <- function(x, stream, start, end, rule = "nominal",
                           gap_factor = 1.5) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  if (!stream %in% names(x@streams)) {
    stop(sprintf("unknown stream '%s'", stream), call. = FALSE)
  }
  if (!is.numeric(start) || !is.numeric(end) || length(start) != 1L ||
      length(end) != 1L || !is.finite(start) || !is.finite(end)) {
    stop("'start' and 'end' must be finite numeric scalars", call. = FALSE)
  }
  if (start > end) {
    stop(sprintf("'start' (%g) must not be greater than 'end' (%g)", start, end),
         call. = FALSE)
  }
  measured <- hasMeasuredTimes(x, stream)
  tt <- streamTimeIndex(x, stream)
  rate <- as.numeric(samplingRate(x@streams[[stream]]))
  tol <- .window_tol(start, end)
  sel <- which(tt >= start - tol & tt <= end + tol)

  # The expected count is how many points of the reference grid fall inside the
  # interval -- not (end - start) * rate, which counts a span rather than a grid
  # and so overestimates whenever the bounds do not land on sample times. The
  # grid is anchored on the stream's first sample time, which for a measured
  # stream is an assumption and is reported as one.
  anchor <- if (length(tt)) tt[1L] else NA_real_
  usable <- identical(rule, "nominal") && is.finite(rate) && rate > 0 &&
    is.finite(anchor)
  n_expected <- if (usable) {
    lo <- (start - tol - anchor) * rate
    hi <- (end + tol - anchor) * rate
    eps <- 1e-9 * max(1, abs(lo), abs(hi))
    as.integer(max(0, floor(hi + eps) - ceiling(lo - eps) + 1))
  } else NA_integer_

  # Detection runs on the stream's whole series and the window then selects,
  # so a gap straddling an edge of the window is still found: its outer
  # bounding sample is outside the window, and differencing the selection alone
  # would never form the step across it.
  gaps <- if (measured) {
    detectGaps(tt, rate, gap_factor, window = c(start, end), tolerance = tol)
  } else data.frame(start = numeric(0), end = numeric(0))

  # Gaps an earlier window recorded. Subsetting removes the sample that bounded
  # the gap from outside, so after timeWindow() the evidence is no longer in the
  # series and the record is the only place it survives.
  carried <- .carried_gaps(x, stream, start, end, rate, gap_factor, tol)
  if (nrow(carried$usable)) {
    gaps <- rbind(gaps, carried$usable)
    key <- sprintf("%.12g/%.12g", gaps$start, gaps$end)
    gaps <- gaps[!duplicated(key), , drop = FALSE]
    gaps <- gaps[order(gaps$start), , drop = FALSE]
  }
  row.names(gaps) <- NULL

  list(
    n_present = length(sel),
    n_expected = n_expected,
    coverage = if (is.na(n_expected) || n_expected <= 0) NA_real_
               else length(sel) / n_expected,
    gaps = gaps,
    rule = list(
      name = rule,
      gap_factor = gap_factor,
      anchor = if (usable) anchor else NA_real_,
      step = if (usable) 1 / rate else NA_real_,
      carried_gaps = nrow(carried$usable),
      unusable_carried_gaps = nrow(carried$unusable),
      basis = if (!usable) {
        "no reference grid could be assumed, so nothing is expected"
      } else if (measured) {
        sprintf(paste0("measured sample times, counted against a grid of step ",
                       "%g s with its anchor assumed at the stream's first ",
                       "measured sample (t = %g)"), 1 / rate, anchor)
      } else {
        sprintf(paste0("reconstructed from the start offset and the rate (grid ",
                       "anchor %g, step %g s); no gap can be detected, which is ",
                       "not a measurement"), anchor, 1 / rate)
      },
      carried_basis = if (!nrow(carried$usable) && !nrow(carried$unusable)) {
        NULL
      } else if (nrow(carried$unusable)) {
        sprintf(paste0("%d gap(s) recorded by an earlier window were detected ",
                       "under a different rate or gap_factor and cannot be ",
                       "re-derived here, because the sample that bounded them ",
                       "from outside is no longer in the series"),
                nrow(carried$unusable))
      } else {
        sprintf("%d gap(s) came from the record an earlier window left",
                nrow(carried$usable))
      }))
}

# Gaps recorded when an earlier window removed the evidence for them. A recorded
# row can only be reused when it was detected under the same rate and the same
# gap_factor; rows that were not are reported as unusable rather than dropped,
# since the series can no longer decide them either way.
.carried_gaps <- function(x, stream, start, end, rate, gap_factor, tol) {
  empty <- data.frame(start = numeric(0), end = numeric(0))
  sg <- x@clock$stream_gaps
  if (is.null(sg) || !nrow(sg)) return(list(usable = empty, unusable = empty))
  mine <- sg[as.character(sg$stream) == as.character(stream), , drop = FALSE]
  if (!nrow(mine)) return(list(usable = empty, unusable = empty))
  inwin <- mine$start < end - tol & mine$end > start + tol
  mine <- mine[inwin, , drop = FALSE]
  if (!nrow(mine)) return(list(usable = empty, unusable = empty))
  same <- rep(TRUE, nrow(mine))
  if ("gap_factor" %in% names(mine)) {
    same <- same & is.finite(mine$gap_factor) & mine$gap_factor == gap_factor
  }
  if ("rate" %in% names(mine)) {
    same <- same & is.finite(mine$rate) & mine$rate == rate
  }
  list(usable = mine[same, c("start", "end"), drop = FALSE],
       unusable = mine[!same, c("start", "end"), drop = FALSE])
}

# Record the gaps a window is about to make undetectable. Detection runs on the
# pre-window series; rows the previous container already carried are kept, since
# a second window cannot re-derive them either.
.window_stream_gaps <- function(out, x, start, end, gap_factor = 1.5) {
  keep <- names(out@streams)
  if (!length(keep)) return(out)
  tol <- .window_tol(start, end)
  rows <- lapply(keep, function(nm) {
    rate <- as.numeric(samplingRate(x@streams[[nm]]))
    found <- if (isTRUE(tryCatch(hasMeasuredTimes(x, nm), error = function(e) FALSE))) {
      tt <- tryCatch(streamTimeIndex(x, nm), error = function(e) numeric(0))
      detectGaps(tt, rate, gap_factor, window = c(start, end), tolerance = tol)
    } else data.frame(start = numeric(0), end = numeric(0))
    prior <- x@clock$stream_gaps
    prior <- if (is.null(prior) || !nrow(prior)) NULL else {
      p <- prior[as.character(prior$stream) == nm, , drop = FALSE]
      p <- p[p$start < end - tol & p$end > start + tol, , drop = FALSE]
      if (nrow(p)) p[, c("start", "end"), drop = FALSE] else NULL
    }
    all_rows <- rbind(found, prior)
    if (!nrow(all_rows)) return(NULL)
    key <- sprintf("%.12g/%.12g", all_rows$start, all_rows$end)
    all_rows <- all_rows[!duplicated(key), , drop = FALSE]
    data.frame(stream = nm, start = all_rows$start, end = all_rows$end,
               gap_factor = gap_factor, rate = rate, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) {
    tb <- do.call(rbind, rows)
    row.names(tb) <- NULL
    out@clock$stream_gaps <- tb
  }
  out
}

# ---- the tables -------------------------------------------------------------

.as_table <- function(value, what) {
  if (is.null(value)) return(NULL)
  df <- as.data.frame(value, stringsAsFactors = FALSE)
  if (!nrow(df)) return(df)
  df
}

.require_cols <- function(df, cols, what) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stop(sprintf("%s is missing column(s): %s", what, paste(miss, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

.require_unique <- function(df, cols, what) {
  key <- do.call(paste, c(unname(df[cols]), sep = "\r"))
  if (anyDuplicated(key)) {
    dup <- unique(key[duplicated(key)])
    stop(sprintf("%s must be unique on (%s); duplicate rows: %d",
                 what, paste(cols, collapse = ", "), length(dup)), call. = FALSE)
  }
  invisible(TRUE)
}

#' Trials recorded in a container
#'
#' The trial table records that a trial happened. It is keyed on
#' \code{(subject_id, session_id, trial_id)} and is unique on that composite:
#' \code{trial_id} alone is not assumed unique across subjects.
#'
#' A trial that took place outside the recording is carried here with
#' \code{observed = FALSE} rather than as an interval with out-of-range times.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param value A data frame with \code{subject_id}, \code{session_id},
#'   \code{trial_id} and optionally \code{condition} and \code{observed}.
#' @return The trial table, or the modified container.
#' @seealso \code{\link{trialIntervals}}, \code{\link{trial}}
#' @export
trials <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  x@clock$trials
}

#' @rdname trials
#' @export
`trials<-` <- function(x, value) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  df <- .as_table(value, "trials")
  if (!is.null(df) && nrow(df)) {
    .require_cols(df, .TRIAL_KEYS, "the trial table")
    .require_unique(df, .TRIAL_KEYS, "the trial table")
    if (!"observed" %in% names(df)) df$observed <- TRUE
  }
  x@clock$trials <- df
  x
}

#' Trial intervals on a stream
#'
#' The time span each trial occupies, per recording and per stream. Because the
#' same trial can be captured by several devices with different spans, the
#' interval table is keyed on
#' \code{(subject_id, session_id, trial_id, recording_id, stream)}.
#'
#' An interval that contains a gap **is stored**: the trial happened, and
#' refusing it would erase that. Whether the span can be analysed is a separate
#' question, answered by \code{\link{streamCoverage}} at the point of use.
#' Invalid input is refused: reversed or non-finite bounds, an unknown stream, a
#' duplicate key, or an interval lying wholly outside the recording.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param value A data frame with the five key columns plus \code{start} and
#'   \code{end} in seconds on the shared clock.
#' @return The interval table, or the modified container.
#' @seealso \code{\link{trials}}, \code{\link{streamCoverage}}
#' @export
trialIntervals <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  it <- x@clock$trial_intervals
  if (is.null(it)) {
    return(data.frame(subject_id = character(0), session_id = character(0),
                      trial_id = character(0), recording_id = character(0),
                      stream = character(0), start = numeric(0), end = numeric(0),
                      stringsAsFactors = FALSE))
  }
  it
}

#' @rdname trialIntervals
#' @param allow_overlap Overlapping intervals on the same recording and stream
#'   are refused by default, because two trials claiming the same samples is
#'   usually a mistake in the table rather than a property of the experiment.
#'   Pass a non-empty character reason to allow them; the reason is kept on the
#'   table so the decision is visible later.
#' @export
`trialIntervals<-` <- function(x, allow_overlap = FALSE, value) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  df <- .as_table(value, "trial intervals")
  if (!is.null(df) && nrow(df)) {
    keys <- c(.TRIAL_KEYS, "recording_id", "stream")
    .require_cols(df, c(keys, "start", "end"), "the interval table")
    .require_unique(df, keys, "the interval table")

    if (any(!is.finite(df$start)) || any(!is.finite(df$end))) {
      stop("interval 'start' and 'end' must be finite", call. = FALSE)
    }
    bad <- which(df$start > df$end)
    if (length(bad)) {
      stop(sprintf(paste0("interval 'start' must not be greater than 'end'; ",
                          "offending trial(s): %s"),
                   paste(unique(df$trial_id[bad]), collapse = ", ")), call. = FALSE)
    }
    unknown <- setdiff(unique(as.character(df$stream)), names(x@streams))
    if (length(unknown)) {
      stop(sprintf("the interval table names stream(s) that do not exist: %s",
                   paste(unknown, collapse = ", ")), call. = FALSE)
    }
    # wholly outside the recording: the trial cannot be an interval of this
    # stream. Use trials(x)$observed = FALSE for a trial that happened elsewhere.
    for (i in seq_len(nrow(df))) {
      tt <- streamTimeIndex(x, as.character(df$stream[i]))
      tol <- .window_tol(df$start[i], df$end[i])
      if (df$end[i] < tt[1] - tol || df$start[i] > tt[length(tt)] + tol) {
        stop(sprintf(paste0("trial '%s' spans [%g, %g] s, wholly outside stream ",
                            "'%s' which covers [%g, %g] s. A trial that happened ",
                            "outside the recording belongs in trials() with ",
                            "observed = FALSE, not in the interval table."),
                     df$trial_id[i], df$start[i], df$end[i], df$stream[i],
                     tt[1], tt[length(tt)]), call. = FALSE)
      }
    }
  }
  if (!is.null(df) && nrow(df) > 1L) {
    ov <- .find_overlaps(df)
    if (nrow(ov)) {
      if (!is.character(allow_overlap) || !nzchar(allow_overlap[[1]])) {
        stop(sprintf(paste0("these intervals overlap on the same recording and ",
                            "stream: %s. Two trials claiming the same samples is ",
                            "usually a mistake in the table. If it is intended, ",
                            "pass allow_overlap = \"<reason>\"."),
                     paste(sprintf("%s/%s on %s", ov$a, ov$b, ov$stream),
                           collapse = "; ")), call. = FALSE)
      }
      attr(df, "overlap_allowed") <- allow_overlap[[1]]
    }
  }
  x@clock$trial_intervals <- df
  x
}

# Overlapping pairs within the same recording and stream. Touching intervals --
# one ending exactly where the next begins -- are not overlaps: on a closed
# interval they share a single instant, and calling that an overlap would make
# back-to-back trials unrepresentable.
.find_overlaps <- function(df) {
  out <- data.frame(a = character(0), b = character(0), stream = character(0),
                    stringsAsFactors = FALSE)
  key <- paste(df$subject_id, df$session_id, df$recording_id, df$stream,
               sep = "\r")
  for (k in unique(key)) {
    idx <- which(key == k)
    if (length(idx) < 2L) next
    sub <- df[idx, , drop = FALSE]
    o <- order(sub$start)
    sub <- sub[o, , drop = FALSE]
    for (i in seq_len(nrow(sub) - 1L)) {
      tol <- .window_tol(sub$start[i], sub$end[i])
      if (sub$start[i + 1L] < sub$end[i] - tol) {
        out <- rbind(out, data.frame(
          a = as.character(sub$trial_id[i]), b = as.character(sub$trial_id[i + 1L]),
          stream = as.character(sub$stream[i]), stringsAsFactors = FALSE))
      }
    }
  }
  out
}

# ---- extraction -------------------------------------------------------------

#' Extract one trial
#'
#' Selects the trial's span with \code{\link{timeWindow}}, so the selection, the
#' boundary tolerance, the clock and the provenance all follow the same rules as
#' any other window.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param id Trial identifier.
#' @param streams Optional stream names to keep.
#' @param ... Passed to \code{\link{timeWindow}}.
#' @return A \code{MultiPhysioExperiment} covering the trial.
#' @seealso \code{\link{trialIntervals}}, \code{\link{timeWindow}}
#' @export
trial <- function(x, id, streams = NULL, ...) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  it <- trialIntervals(x)
  rows <- it[as.character(it$trial_id) == as.character(id), , drop = FALSE]
  if (!nrow(rows)) {
    stop(sprintf("no interval recorded for trial '%s'", id), call. = FALSE)
  }
  # several devices can hold the same trial over different spans; the window is
  # their union, and each stream then keeps only what it actually covers
  timeWindow(x, min(rows$start), max(rows$end), streams = streams, ...)
}

# ---- keeping the tables true through a window -------------------------------

# Update the trial tables for a window. Partial trials are kept, truncated to
# the window and marked, because the trial happened; `complete_only` keeps whole
# trials instead and records why the others went. Trials with no overlap are
# dropped, and what was dropped is always recorded.
.window_trial_tables <- function(out, x, start, end, complete_only = FALSE) {
  it <- x@clock$trial_intervals
  tr <- x@clock$trials
  if (is.null(it) && is.null(tr)) return(out)

  dropped <- data.frame(trial_id = character(0), reason = character(0),
                        stringsAsFactors = FALSE)
  if (!is.null(it) && nrow(it)) {
    tol <- .window_tol(start, end)
    keep <- it$end >= start - tol & it$start <= end + tol
    gone <- it[!keep, , drop = FALSE]
    if (nrow(gone)) {
      dropped <- rbind(dropped, data.frame(
        trial_id = as.character(gone$trial_id),
        reason = sprintf("no overlap with [%g, %g]", start, end),
        stringsAsFactors = FALSE))
    }
    sub <- it[keep, , drop = FALSE]
    if (nrow(sub)) {
      sub$original_start <- sub$start
      sub$original_end <- sub$end
      sub$truncated_start <- sub$start < start - tol
      sub$truncated_end <- sub$end > end + tol
      if (isTRUE(complete_only)) {
        partial <- sub$truncated_start | sub$truncated_end
        if (any(partial)) {
          dropped <- rbind(dropped, data.frame(
            trial_id = as.character(sub$trial_id[partial]),
            reason = "partial: not wholly inside the window",
            stringsAsFactors = FALSE))
        }
        sub <- sub[!partial, , drop = FALSE]
      } else {
        sub$start <- pmax(sub$start, start)
        sub$end <- pmin(sub$end, end)
      }
    }
    out@clock$trial_intervals <- sub
    if (!is.null(tr) && nrow(tr)) {
      out@clock$trials <- tr[as.character(tr$trial_id) %in%
                              as.character(sub$trial_id), , drop = FALSE]
    }
  }
  out@clock$dropped_trials <- dropped
  out
}

# ---- situation B: independently recorded trials -----------------------------
#
# When each trial is its own recording, what matters is not an absolute origin
# but whether the recordings can be placed relative to each other. Those are
# different facts: a container can have t0 = NA and still support relative
# extraction within a trial, and it can carry a t0 while nothing relates one
# recording to the next.

.TIME_CORRESPONDENCE <- c("measured", "order_only", "unknown")

#' How trials map onto recordings
#'
#' One trial can be captured by several devices, so this table allows more than
#' one row per trial and is unique on
#' \code{(subject_id, session_id, trial_id, recording_id)}.
#'
#' \code{time_correspondence} states what is known about placing the recordings
#' relative to one another:
#' \describe{
#'   \item{\code{"measured"}}{The relative times were measured - a shared trigger,
#'     say. \code{reference} names what they are measured against (a common clock
#'     or a reference \code{recording_id}) and \code{offset} gives the value in
#'     seconds. Elapsed time between trials may be computed.}
#'   \item{\code{"order_only"}}{Only the order is known. Elapsed time may not be
#'     computed - row order is not a duration.}
#'   \item{\code{"unknown"}}{Nothing relates the recordings.}
#' }
#'
#' A row may declare \code{"measured"} while leaving \code{reference} or
#' \code{offset} missing: the table records what was declared, and
#' \code{\link{elapsedBetweenTrials}} refuses to compute from it. The state name
#' alone never authorises a duration.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param value A data frame with the four key columns,
#'   \code{time_correspondence}, and \code{reference} / \code{offset}.
#' @return The recording map, or the modified container.
#' @seealso \code{\link{elapsedBetweenTrials}}, \code{\link{trials}}
#' @export
recordingMap <- function(x) {
  if (inherits(x, "physio_trial_set")) {
    rm_ <- x$map
  } else {
    stopifnot(methods::is(x, "MultiPhysioExperiment"))
    rm_ <- x@clock$recording_map
  }
  if (is.null(rm_)) {
    return(data.frame(subject_id = character(0), session_id = character(0),
                      trial_id = character(0), recording_id = character(0),
                      time_correspondence = character(0),
                      reference = character(0), offset = numeric(0),
                      stringsAsFactors = FALSE))
  }
  rm_
}

#' @rdname recordingMap
#' @export
`recordingMap<-` <- function(x, value) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  df <- .as_table(value, "recording map")
  if (!is.null(df) && nrow(df)) {
    keys <- c(.TRIAL_KEYS, "recording_id")
    .require_cols(df, c(keys, "time_correspondence"), "the recording map")
    .require_unique(df, keys, "the recording map")
    bad <- setdiff(unique(as.character(df$time_correspondence)),
                   .TIME_CORRESPONDENCE)
    if (length(bad)) {
      stop(sprintf("unknown time_correspondence: %s (expected one of %s)",
                   paste(bad, collapse = ", "),
                   paste(.TIME_CORRESPONDENCE, collapse = ", ")), call. = FALSE)
    }
    if (!"reference" %in% names(df)) df$reference <- NA_character_
    if (!"offset" %in% names(df)) df$offset <- NA_real_
  }
  x@clock$recording_map <- df
  x
}

#' Elapsed time between two trials
#'
#' Returns the seconds between the starts of two independently recorded trials.
#'
#' This is refused unless the recordings are actually placed relative to one
#' another. Declaring \code{time_correspondence = "measured"} is not enough: the
#' rows must also carry a \code{reference} and an \code{offset}, and both trials
#' must be measured against the *same* reference, since offsets on different
#' references are not comparable. \code{"order_only"} and \code{"unknown"} are
#' refused outright - the order in which recordings were made is not a duration,
#' and turning one into the other would fabricate a measurement.
#'
#' An absent absolute origin (\code{clock$t0} of \code{NA}) is a different matter
#' and does not block this: what is needed is the correspondence between
#' recordings, not an absolute time.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param from,to Trial identifiers.
#' @return Numeric seconds from \code{from} to \code{to}.
#' @seealso \code{\link{recordingMap}}
#' @export
elapsedBetweenTrials <- function(x, from, to) {
  if (!inherits(x, "physio_trial_set")) {
    stopifnot(methods::is(x, "MultiPhysioExperiment"))
  }
  map <- recordingMap(x)
  pick <- function(id) {
    rows <- map[as.character(map$trial_id) == as.character(id), , drop = FALSE]
    if (!nrow(rows)) {
      stop(sprintf("trial '%s' is not in the recording map", id), call. = FALSE)
    }
    rows
  }
  a <- pick(from); b <- pick(to)

  states <- unique(c(as.character(a$time_correspondence),
                     as.character(b$time_correspondence)))
  blocked <- intersect(states, c("order_only", "unknown"))
  if (length(blocked)) {
    stop(sprintf(paste0("elapsed time between '%s' and '%s' is not available: ",
                        "the recordings are related by '%s'. Only the order of ",
                        "these recordings is established, and an order is not a ",
                        "duration; supply measured offsets against a common ",
                        "reference to compute it."),
                 from, to, paste(blocked, collapse = "', '")), call. = FALSE)
  }

  # a trial captured by several devices gives several rows; they must agree
  one <- function(rows, id) {
    ref <- unique(as.character(rows$reference))
    off <- unique(as.numeric(rows$offset))
    if (any(is.na(ref)) || !length(ref) || any(!is.finite(off)) || !length(off)) {
      stop(sprintf(paste0("trial '%s' declares time_correspondence = \"measured\" ",
                          "but is missing its reference or offset. A state name ",
                          "alone does not establish a time correspondence."),
                   id), call. = FALSE)
    }
    if (length(ref) > 1L) {
      stop(sprintf("trial '%s' is measured against several references: %s",
                   id, paste(ref, collapse = ", ")), call. = FALSE)
    }
    if (length(off) > 1L) {
      stop(sprintf(paste0("trial '%s' has inconsistent offsets against reference ",
                          "'%s': %s"), id, ref,
                   paste(format(off), collapse = ", ")), call. = FALSE)
    }
    list(reference = ref, offset = off)
  }
  A <- one(a, from); B <- one(b, to)

  if (!identical(A$reference, B$reference)) {
    stop(sprintf(paste0("'%s' is measured against reference '%s' and '%s' against ",
                        "'%s'. Offsets on different references are not ",
                        "comparable; convert them to a common reference first."),
                 from, A$reference, to, B$reference), call. = FALSE)
  }
  B$offset - A$offset
}

# ---- aggregation over trials ------------------------------------------------

#' Aggregate per-trial features, choosing what counts as a trial
#'
#' Selects rows from the trial table and hands them to
#' \code{\link{aggregatePhysioFeatures}}. The aggregation itself - the equal
#' weighting of immediate children at each level, the link back to source rows,
#' the checksum - is unchanged: choosing which trials take part is a selection of
#' input rows, not a reweighting.
#'
#' Each trial is classified before anything is averaged, because averaging a
#' truncated trial together with whole ones silently mixes different amounts of
#' evidence:
#'
#' \describe{
#'   \item{\code{"complete"}}{Every stream the trial names covers it fully.}
#'   \item{\code{"partial"}}{A stream covers less of the interval than it asks
#'     for. The interval was cut by a window, or it runs past the end of the
#'     recording, or \code{\link{streamCoverage}} reports a dropout inside it.}
#'   \item{\code{"unknown"}}{Completeness cannot be decided here. Coverage is
#'     undecidable; or a gap an earlier window recorded cannot be re-derived under
#'     this query's rate or \code{gap_factor}, so the dropout can be neither
#'     confirmed nor ruled out; or the stream holds more samples in the interval
#'     than its declared rate predicts -- nothing is missing, but the grid does not
#'     describe it. None of these is the same as complete.}
#'   \item{\code{"undescribed"}}{No interval is recorded for the trial, so
#'     nothing describes its extent. Excluded by default: a trial table of
#'     per-trial values with no intervals is a job for
#'     \code{\link{aggregatePhysioFeatures}} directly, or ask for
#'     \code{include = "undescribed"} here.}
#' }
#'
#' Every judgement comes from \code{\link{streamCoverage}} -- the shortfall, the
#' dropout it reports, and what it says it could not re-derive -- so a trial is
#' excluded by the same rule that reports the problem rather than by a second
#' definition of it.
#'
#' The states are decided in that order: shown to be short, then unable to tell,
#' then shown to be whole. Losing the evidence for a gap therefore never promotes
#' a trial to \code{"complete"} -- a question that can no longer be answered is not
#' an answer of yes -- while a shortfall this query can still demonstrate outranks
#' a record it cannot reuse.
#'
#' @param x A \code{MultiPhysioExperiment} carrying a trial table.
#' @param level Target level, passed to \code{\link{aggregatePhysioFeatures}}.
#' @param features Feature column names on the trial table.
#' @param include Which states take part: any of \code{"complete"},
#'   \code{"partial"}, \code{"unknown"}, \code{"undescribed"}. Defaults to
#'   \code{"complete"} -- only trials shown to be whole take part unless another
#'   state is named.
#' @param rule,gap_factor Passed to \code{\link{streamCoverage}} when judging
#'   completeness.
#' @param ... Passed to \code{\link{aggregatePhysioFeatures}}.
#' @return A list with \code{aggregate} (the aggregation result), \code{counts}
#'   (one entry per state plus \code{n_included} / \code{n_excluded}),
#'   \code{states} (the state of every trial) and \code{excluded}.
#' @seealso \code{\link{aggregatePhysioFeatures}}, \code{\link{trialIntervals}},
#'   \code{\link{streamCoverage}}
#' @export
aggregateTrials <- function(x, level, features = NULL,
                            include = "complete",
                            rule = "nominal", gap_factor = 1.5, ...) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  include <- match.arg(include, .TRIAL_STATES, several.ok = TRUE)
  tr <- trials(x)
  if (is.null(tr) || !nrow(tr)) {
    stop("no trial table on this container; set trials(x) first", call. = FALSE)
  }
  ids <- as.character(tr$trial_id)
  state <- .trial_states(x, ids, rule = rule, gap_factor = gap_factor)

  keep <- state %in% include
  excluded <- data.frame(
    trial_id = ids[!keep],
    state = state[!keep],
    reason = .TRIAL_STATE_REASONS[state[!keep]],
    stringsAsFactors = FALSE)
  row.names(excluded) <- NULL

  if (!any(keep)) {
    hint <- if (all(state == "undescribed")) {
      paste0(" No interval is recorded for any of them, so completeness cannot ",
             "be shown; pass include = \"undescribed\" if that is what you ",
             "mean, or aggregate the table with aggregatePhysioFeatures() ",
             "directly.")
    } else ""
    tb <- table(state)
    stop(sprintf(paste0("no trial satisfies include = %s. The %d trial(s) are ",
                        "%s.%s"),
                 paste(sQuote(include), collapse = ", "), length(ids),
                 paste(sprintf("%d %s", as.integer(tb), names(tb)),
                       collapse = ", "), hint),
         call. = FALSE)
  }

  agg <- aggregatePhysioFeatures(tr[keep, , drop = FALSE], level, features, ...)
  counts <- as.list(stats::setNames(
    vapply(.TRIAL_STATES, function(s) sum(state == s), integer(1)),
    paste0("n_", .TRIAL_STATES)))
  counts$n_included <- sum(keep)
  counts$n_excluded <- sum(!keep)
  list(
    aggregate = agg,
    counts = counts,
    states = data.frame(trial_id = ids, state = state,
                        stringsAsFactors = FALSE, row.names = NULL),
    excluded = excluded)
}

.TRIAL_STATES <- c("complete", "partial", "unknown", "undescribed")

.TRIAL_STATE_REASONS <- c(
  complete    = "complete",
  partial     = paste("partial: cut by a window, or a stream covers less of the",
                      "interval than it asks for"),
  unknown     = paste("unknown: the trial has intervals but their coverage",
                      "cannot be decided"),
  undescribed = "undescribed: no interval is recorded for the trial")

# Classify each trial as complete / partial / unknown / undescribed. Partiality
# is measured with streamCoverage() so that one definition of a dropout serves
# both reporting and selection; an interval whose coverage cannot be computed
# makes the trial unknown rather than quietly complete.
.trial_states <- function(x, ids, rule = "nominal", gap_factor = 1.5) {
  it <- trialIntervals(x)
  if (is.null(it)) it <- data.frame()
  flag <- function(rows, col) {
    if (col %in% names(rows)) isTRUE(any(as.logical(rows[[col]]), na.rm = TRUE)) else FALSE
  }
  vapply(ids, function(id) {
    rows <- if (nrow(it)) it[as.character(it$trial_id) == id, , drop = FALSE] else it
    if (!nrow(rows)) return("undescribed")
    if (flag(rows, "truncated_start") || flag(rows, "truncated_end")) return("partial")
    cv <- lapply(seq_len(nrow(rows)), function(i) {
      tryCatch(streamCoverage(x, as.character(rows$stream[i]), rows$start[i],
                              rows$end[i], rule = rule, gap_factor = gap_factor),
               error = function(e) NULL)
    })
    if (any(vapply(cv, is.null, logical(1)))) return("unknown")
    n_exp <- vapply(cv, function(z) as.numeric(z$n_expected), numeric(1))
    n_pre <- vapply(cv, function(z) as.numeric(z$n_present), numeric(1))
    n_gap <- vapply(cv, function(z) nrow(z$gaps), integer(1))
    n_bad <- vapply(cv, function(z) as.integer(z$rule$unusable_carried_gaps %||% 0L),
                    integer(1))

    # The order matters, and it is: shown to be short, then unable to tell, then
    # shown to be whole. Losing the evidence for a gap must never promote a trial
    # to complete -- a question that can no longer be answered is not an answer of
    # "yes".

    # 1. partial, on evidence this query can use. A reported dropout is a
    #    dropout: the sample total can be made up by off-grid samples elsewhere
    #    in the interval and still leave a hole in it.
    if (any(n_gap > 0L)) return("partial")
    if (any(!is.na(n_exp) & n_pre < n_exp)) return("partial")

    # 2. unknown, because something needed is missing or undecidable. A gap an
    #    earlier window recorded under a different rate or gap_factor cannot be
    #    re-derived here -- the sample that bounded it from outside is gone from
    #    the series -- so this query can neither confirm nor rule it out.
    if (any(n_bad > 0L)) return("unknown")
    if (any(is.na(n_exp))) return("unknown")
    # more samples than the declared grid predicts: nothing is missing, but the
    # grid does not describe the stream, so full coverage is not established
    if (any(n_pre > n_exp)) return("unknown")

    # 3. complete: the evidence is present and it shows the interval is whole.
    "complete"
  }, character(1), USE.NAMES = FALSE)
}

# ---- cross-stream coverage of a trial ---------------------------------------

#' Coverage of one trial across every stream it names
#'
#' A trial is usually captured by more than one stream, and they need not cover
#' it equally: one may have a dropout the others do not. This reports
#' \code{\link{streamCoverage}} for each stream the trial's intervals name, so a
#' caller can see whether a cross-stream comparison over that trial is
#' defensible before making one.
#'
#' The returned table carries a \code{consistent} attribute: \code{FALSE} when
#' the streams do not agree about how much of the trial they cover. Disagreement
#' is surfaced rather than resolved -- which stream to trust is not something this
#' function can decide.
#'
#' @param x A \code{MultiPhysioExperiment} carrying trial intervals.
#' @param id Trial identifier.
#' @param rule,gap_factor Passed to \code{\link{streamCoverage}}.
#' @param tolerance Coverage difference below which the streams count as
#'   agreeing.
#' @return A data frame with one row per stream: \code{stream}, \code{start},
#'   \code{end}, \code{n_present}, \code{n_expected}, \code{coverage},
#'   \code{n_gaps} and \code{n_unusable_carried_gaps} -- dropouts an earlier
#'   window recorded that this query cannot re-derive, and which therefore leave
#'   completeness undecided rather than confirmed.
#' @seealso \code{\link{streamCoverage}}, \code{\link{trialIntervals}}
#' @export
trialCoverage <- function(x, id, rule = "nominal", gap_factor = 1.5,
                          tolerance = 1e-6) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  it <- trialIntervals(x)
  rows <- it[as.character(it$trial_id) == as.character(id), , drop = FALSE]
  if (!nrow(rows)) {
    stop(sprintf("no interval recorded for trial '%s'", id), call. = FALSE)
  }
  out <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    cv <- streamCoverage(x, as.character(rows$stream[i]), rows$start[i],
                         rows$end[i], rule = rule, gap_factor = gap_factor)
    data.frame(stream = as.character(rows$stream[i]),
               start = rows$start[i], end = rows$end[i],
               n_present = cv$n_present, n_expected = cv$n_expected,
               coverage = cv$coverage, n_gaps = nrow(cv$gaps),
               # a dropout an earlier window recorded that this query cannot
               # re-derive: reported here too, so the table and the completeness
               # judgement rest on the same facts
               n_unusable_carried_gaps =
                 as.integer(cv$rule$unusable_carried_gaps %||% 0L),
               stringsAsFactors = FALSE)
  }))
  cov <- out$coverage[is.finite(out$coverage)]
  attr(out, "consistent") <- length(cov) < 2L ||
    (max(cov) - min(cov)) <= tolerance
  attr(out, "rule") <- list(name = rule, gap_factor = gap_factor,
                            tolerance = tolerance)
  out
}

# ---- trial-aware queries on the hierarchy -----------------------------------

.session_trials <- function(s, label) {
  tr <- tryCatch(trials(s), error = function(e) NULL)
  if (is.null(tr) || !nrow(tr)) return(NULL)
  cbind(session = label, tr, row.names = NULL)
}

.empty_trial_listing <- function(extra = character(0)) {
  base <- data.frame(session = character(0), subject_id = character(0),
                     session_id = character(0), trial_id = character(0),
                     observed = logical(0), stringsAsFactors = FALSE)
  if (length(extra)) {
    for (e in rev(extra)) base <- cbind(stats::setNames(
      data.frame(character(0), stringsAsFactors = FALSE), e), base)
  }
  base
}

#' Every trial of one subject, across sessions
#'
#' Lists the trial tables of all sessions in a \code{PhysioLongitudinal},
#' carrying the session each row came from. Without that column the rows would be
#' ambiguous: the same \code{trial_id} legitimately recurs in different sessions.
#'
#' A hierarchy with no trials returns an empty table with the same columns, not
#' an error -- "this subject has no trials recorded" is an answer.
#'
#' @param x A \code{PhysioLongitudinal}.
#' @return A data frame of trials with a \code{session} column.
#' @seealso \code{\link{cohortTrials}}, \code{\link{trials}}
#' @export
subjectTrials <- function(x) {
  stopifnot(methods::is(x, "PhysioLongitudinal"))
  ss <- sessions(x)
  parts <- Filter(Negate(is.null),
                  lapply(names(ss), function(nm) .session_trials(ss[[nm]], nm)))
  if (!length(parts)) return(.empty_trial_listing())
  do.call(rbind, parts)
}

#' Every trial in a cohort
#'
#' Lists the trials of every subject, carrying both the subject and the session.
#' The same \code{trial_id} under two subjects has to stay distinguishable, which
#' is why the subject travels with the row rather than being inferred.
#'
#' @param x A \code{PhysioCohort}.
#' @return A data frame of trials with \code{subject} and \code{session} columns.
#' @seealso \code{\link{subjectTrials}}
#' @export
cohortTrials <- function(x) {
  stopifnot(methods::is(x, "PhysioCohort"))
  subs <- subjects(x)
  parts <- Filter(function(d) !is.null(d) && nrow(d),
                  lapply(names(subs), function(nm) {
                    d <- subjectTrials(subs[[nm]])
                    if (!nrow(d)) return(NULL)
                    cbind(subject = nm, d, row.names = NULL)
                  }))
  if (!length(parts)) return(.empty_trial_listing("subject"))
  do.call(rbind, parts)
}

# ---- holding several independent recordings ---------------------------------
#
# The evaluation left this open: recordingMap() could state which recording a
# trial belongs to, but the recordings themselves were not held anywhere, so the
# ids were labels pointing outside the object.
#
# A trial set holds them. It is a plain list with a class attribute rather than a
# new S4 class: nothing here needs S4 dispatch, inheritance or slot validation,
# and an S4 class would add a definition whose relocation has to be managed
# forever (this ecosystem has just spent a reorganization learning what that
# costs). Each recording keeps its own container and therefore its own clock;
# nothing is concatenated.

#' Hold several independently recorded trials together
#'
#' When each trial is its own recording, the trials belong to one session but the
#' signals live in separate containers. A trial set holds those containers
#' alongside the map that says which trial each one carries and how the
#' recordings are placed relative to one another.
#'
#' **No common time axis is fabricated.** Each recording keeps its own clock, and
#' the second does not start where the first left off. Whether elapsed time
#' between trials can be computed is decided by \code{time_correspondence} in the
#' map, exactly as for \code{\link{recordingMap}} on a single container.
#'
#' Every \code{recording_id} in the map must name a recording that is actually
#' held; a map referring to a recording kept elsewhere is the situation this
#' structure exists to end.
#'
#' @param recordings A named list of \code{MultiPhysioExperiment} objects, named
#'   by \code{recording_id}.
#' @param map A data frame with \code{subject_id}, \code{session_id},
#'   \code{trial_id}, \code{recording_id}, \code{time_correspondence} and, for
#'   \code{"measured"}, \code{reference} and \code{offset}.
#' @return An object of class \code{physio_trial_set}.
#' @seealso \code{\link{recordings}}, \code{\link{recordingMap}},
#'   \code{\link{elapsedBetweenTrials}}
#' @export
#' @examples
#' rec <- function() MultiPhysioExperiment(emg = PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(100), 50, 2)), samplingRate = 50))
#' ts <- trialSet(
#'   recordings = list(R1 = rec(), R2 = rec()),
#'   map = data.frame(subject_id = "P1", session_id = "S1",
#'                    trial_id = c("T1", "T2"), recording_id = c("R1", "R2"),
#'                    time_correspondence = "measured", reference = "trigger",
#'                    offset = c(0, 12.5)))
#' elapsedBetweenTrials(ts, "T1", "T2")
trialSet <- function(recordings = list(), map = NULL) {
  recs <- as.list(recordings)
  if (length(recs)) {
    nm <- names(recs)
    if (is.null(nm) || any(!nzchar(nm))) {
      stop("every recording must be named by its recording_id", call. = FALSE)
    }
    if (anyDuplicated(nm)) {
      stop(sprintf("recording ids must be unique; duplicated: %s",
                   paste(unique(nm[duplicated(nm)]), collapse = ", ")),
           call. = FALSE)
    }
    ok <- vapply(recs, methods::is, logical(1), "MultiPhysioExperiment")
    if (!all(ok)) {
      stop(sprintf("these are not MultiPhysioExperiment objects: %s",
                   paste(nm[!ok], collapse = ", ")), call. = FALSE)
    }
  }
  df <- .as_table(map, "recording map")
  if (!is.null(df) && nrow(df)) {
    keys <- c(.TRIAL_KEYS, "recording_id")
    .require_cols(df, c(keys, "time_correspondence"), "the recording map")
    .require_unique(df, keys, "the recording map")
    bad <- setdiff(unique(as.character(df$time_correspondence)),
                   .TIME_CORRESPONDENCE)
    if (length(bad)) {
      stop(sprintf("unknown time_correspondence: %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    missing_recs <- setdiff(unique(as.character(df$recording_id)), names(recs))
    if (length(missing_recs)) {
      stop(sprintf(paste0("the map names recording(s) that are not held: %s. A ",
                          "trial set holds its recordings; pass them in ",
                          "`recordings` or drop the rows."),
                   paste(missing_recs, collapse = ", ")), call. = FALSE)
    }
    if (!"reference" %in% names(df)) df$reference <- NA_character_
    if (!"offset" %in% names(df)) df$offset <- NA_real_
  }
  structure(list(recordings = recs, map = df), class = "physio_trial_set")
}

#' The recordings held by a trial set
#'
#' @param x A \code{physio_trial_set}.
#' @return The named list of \code{MultiPhysioExperiment} recordings.
#' @seealso \code{\link{trialSet}}
#' @export
recordings <- function(x) {
  if (!inherits(x, "physio_trial_set")) {
    stop("'x' must be a trial set", call. = FALSE)
  }
  x$recordings
}

#' @export
print.physio_trial_set <- function(x, ...) {
  cat("physio_trial_set\n")
  cat("recordings(", length(x$recordings), "): ",
      paste(names(x$recordings), collapse = ", "), "\n", sep = "")
  m <- x$map
  if (!is.null(m) && nrow(m)) {
    cat("trials(", length(unique(m$trial_id)), "): ",
        paste(unique(as.character(m$trial_id)), collapse = ", "), "\n", sep = "")
    cat("time correspondence: ",
        paste(unique(as.character(m$time_correspondence)), collapse = ", "),
        "\n", sep = "")
  }
  invisible(x)
}

# ---- aligned waveform aggregation ------------------------------------------

#' Align and average trial waveforms through the experimental hierarchy
#'
#' Evaluates one stream on an explicitly supplied trial-relative grid, then
#' averages trials, sessions and subjects with equal weight at each level.
#' Measured sample times take priority over reconstructed sampling times.
#'
#' @param x A \code{MultiPhysioExperiment}, a named list of these recordings,
#'   a \code{PhysioLongitudinal}, or a \code{PhysioCohort}. Each recording must
#'   carry trial and interval tables for one subject and session. For a trial
#'   set, pass \code{recordings(x)} with these tables populated; map rows alone
#'   do not define waveform intervals. Duplicate full trial keys are rejected.
#' @param stream One stream name, evaluated separately from other modalities.
#' @param grid Finite, strictly increasing evaluation points. For \code{start},
#'   seconds relative to the alignment start; for \code{phase}, fractions in
#'   \code{[0,1]}. No output grid or sampling rate is inferred.
#' @param align Either \code{start} or \code{phase}. Phase maps each trial's
#'   supplied start and end to zero and one; this changes its time scale.
#' @param level Target: \code{session}, \code{subject}, or \code{cohort}.
#' @param assay Name of the two-dimensional numeric signal assay.
#' @param channels Unique channel names. By default all channels are used and
#'   their sets must agree across included recordings. Column order may differ.
#' @param strata Trial-table columns, such as condition, kept separate at every
#'   aggregation level. Modalities and channels are never averaged together.
#' @param anchors Optional data frame with subject_id, session_id, trial_id,
#'   start and end, keyed uniquely by the first three columns. Times use each
#'   recording's local shared clock. Without this table, the union of all stream
#'   intervals defines each trial's anchors, preserving inter-stream offsets.
#'   Windowed inputs require explicit anchors: a cropped boundary is not an
#'   original trial onset. A supplied start may also be an event alignment time.
#' @param include Trial states to include: \code{complete} by default, or
#'   explicitly \code{partial}. Selection uses the coverage of all intervals
#'   named by the trial, as in \code{\link{aggregateTrials}}. Unknown or
#'   undescribed trials cannot be included.
#' @param gap_factor Gap criterion passed to \code{\link{streamCoverage}}.
#' @param na.rm If FALSE, any unavailable aligned value causes an error. TRUE
#'   omits it pointwise; an all-missing group is retained with mean NA and zero
#'   contributing trials. This can change contributing units along the waveform.
#' @return A list with \code{data} (mean waveform by output unit, channel and
#'   grid point), \code{counts} (immediate-child counts and \code{n_trials}, the
#'   number of finite contributing original trial values), \code{aggregate}
#'   (the full hierarchical result), \code{aligned} (selected trial values),
#'   \code{sample_map} (record, stream, channel, bracketing original row indices,
#'   interpolation weight and missingness reason for each aligned row),
#'   \code{trials} (all trial states and selections), \code{anchors},
#'   \code{settings}, source hashes, and a result \code{checksum}.
#' @details
#' Linear interpolation uses adjacent finite samples inside the selected
#' stream's trial interval. There is no extrapolation, interpolation across
#' reported gaps, or skipping over missing signal values. Exact samples at gap
#' boundaries remain usable. Included trials with unusable retained gap evidence
#' are refused. The output is an interpolated representation, not antialias
#' filtering; preprocess signals appropriately before choosing a coarser grid.
#'
#' Channel names establish correspondence, not compatible units or physiology.
#' The caller must provide comparable assays and units. Source hashes include
#' times, selected signal values, channel metadata and interval/trial tables.
#' Interpolation indices refer to rows in the input recording, including when
#' that recording was previously windowed. Inputs themselves are not modified.
#'
#' \code{\link{aggregatePhysioFeatures}} performs the hierarchy and preserves
#' intermediate results and source-row links. The waveform SD or a confidence
#' interval is not estimated: trials are not treated as independent participants.
#' @seealso \code{\link{aggregateTrials}}, \code{\link{streamTimeIndex}}
#' @export
#' @examples
#' pe <- PhysioExperiment(assays = list(raw = matrix(0:30, ncol = 1,
#'   dimnames = list(NULL, "signal"))), samplingRate = 10)
#' m <- MultiPhysioExperiment(sensor = pe)
#' trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
#'                          trial_id = c("T1", "T2"))
#' trialIntervals(m) <- data.frame(trials(m)[1:3], recording_id = "R1",
#'   stream = "sensor", start = c(0, 2), end = c(1, 3))
#' z <- aggregateTrialWaveforms(m, "sensor", grid = c(0, 0.5, 1))
#' z$data
#' z$counts
aggregateTrialWaveforms <- function(x, stream, grid,
                                    align = c("start", "phase"),
                                    level = c("session", "subject", "cohort"),
                                    assay = "raw", channels = NULL,
                                    strata = NULL, anchors = NULL,
                                    include = "complete", gap_factor = 1.5,
                                    na.rm = FALSE) {
  align <- match.arg(align); level <- match.arg(level)
  include <- match.arg(include, c("complete", "partial"), several.ok = TRUE)
  names_ok <- function(v) is.character(v) && !anyNA(v) &&
    all(nzchar(trimws(v))) && !anyDuplicated(v)
  if (!names_ok(stream) || length(stream) != 1L ||
      !names_ok(assay) || length(assay) != 1L)
    stop("stream and assay must be single nonempty names", call. = FALSE)
  if (!is.numeric(grid) || is.complex(grid) || !is.null(dim(grid)) ||
      !length(grid) || any(!is.finite(grid)) || any(diff(grid) <= 0))
    stop("grid must be finite and strictly increasing", call. = FALSE)
  if (align == "phase" && any(grid < 0 | grid > 1))
    stop("phase grid must lie in [0,1]", call. = FALSE)
  if (!is.numeric(gap_factor) || length(gap_factor) != 1L ||
      !is.finite(gap_factor) || gap_factor <= 1)
    stop("gap_factor must be a finite scalar greater than one", call. = FALSE)
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("na.rm must be one nonmissing logical value", call. = FALSE)
  if (!is.null(channels) && (!names_ok(channels) || !length(channels)))
    stop("channels must be unique nonempty names", call. = FALSE)
  if (is.null(strata)) strata <- character()
  reserved <- c(.TRIAL_KEYS, "cycle_id", "channel", "grid_index", "coordinate",
                "value", "record", "state", "included", "reason")
  if (!names_ok(strata) || any(strata %in% reserved))
    stop("strata must be unique nonreserved column names", call. = FALSE)
  recs <- .waveform_recordings(x)
  tables <- lapply(recs, function(m) {
    tr <- trials(m)
    if (is.null(tr) || !nrow(tr)) stop("Every recording needs a trial table", call. = FALSE)
    .require_cols(tr, c(.TRIAL_KEYS, strata), "trial table")
    .require_unique(tr, .TRIAL_KEYS, "trial table")
    if (nrow(unique(tr[c("subject_id", "session_id")])) != 1L)
      stop("Each recording must describe one subject and session", call. = FALSE)
    tr <- tr[c(.TRIAL_KEYS, strata)]
    for (nm in names(tr)) {
      if (!(is.character(tr[[nm]]) || is.factor(tr[[nm]]) || is.numeric(tr[[nm]])) ||
          anyNA(tr[[nm]]) || any(!nzchar(trimws(as.character(tr[[nm]])))))
        stop("Missing or invalid trial key/stratum: ", nm, call. = FALSE)
      if (is.numeric(tr[[nm]]) && any(!is.finite(tr[[nm]])))
        stop("Nonfinite trial key/stratum: ", nm, call. = FALSE)
      tr[[nm]] <- as.character(tr[[nm]])
    }
    tr
  })
  all_tr <- do.call(rbind, unname(tables)); rownames(all_tr) <- NULL
  if (anyDuplicated(all_tr[.TRIAL_KEYS]))
    stop("Duplicate full trial keys across recordings; resolve recording identity first", call. = FALSE)
  if (!is.null(anchors)) {
    anchors <- as.data.frame(anchors)
    .require_cols(anchors, c(.TRIAL_KEYS, "start", "end"), "anchors")
    .require_unique(anchors, .TRIAL_KEYS, "anchors")
    if (!is.numeric(anchors$start) || !is.numeric(anchors$end) ||
        any(!is.finite(anchors$start)) || any(!is.finite(anchors$end)) ||
        any(anchors$end < anchors$start))
      stop("anchors require finite start <= end", call. = FALSE)
  }
  states <- aligned <- maps <- used_anchors <- hashes <- list()
  wanted <- channels
  for (r in seq_along(recs)) {
    m <- recs[[r]]; tr <- tables[[r]]; it <- trialIntervals(m)
    if (nrow(it)) {
      valid <- vapply(seq_len(nrow(it)), function(i)
        any(.waveform_key_match(tr, it[i, , drop = FALSE])), logical(1))
      if (!all(valid)) stop("Interval keys not found in trial table", call. = FALSE)
    }
    state <- .trial_states(m, tr$trial_id, gap_factor = gap_factor)
    selected <- state %in% include
    reason <- ifelse(selected, "included", unname(.TRIAL_STATE_REASONS[state]))
    if (!stream %in% names(m@streams)) {
      selected[] <- FALSE; reason[] <- "requested stream absent"
    }
    interval_rows <- lapply(seq_len(nrow(tr)), function(j)
      it[.waveform_key_match(it, tr[j, , drop = FALSE]), , drop = FALSE])
    for (j in which(selected)) {
      nr <- sum(as.character(interval_rows[[j]]$stream) == stream)
      if (nr == 0L) {selected[j] <- FALSE; reason[j] <- "requested stream has no trial interval"}
      if (nr > 1L) stop("Ambiguous recording intervals for requested stream and trial", call. = FALSE)
    }
    states[[r]] <- cbind(tr, record = names(recs)[r], state = state,
                          included = selected, reason = reason)
    if (!any(selected)) next
    pe <- m@streams[[stream]]
    if (!assay %in% SummarizedExperiment::assayNames(pe))
      stop("Requested assay absent: ", assay, call. = FALSE)
    a <- as.matrix(SummarizedExperiment::assay(pe, assay))
    if (length(dim(SummarizedExperiment::assay(pe, assay))) != 2L ||
        !is.numeric(a) || is.complex(a) || any(is.infinite(a)))
      stop("Waveform assay must be a numeric matrix without infinity", call. = FALSE)
    cn <- colnames(a)
    if (!names_ok(cn) || !length(cn)) stop("Waveforms need unique channel names", call. = FALSE)
    if (is.null(wanted)) wanted <- cn
    if (!all(wanted %in% cn) || (is.null(channels) && !setequal(wanted, cn)))
      stop("Incompatible channel names; specify a common channels subset", call. = FALSE)
    a <- a[, wanted, drop = FALSE]
    tt <- streamTimeIndex(m, stream)
    if (length(tt) != nrow(a) || !length(tt) || any(!is.finite(tt)) || any(diff(tt) <= 0))
      stop("Sample times must be finite, strictly increasing and match assay rows", call. = FALSE)
    hashes[[length(hashes) + 1L]] <- data.frame(record = names(recs)[r],
      sha256 = digest::digest(list(times = tt, values = a, clock = m@clock,
        channel_metadata = SummarizedExperiment::colData(pe), trials = trials(m),
        intervals = it), algo = "sha256"))
    for (j in which(selected)) {
      rows <- interval_rows[[j]]
      row <- rows[as.character(rows$stream) == stream, , drop = FALSE]
      coverage <- lapply(seq_len(nrow(rows)), function(k)
        streamCoverage(m, as.character(rows$stream[k]), rows$start[k], rows$end[k],
                       gap_factor = gap_factor))
      cv <- coverage[[which(as.character(rows$stream) == stream)]]
      if (any(vapply(coverage, function(z) z$rule$unusable_carried_gaps > 0L, logical(1))))
        stop("Included trial has unusable retained gap evidence: ", tr$trial_id[j], call. = FALSE)
      if (is.null(anchors)) {
        if (any(c("original_start", "original_end") %in% names(rows)) ||
            any(vapply(intersect(c("truncated_start", "truncated_end"), names(rows)),
                       function(nm) any(rows[[nm]] %in% TRUE), logical(1))))
          stop("Windowed trials require explicit anchors to preserve the original onset", call. = FALSE)
        lo <- min(rows$start); hi <- max(rows$end)
      } else {
        ai <- which(.waveform_key_match(anchors, tr[j, , drop = FALSE]))
        if (length(ai) != 1L) stop("One explicit anchor row is required per included trial", call. = FALSE)
        lo <- anchors$start[ai]; hi <- anchors$end[ai]
      }
      if (align == "phase" && hi <= lo)
        stop("phase alignment needs a positive trial duration", call. = FALSE)
      target <- if (align == "start") lo + grid else lo + grid * (hi - lo)
      if (any(!is.finite(target)) || any(diff(target) <= 0))
        stop("grid cannot be represented on this recording's clock", call. = FALSE)
      used_anchors[[length(used_anchors) + 1L]] <- cbind(tr[j, .TRIAL_KEYS, drop = FALSE],
        start = lo, end = hi, record = names(recs)[r])
      tol <- .window_tol(row$start, row$end)
      idx <- which(tt >= row$start - tol & tt <= row$end + tol)
      for (ch in wanted) {
        ev <- .waveform_evaluate(tt, a[, ch], idx, target, cv$gaps)
        n <- length(grid)
        block <- cbind(tr[rep(j, n), , drop = FALSE], channel = ch,
                       grid_index = seq_len(n), value = ev$value)
        aligned[[length(aligned) + 1L]] <- block
        maps[[length(maps) + 1L]] <- cbind(block[c(.TRIAL_KEYS, "channel", "grid_index")],
          record = names(recs)[r], recording_id = as.character(row$recording_id),
          stream = stream, coordinate = grid, target_time = target,
          ev[c("left_sample", "right_sample", "weight_right", "reason")])
      }
    }
  }
  state_table <- do.call(rbind, states); rownames(state_table) <- NULL
  if (!length(aligned)) stop("No trial satisfies waveform selection", call. = FALSE)
  values <- do.call(rbind, aligned); rownames(values) <- NULL
  sample_map <- do.call(rbind, maps); rownames(sample_map) <- NULL
  if (!na.rm && anyNA(values$value))
    stop("Missing aligned waveform values (gap, nonfinite sample or outside support); use na.rm = TRUE for explicit pointwise omission", call. = FALSE)
  agg <- aggregatePhysioFeatures(values, level, "value",
    strata = c(strata, "channel", "grid_index"), na.rm = na.rm)
  data <- agg$data
  data$grid_index <- as.integer(data$grid_index)
  data$coordinate <- grid[data$grid_index]
  counts <- agg$counts
  counts$n_trials <- vapply(agg$source_rows, function(i)
    sum(is.finite(agg$source$value[i])), integer(1))
  result <- list(data = data, counts = counts, aggregate = agg,
    aligned = values, sample_map = sample_map, trials = state_table,
    anchors = do.call(rbind, used_anchors), source_hashes = do.call(rbind, hashes),
    settings = list(stream = stream, assay = assay, channels = wanted, grid = grid,
      align = align, level = level, strata = strata, include = include,
      gap_factor = gap_factor, na.rm = na.rm, interpolation = "linear, adjacent samples only",
      weighting = "equal immediate children at each hierarchy level"))
  result$checksum <- digest::digest(result, algo = "sha256")
  result
}

.waveform_key_match <- function(table, row) {
  keep <- rep(TRUE, nrow(table))
  for (k in .TRIAL_KEYS) keep <- keep & as.character(table[[k]]) == as.character(row[[k]])
  keep[is.na(keep)] <- FALSE
  keep
}

.waveform_recordings <- function(x) {
  if (methods::is(x, "MultiPhysioExperiment")) return(list(record1 = x))
  check_subject <- function(person, subject_id = NULL) {
    ss <- as.list(sessions(person))
    seen <- character()
    sd <- as.data.frame(subjectData(person))
    for (nm in intersect(c("id", "subject_id"), names(sd))) {
      if (nrow(sd) && (anyNA(sd[[nm]]) ||
          length(unique(as.character(sd[[nm]]))) != 1L))
        stop("Invalid subject identity in enclosing hierarchy", call. = FALSE)
      if (nrow(sd)) {
        declared <- as.character(sd[[nm]][1L])
        if (!is.null(subject_id) && !identical(subject_id, declared))
          stop("Subject metadata disagrees with enclosing hierarchy", call. = FALSE)
        subject_id <- declared
      }
    }
    for (sid in names(ss)) {
      if (!methods::is(ss[[sid]], "MultiPhysioExperiment"))
        stop("Hierarchy sessions need MultiPhysioExperiment trial tables", call. = FALSE)
      tr <- trials(ss[[sid]])
      if (is.null(tr) || !nrow(tr)) stop("Every session needs a trial table", call. = FALSE)
      if (anyNA(tr$session_id) || any(as.character(tr$session_id) != sid))
        stop("Trial session_id disagrees with enclosing hierarchy", call. = FALSE)
      ids <- unique(as.character(tr$subject_id))
      if (length(ids) != 1L || anyNA(ids) ||
          (!is.null(subject_id) && any(ids != subject_id)))
        stop("Trial subject_id disagrees with enclosing hierarchy", call. = FALSE)
      seen <- c(seen, ids)
    }
    if (length(unique(seen)) > 1L)
      stop("Sessions of one subject contain different trial subject_id values", call. = FALSE)
    ss
  }
  if (methods::is(x, "PhysioLongitudinal")) x <- check_subject(x)
  if (methods::is(x, "PhysioCohort")) {
    pp <- subjects(x)
    parts <- lapply(names(pp), function(id) unname(check_subject(pp[[id]], id)))
    # Numeric list labels avoid collisions in user-supplied names containing '/'.
    x <- unlist(parts, recursive = FALSE)
    names(x) <- paste0("record", seq_along(x))
  }
  if (!(is.list(x) || methods::is(x, "List")) || !length(x) ||
      inherits(x, "physio_trial_set"))
    stop("x must contain recordings with trial tables", call. = FALSE)
  x <- as.list(x)
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x)) ||
      !all(vapply(x, methods::is, logical(1), "MultiPhysioExperiment")))
    stop("Provide a uniquely named list of MultiPhysioExperiment recordings", call. = FALSE)
  x
}

.waveform_evaluate <- function(times, values, idx, target, gaps) {
  out <- data.frame(value = rep(NA_real_, length(target)),
    left_sample = NA_integer_, right_sample = NA_integer_, weight_right = NA_real_,
    reason = "outside support")
  if (!length(idx)) return(out)
  t <- times[idx]
  positions <- findInterval(target, t)
  spacing <- if (length(t) > 1L) min(diff(t)) / 4 else Inf
  for (i in seq_along(target)) {
    at <- target[i]; left <- positions[i]
    # Timestamp equality follows floating-point resolution, not the much wider
    # tolerance used for window membership. A large clock must not snap a
    # nearby interpolation target to an observed sample.
    eps <- min(2 * .Machine$double.eps * max(1, abs(at)), spacing)
    candidates <- unique(pmax(1L, pmin(length(t), c(left, left + 1L))))
    exact <- candidates[abs(t[candidates] - at) <= eps]
    if (length(exact)) {
      l <- r <- idx[exact[which.min(abs(t[exact] - at))]]; w <- 0
    } else {
      if (left < 1L || left >= length(t)) next
      l <- idx[left]; r <- idx[left + 1L]
      w <- (at - times[l]) / (times[r] - times[l])
    }
    out$left_sample[i] <- l; out$right_sample[i] <- r; out$weight_right[i] <- w
    if (l != r && nrow(gaps) && any(gaps$start < times[r] & gaps$end > times[l])) {
      out$reason[i] <- "gap"; next
    }
    if (!is.finite(values[l]) || !is.finite(values[r])) {
      out$reason[i] <- "nonfinite sample"; next
    }
    out$value[i] <- (1 - w) * values[l] + w * values[r]
    out$reason[i] <- "available"
  }
  out
}

# ---- feature extraction across recordings and hierarchy ---------------------

.trial_features_hash <- function(x) {
  digest::digest(unclass(x)[setdiff(names(x), "checksum")], algo = "sha256")
}

#' Extract scalar trial features across sessions and participants
#'
#' Applies named feature functions to each selected trial and channel. Accepts
#' the same recording and hierarchy inputs as \code{aggregateTrialWaveforms()}.
#' Trial selection, time-based sample selection, channel matching and assembly
#' of the feature table are performed together. Pass the returned object to
#' \code{aggregatePhysioFeatures()} to obtain session, subject or cohort summaries.
#'
#' @param x A \code{MultiPhysioExperiment}, uniquely named list of these,
#'   \code{PhysioLongitudinal}, or \code{PhysioCohort}. Each recording must describe
#'   one subject and session, with unique full trial keys across recordings.
#' @param stream Name of the stream to measure.
#' @param FUN Named, nonempty list of feature functions, for example
#'   \code{list(amplitude = mean)}. Each receives the numeric sample vector for
#'   one trial and channel, followed by \code{...}, and must return one numeric
#'   value (\code{NA} is allowed). Functions are responsible for their scientific
#'   definition and treatment of missing signal values.
#' @param assay Assay name, default \code{"raw"}.
#' @param channels Channel names. Defaults to all channels, requiring the same
#'   names across selected recordings; column order may differ.
#' @param strata Trial-table columns to preserve as separate groups in subsequent
#'   aggregation. Channels are always kept separate.
#' @param include Trial states to process: \code{"complete"} (default) or
#'   explicitly \code{"partial"}, or both. Classification uses every stream
#'   described for the trial, as in \code{aggregateTrials()}.
#' @param gap_factor Gap criterion passed to \code{streamCoverage()}.
#' @param ... Additional fixed arguments passed to every feature function.
#' @return A \code{physio_trial_features} list containing \code{data} (one row per
#'   selected trial and channel), \code{trials} (all trial selections and reasons),
#'   \code{source_map} (one entry per feature-table row: recording, interval,
#'   original sample indices and sample times), \code{source_hashes} (full input
#'   recording hashes), \code{features}, \code{strata}, \code{settings} and
#'   \code{checksum}. Input containers are not modified.
#' @details
#' Sample membership uses the measured time index and the same boundary tolerance
#' as \code{timeWindow()}. Samples are neither interpolated nor resampled. For
#' explicitly included partial trials, features describe the available samples
#' inside the retained interval; these can differ in duration between trials.
#' Included trials with unusable retained gap evidence are refused. Trials
#' lacking the requested stream or interval are recorded as excluded. A recording
#' without a trial table, ambiguous identity, an incompatible assay/channel set,
#' an empty selected interval, or a failed feature function causes an error.
#'
#' Temporal coverage does not establish signal quality. Missing values are passed
#' to the feature function; returned \code{NA} values remain in the table and
#' require explicit handling when aggregating. Infinite input samples and
#' infinite feature results are rejected. Feature-function text and supplied
#' arguments are recorded, but arbitrary external state used by a callback is
#' not captured; use self-contained functions and explicit arguments.
#'
#' Correspondence is established by channel names. Comparable units and feature
#' definitions remain the caller's responsibility. The returned checksum detects
#' modification of the extraction result before aggregation, and references are
#' preserved when continuing aggregation through further levels.
#' @seealso \code{\link{aggregatePhysioFeatures}}, \code{\link{aggregateTrials}},
#'   \code{\link{aggregateTrialWaveforms}}
#' @export
#' @examples
#' pe <- PhysioExperiment(assays = list(raw = matrix(0:30, ncol = 1,
#'   dimnames = list(NULL, "signal"))), samplingRate = 10)
#' m <- MultiPhysioExperiment(sensor = pe)
#' trials(m) <- data.frame(subject_id = "P1", session_id = "S1",
#'                         trial_id = c("T1", "T2"))
#' trialIntervals(m) <- data.frame(trials(m)[1:3], recording_id = "R1",
#'   stream = "sensor", start = c(0, 2), end = c(1, 3))
#' f <- extractTrialFeatures(m, "sensor", FUN = list(amplitude = mean))
#' f$data
#' aggregatePhysioFeatures(f, "session")$data # mean(c(5, 25)) = 15
extractTrialFeatures <- function(x, stream, FUN, assay = "raw", channels = NULL,
                                 strata = NULL, include = "complete",
                                 gap_factor = 1.5, ...) {
  names_ok <- function(v) is.character(v) && !anyNA(v) &&
    all(nzchar(trimws(v))) && !anyDuplicated(v)
  if (!names_ok(stream) || length(stream) != 1L ||
      !names_ok(assay) || length(assay) != 1L)
    stop("stream and assay must be single nonempty names", call. = FALSE)
  if (!is.list(FUN) || !length(FUN) || !names_ok(names(FUN)) ||
      !all(vapply(FUN, is.function, logical(1))))
    stop("FUN must be a named list of feature functions", call. = FALSE)
  if (is.null(strata)) strata <- character()
  reserved <- c(.TRIAL_KEYS, "cycle_id", "channel", "record", "state",
                "included", "reason")
  if (!names_ok(strata) || any(strata %in% reserved) ||
      any(names(FUN) %in% c(reserved, strata)))
    stop("Feature and stratum names must be unique and nonreserved", call. = FALSE)
  if (!is.null(channels) && (!names_ok(channels) || !length(channels)))
    stop("channels must be unique nonempty names", call. = FALSE)
  include <- match.arg(include, c("complete", "partial"), several.ok = TRUE)
  if (!is.numeric(gap_factor) || length(gap_factor) != 1L ||
      !is.finite(gap_factor) || gap_factor <= 1)
    stop("gap_factor must be a finite scalar greater than one", call. = FALSE)
  args <- list(...)
  recs <- .waveform_recordings(x)
  tables <- lapply(recs, function(m) {
    tr <- trials(m)
    if (is.null(tr) || !nrow(tr))
      stop("Every recording needs a trial table", call. = FALSE)
    .require_cols(tr, c(.TRIAL_KEYS, strata), "trial table")
    .require_unique(tr, .TRIAL_KEYS, "trial table")
    if (nrow(unique(tr[c("subject_id", "session_id")])) != 1L)
      stop("Each recording must describe one subject and session", call. = FALSE)
    tr <- tr[c(.TRIAL_KEYS, strata)]
    for (nm in names(tr)) {
      v <- tr[[nm]]
      if (!is.null(dim(v)) ||
          !(is.character(v) || is.factor(v) || (is.numeric(v) && !is.object(v))) ||
          anyNA(v) || (is.numeric(v) && any(!is.finite(v))) ||
          any(!nzchar(trimws(as.character(v)))))
        stop("Invalid trial key or stratum: ", nm, call. = FALSE)
      tr[[nm]] <- as.character(v)
    }
    tr
  })
  all_trials <- do.call(rbind, unname(tables))
  if (anyDuplicated(all_trials[.TRIAL_KEYS]))
    stop("Duplicate full trial keys across recordings; resolve recording identity first", call. = FALSE)
  states <- blocks <- maps <- hashes <- list()
  wanted <- channels
  for (r in seq_along(recs)) {
    m <- recs[[r]]; tr <- tables[[r]]; it <- trialIntervals(m)
    if (nrow(it) && !all(vapply(seq_len(nrow(it)), function(i)
        any(.waveform_key_match(tr, it[i, , drop = FALSE])), logical(1))))
      stop("Interval keys not found in trial table", call. = FALSE)
    state <- .trial_states(m, tr$trial_id, gap_factor = gap_factor)
    selected <- state %in% include
    reason <- ifelse(selected, "included", unname(.TRIAL_STATE_REASONS[state]))
    if (!stream %in% names(m@streams)) {
      selected[] <- FALSE; reason[] <- "requested stream absent"
    }
    interval_rows <- lapply(seq_len(nrow(tr)), function(j)
      it[.waveform_key_match(it, tr[j, , drop = FALSE]), , drop = FALSE])
    for (j in which(selected)) {
      nr <- sum(as.character(interval_rows[[j]]$stream) == stream)
      if (nr == 0L) {selected[j] <- FALSE; reason[j] <- "requested stream has no trial interval"}
      if (nr > 1L) stop("Ambiguous recording intervals for requested stream and trial", call. = FALSE)
    }
    states[[r]] <- cbind(tr, record = names(recs)[r], state = state,
                          included = selected, reason = reason)
    hashes[[r]] <- data.frame(record = names(recs)[r],
                              sha256 = digest::digest(m, algo = "sha256"))
    if (!any(selected)) next
    pe <- m@streams[[stream]]
    if (!assay %in% SummarizedExperiment::assayNames(pe))
      stop("Requested assay absent: ", assay, call. = FALSE)
    a <- SummarizedExperiment::assay(pe, assay)
    if (length(dim(a)) != 2L)
      stop("Feature assay must be a numeric matrix", call. = FALSE)
    a <- as.matrix(a)
    if (!is.numeric(a) || is.complex(a) || any(is.infinite(a)))
      stop("Feature assay must be a numeric matrix without infinity", call. = FALSE)
    cn <- colnames(a)
    if (!names_ok(cn) || !length(cn))
      stop("Features need unique channel names", call. = FALSE)
    if (is.null(wanted)) wanted <- cn
    if (!all(wanted %in% cn) || (is.null(channels) && !setequal(wanted, cn)))
      stop("Incompatible channel names; specify a common channels subset", call. = FALSE)
    tt <- streamTimeIndex(m, stream)
    if (length(tt) != nrow(a) || !length(tt) || any(!is.finite(tt)) || any(diff(tt) <= 0))
      stop("Sample times must be finite, strictly increasing and match assay rows", call. = FALSE)
    for (j in which(selected)) {
      rows <- interval_rows[[j]]
      coverage <- lapply(seq_len(nrow(rows)), function(k)
        streamCoverage(m, as.character(rows$stream[k]), rows$start[k], rows$end[k],
                       gap_factor = gap_factor))
      if (any(vapply(coverage, function(z) z$rule$unusable_carried_gaps > 0L, logical(1))))
        stop("Included trial has unusable retained gap evidence: ", tr$trial_id[j], call. = FALSE)
      interval <- rows[as.character(rows$stream) == stream, , drop = FALSE]
      tol <- .window_tol(interval$start, interval$end)
      idx <- which(tt >= interval$start - tol & tt <= interval$end + tol)
      if (!length(idx))
        stop("Selected trial has no samples in requested stream: ", tr$trial_id[j], call. = FALSE)
      for (ch in wanted) {
        block <- cbind(tr[j, , drop = FALSE], channel = ch)
        for (nm in names(FUN)) {
          v <- tryCatch(do.call(FUN[[nm]], c(list(unname(a[idx, ch])), args)),
            error = function(e) stop("Feature ", nm, " failed in ", names(recs)[r],
              "/", tr$trial_id[j], "/", ch, ": ", conditionMessage(e), call. = FALSE))
          if (!is.numeric(v) || is.complex(v) || is.object(v) || length(v) != 1L ||
              !is.null(dim(v)) || is.infinite(v))
            stop("Feature ", nm, " must return one numeric value (NA allowed): ",
                 names(recs)[r], "/", tr$trial_id[j], "/", ch, call. = FALSE)
          block[[nm]] <- as.numeric(v)
        }
        pos <- length(blocks) + 1L
        blocks[[pos]] <- block
        maps[[pos]] <- list(data_row = pos, record = names(recs)[r],
          recording_id = as.character(interval$recording_id), stream = stream,
          assay = assay, channel = ch, interval = interval,
          samples = idx, times = tt[idx], n_missing = sum(is.na(a[idx, ch])))
      }
    }
  }
  if (!length(blocks)) stop("No trial satisfies feature selection", call. = FALSE)
  data <- do.call(rbind, blocks); rownames(data) <- NULL
  state_table <- do.call(rbind, states); rownames(state_table) <- NULL
  result <- structure(list(data = data, trials = state_table, source_map = maps,
    source_hashes = do.call(rbind, hashes), features = names(FUN),
    strata = c(strata, "channel"),
    settings = list(stream = stream, assay = assay, channels = wanted,
      include = include, gap_factor = gap_factor, arguments = args,
      function_text = lapply(FUN, function(f) paste(deparse(f), collapse = "\n")))),
    class = "physio_trial_features")
  result$checksum <- .trial_features_hash(result)
  result
}
