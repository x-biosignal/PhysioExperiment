# Per-patient change scores over a PhysioLongitudinal: apply any user metric to
# each session and report the per-visit change from baseline, optionally flagged
# against a minimal-detectable-change threshold. asMSKTracker() bridges the
# per-visit metric matrix into the shape PhysioMSKNet's mskMinimalDetectableChange()
# / mskRecoveryTrajectoryFit() consume (their `activation_series` slot), so MDC /
# MCID and recovery-curve fits can run on the container output directly.

# metrics x visits matrix of a user metric applied to each session
.metric_matrix <- function(long, metric_fn, metric_name = "metric") {
  s <- as.list(sessions(long))
  d <- design(long)
  if (length(s) == 0) stop("the container has no sessions", call. = FALSE)
  nms <- names(s)
  vals <- lapply(seq_along(s), function(i) {
    v <- tryCatch(metric_fn(s[[i]]), error = function(err)
      stop(sprintf("metric_fn failed on session '%s': %s", nms[i],
                   conditionMessage(err)), call. = FALSE))
    if (!is.numeric(v) || length(v) == 0L) {
      stop("metric_fn must return a non-empty numeric value", call. = FALSE)
    }
    if (is.null(names(v))) {
      names(v) <- if (length(v) == 1L) metric_name
                  else paste0(metric_name, seq_along(v))
    }
    v
  })
  metric_names <- names(vals[[1]])
  if (anyDuplicated(metric_names)) {
    stop("metric_fn must return uniquely named metrics; got: ",
         paste(metric_names, collapse = ", "), call. = FALSE)
  }
  for (v in vals) {
    # same name set (order may differ; reindexed by name below)
    if (length(v) != length(metric_names) || !setequal(names(v), metric_names)) {
      stop("metric_fn must return the same metric name(s) for every session",
           call. = FALSE)
    }
  }
  raw <- vapply(vals, function(v) unname(v[metric_names]),   # names are unique
                numeric(length(metric_names)))
  cn <- as.character(d$visit_label)                          # unique, non-NA keys
  cn[is.na(cn)] <- paste0("visit", which(is.na(cn)))
  matrix(raw, nrow = length(metric_names), ncol = length(vals),
         dimnames = list(metric_names, make.unique(cn)))
}

.resolve_baseline <- function(long, baseline) {
  d <- design(long)
  if (is.null(baseline)) {
    if ("baseline" %in% d$visit_label) return(which(d$visit_label == "baseline")[1])
    return(1L)                      # design is chronologically sorted
  }
  idx <- which(d$visit_label == baseline)
  if (length(idx) == 0L) idx <- which(d$session_id == baseline)
  if (length(idx) == 0L) {
    stop(sprintf("baseline '%s' matches no visit_label or session_id", baseline),
         call. = FALSE)
  }
  idx[1]
}

.subject_id <- function(long) {
  s <- subjectData(long)
  if (nrow(s) > 0 && "id" %in% colnames(s)) as.character(s$id[1]) else NA_character_
}

.normalize_mdc <- function(mdc, metrics) {
  out <- stats::setNames(rep(NA_real_, length(metrics)), metrics)
  if (is.null(mdc)) return(out)
  if (!is.numeric(mdc)) stop("`mdc` must be numeric", call. = FALSE)
  if (!is.null(names(mdc))) {
    extra <- setdiff(names(mdc), metrics)
    if (length(extra)) {
      warning("`mdc` name(s) not among metrics, ignored: ",
              paste(extra, collapse = ", "), call. = FALSE)
    }
    common <- intersect(names(mdc), metrics)
    out[common] <- mdc[common]
  } else if (length(mdc) == 1L) {
    out[] <- mdc                                   # scalar applies to all metrics
  } else {
    stop("name `mdc` by metric when passing more than one threshold",
         call. = FALSE)
  }
  out
}

#' Per-visit change scores across a longitudinal container
#'
#' Applies a user metric to every session of a \code{\link{PhysioLongitudinal}}
#' and reports each visit's change from the baseline visit, optionally flagged
#' against a minimal-detectable-change (MDC) threshold.
#'
#' @param long A \code{PhysioLongitudinal}.
#' @param metric_fn A function taking one session (a \code{PhysioExperiment})
#'   and returning a numeric scalar, or a named numeric vector of several
#'   metrics. It must return the same metric name(s) for every session.
#' @param baseline The baseline visit label or session id. If \code{NULL}
#'   (default) the visit labelled \code{"baseline"} is used, else the earliest
#'   visit.
#' @param method Delta reported: \code{"absolute"} (value - baseline),
#'   \code{"percent"} (percent of baseline) or \code{"z"} (change divided by the
#'   metric's across-visit SD).
#' @param mdc Optional MDC threshold: a numeric scalar (applied to all metrics),
#'   a numeric vector named by metric, or a vector matching the metric count.
#'   The \code{exceeds_mdc} flag compares the \emph{absolute} change to it. If
#'   \code{NULL}, \code{exceeds_mdc} is \code{NA}. See \code{\link{asMSKTracker}}
#'   for obtaining an MDC from \pkg{PhysioMSKNet}.
#' @param metric_name Name used for the metric when \code{metric_fn} returns an
#'   unnamed scalar (default \code{"metric"}).
#' @return A tidy \code{DataFrame} with columns \code{subject}, \code{visit},
#'   \code{days_from_baseline}, \code{metric}, \code{value},
#'   \code{delta_from_baseline}, \code{exceeds_mdc}.
#' @references Beckerman, H., et al. (2001). Smallest real difference, a link
#'   between reproducibility and responsiveness. \emph{Quality of Life
#'   Research}, 10(7), 571-578.
#' @seealso \code{\link{asMSKTracker}}, \code{\link{PhysioLongitudinal}}
#' @export
#' @examples
#' mk <- function(m) PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(m, 10, 2)), samplingRate = 100)
#' pl <- PhysioLongitudinal(
#'   baseline = mk(1), discharge = mk(3),
#'   design = S4Vectors::DataFrame(
#'     session_id = c("baseline", "discharge"),
#'     visit_label = c("baseline", "discharge"),
#'     days_from_baseline = c(0, 42)))
#' changeScores(pl, function(e) mean(SummarizedExperiment::assay(e, "raw")))
changeScores <- function(long, metric_fn, baseline = NULL,
                         method = c("absolute", "percent", "z"),
                         mdc = NULL, metric_name = "metric") {
  stopifnot(methods::is(long, "PhysioLongitudinal"), is.function(metric_fn))
  method <- match.arg(method)
  d <- design(long)
  if (nrow(d) == 0L) stop("the container has no sessions", call. = FALSE)

  mat <- .metric_matrix(long, metric_fn, metric_name)
  metrics <- rownames(mat); visits <- d$visit_label
  bcol <- .resolve_baseline(long, baseline)
  base_vals <- mat[, bcol]
  sds <- apply(mat, 1, stats::sd, na.rm = TRUE)
  mdc_vec <- .normalize_mdc(mdc, metrics)
  subj <- .subject_id(long)

  rows <- list()
  for (vi in seq_along(visits)) {
    for (mi in seq_along(metrics)) {
      val <- mat[mi, vi]; b <- base_vals[mi]
      abs_delta <- val - b
      delta <- switch(method,
        absolute = abs_delta,
        percent  = if (isTRUE(b != 0)) abs_delta / b * 100 else NA_real_,
        z        = if (isTRUE(sds[mi] > 0)) abs_delta / sds[mi] else NA_real_)
      thr <- mdc_vec[[metrics[mi]]]
      ex <- if (is.na(thr)) NA else abs(abs_delta) > thr
      rows[[length(rows) + 1L]] <- data.frame(
        subject = subj, visit = visits[vi],
        days_from_baseline = d$days_from_baseline[vi],
        metric = metrics[mi], value = val,
        delta_from_baseline = delta, exceeds_mdc = ex,
        stringsAsFactors = FALSE)
    }
  }
  S4Vectors::DataFrame(do.call(rbind, rows))
}

#' Bridge a longitudinal container to a PhysioMSKNet tracker
#'
#' Builds the per-visit metric matrix and wraps it as an
#' \code{MSKLongitudinalTracker} (its \code{activation_series}), so
#' \pkg{PhysioMSKNet}'s \code{mskMinimalDetectableChange()} and
#' \code{mskRecoveryTrajectoryFit()} can consume the container output directly
#' (metrics play the role of "muscles", visits the role of timepoints).
#'
#' @param long A \code{PhysioLongitudinal}.
#' @param metric_fn,metric_name As in \code{\link{changeScores}}.
#' @return An object of class \code{"MSKLongitudinalTracker"} carrying the
#'   metric-by-visit matrix.
#' @seealso \code{\link{changeScores}}
#' @export
#' @examples
#' mk <- function(m) PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(m, 10, 2)), samplingRate = 100)
#' pl <- PhysioLongitudinal(baseline = mk(1), mid = mk(2), discharge = mk(3))
#' tr <- asMSKTracker(pl, function(e) mean(SummarizedExperiment::assay(e, "raw")))
#' dim(tr$activation_series)
asMSKTracker <- function(long, metric_fn, metric_name = "metric") {
  stopifnot(methods::is(long, "PhysioLongitudinal"))
  mat <- .metric_matrix(long, metric_fn, metric_name)
  tp <- colnames(mat)
  tidy <- data.frame(
    timepoint = rep(tp, each = nrow(mat)),
    muscle = rep(rownames(mat), times = ncol(mat)),
    metric_name = "value",
    value = as.vector(mat),
    stringsAsFactors = FALSE)
  structure(
    list(metrics_table = tidy, timepoint_labels = tp,
         n_timepoints = ncol(mat), synergy_series = NULL,
         activation_series = mat),
    class = "MSKLongitudinalTracker")
}
