# Cross-session harmonization of channels, reference and montage over the
# sessions of a PhysioLongitudinal, so that baseline/mid/discharge/followup
# recordings share one channel set, reference and electrode layout before any
# cross-session analysis. Builds on the per-object channel operations in
# channels.R (pickChannels / renameChannels / setReference / applyMontage).

.harmonize_ver <- function() {
  tryCatch(as.character(utils::packageVersion("PhysioExperiment")),
           error = function(e) NA_character_)
}

.assert_pe_sessions <- function(s, what) {
  if (!all(vapply(s, methods::is, logical(1), "PhysioExperiment"))) {
    stop(sprintf("%s requires all sessions to be PhysioExperiment objects",
                 what), call. = FALSE)
  }
}

# replace the sessions of a PhysioLongitudinal, keeping design/subject + order
.set_sessions <- function(long, new_s, names_) {
  new_s <- S4Vectors::SimpleList(new_s)
  names(new_s) <- names_
  sessions(long) <- new_s
  long
}

#' Harmonize channels across sessions
#'
#' Intersects and reorders the channels of every session of a
#' \code{PhysioLongitudinal} so they share one common channel set in identical
#' order. Channels absent from any session are dropped; an optional
#' \code{rename} map first unifies differing label conventions.
#'
#' @param long A \code{PhysioLongitudinal}.
#' @param target_labels Optional character vector giving the desired channel set
#'   and order. Restricted to channels common to all sessions. If \code{NULL},
#'   the common intersection (in the first session's order) is used.
#' @param rename Optional named character vector \code{c(old = new, ...)} applied
#'   to every session before intersecting (e.g. \code{c(T7 = "T3")}).
#' @return The \code{PhysioLongitudinal} with harmonized sessions; each session
#'   records the kept/dropped/renamed channels and a provenance activity.
#' @seealso \code{\link{harmonize}}, \code{\link{harmonizeReport}},
#'   \code{\link{pickChannels}}
#' @export
#' @examples
#' mk <- function(labs) PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(50 * length(labs)), 50, length(labs))),
#'   colData = S4Vectors::DataFrame(label = labs), samplingRate = 100)
#' pl <- PhysioLongitudinal(
#'   baseline = mk(c("Fz", "Cz", "Pz", "Oz")),
#'   discharge = mk(c("Cz", "Fz", "Pz")))
#' h <- harmonizeChannels(pl)
#' channelNames(session(h, "baseline"))
harmonizeChannels <- function(long, target_labels = NULL, rename = NULL) {
  stopifnot(methods::is(long, "PhysioLongitudinal"))
  s <- as.list(sessions(long))
  if (length(s) == 0) return(long)
  .assert_pe_sessions(s, "harmonizeChannels")
  nm_all <- names(s)
  orig_names <- lapply(s, channelNames)          # pre-rename names, per session

  if (!is.null(rename)) {
    old <- names(rename); new <- unname(rename)
    if (is.null(old) || any(!nzchar(old)) || anyNA(old) ||
        anyNA(new) || any(!nzchar(new))) {
      stop("`rename` must be a named character vector c(old = new, ...) with ",
           "non-empty, non-NA labels", call. = FALSE)
    }
    s <- lapply(s, function(e) renameChannels(e, old, new))
  }

  # Reject duplicate labels (present in the raw data or introduced by a
  # colliding rename): pickChannels resolves names by first match, so a
  # duplicate would silently drop a real channel and be misreported as kept.
  for (i in seq_along(s)) {
    cn <- channelNames(s[[i]])
    if (anyDuplicated(cn)) {
      stop(sprintf("session '%s' has duplicate channel labels: %s",
                   nm_all[i],
                   paste(unique(cn[duplicated(cn)]), collapse = ", ")),
           call. = FALSE)
    }
  }

  common <- Reduce(intersect, lapply(s, channelNames))
  if (length(common) == 0) {
    stop("no channels are common to all sessions", call. = FALSE)
  }
  keep <- if (is.null(target_labels)) common
          else target_labels[target_labels %in% common]
  if (length(keep) == 0) {
    stop("none of `target_labels` are common to all sessions", call. = FALSE)
  }

  new_s <- lapply(seq_along(s), function(i) {
    e <- s[[i]]
    dropped <- setdiff(channelNames(e), keep)
    # only the rename entries whose 'old' actually existed in this session
    applied <- if (is.null(rename)) character(0)
               else rename[names(rename) %in% orig_names[[i]]]
    e <- pickChannels(e, keep)
    md <- S4Vectors::metadata(e)
    prev <- md$harmonize                          # accumulate across re-runs
    md$harmonize <- list(
      kept = keep,
      dropped = union(if (is.null(prev)) character(0) else prev$dropped, dropped),
      renamed = c(if (is.null(prev)) NULL else prev$renamed, applied))
    S4Vectors::metadata(e) <- md
    appendProvenance(e, activity = "harmonizeChannels",
                     params = list(n_kept = length(keep),
                                   n_dropped = length(dropped)),
                     software_version = .harmonize_ver())
  })
  .set_sessions(long, new_s, nm_all)
}

#' Harmonize the reference across sessions
#'
#' Records a common reference on every session (building on
#' \code{\link{setReference}}), so that \code{\link{getReference}} is identical
#' across sessions.
#'
#' @param long A \code{PhysioLongitudinal}.
#' @param ref Character reference label (e.g. \code{"average"}, \code{"Cz"}).
#' @return The \code{PhysioLongitudinal} with a common reference and a
#'   provenance activity on each session.
#' @seealso \code{\link{harmonize}}, \code{\link{setReference}}
#' @export
harmonizeReference <- function(long, ref) {
  stopifnot(methods::is(long, "PhysioLongitudinal"),
            is.character(ref), length(ref) == 1L)
  s <- as.list(sessions(long))
  if (length(s) == 0) return(long)
  .assert_pe_sessions(s, "harmonizeReference")
  new_s <- lapply(s, function(e) {
    e <- setReference(e, ref)
    appendProvenance(e, activity = "harmonizeReference",
                     params = list(reference = ref),
                     software_version = .harmonize_ver())
  })
  .set_sessions(long, new_s, names(s))
}

#' Harmonize the montage across sessions
#'
#' Applies a consistent electrode montage to every session (building on
#' \code{\link{applyMontage}}).
#'
#' @param long A \code{PhysioLongitudinal}.
#' @param system Montage system: one of \code{"10-20"}, \code{"10-10"},
#'   \code{"10-5"}.
#' @return The \code{PhysioLongitudinal} with a common montage and a provenance
#'   activity on each session.
#' @seealso \code{\link{harmonize}}, \code{\link{applyMontage}}
#' @export
harmonizeMontage <- function(long, system = c("10-20", "10-10", "10-5")) {
  stopifnot(methods::is(long, "PhysioLongitudinal"))
  system <- match.arg(system)
  s <- as.list(sessions(long))
  if (length(s) == 0) return(long)
  .assert_pe_sessions(s, "harmonizeMontage")
  new_s <- lapply(s, function(e) {
    e <- applyMontage(e, system)
    appendProvenance(e, activity = "harmonizeMontage",
                     params = list(system = system),
                     software_version = .harmonize_ver())
  })
  .set_sessions(long, new_s, names(s))
}

#' Harmonize channels, reference and montage across sessions
#'
#' Convenience wrapper that runs \code{\link{harmonizeChannels}},
#' \code{\link{harmonizeReference}} and \code{\link{harmonizeMontage}} in turn.
#' Pass \code{ref = NULL} or \code{system = NULL} to skip a step.
#'
#' @param long A \code{PhysioLongitudinal}.
#' @param target_labels,rename As in \code{\link{harmonizeChannels}}.
#' @param ref Common reference label, or \code{NULL} to skip (default
#'   \code{"average"}).
#' @param system Montage system, or \code{NULL} to skip (default \code{"10-20"}).
#' @return A fully harmonized \code{PhysioLongitudinal}.
#' @seealso \code{\link{harmonizeReport}}
#' @export
harmonize <- function(long, target_labels = NULL, rename = NULL,
                      ref = "average", system = "10-20") {
  long <- harmonizeChannels(long, target_labels = target_labels, rename = rename)
  if (!is.null(ref)) long <- harmonizeReference(long, ref)
  if (!is.null(system)) long <- harmonizeMontage(long, system)
  long
}

#' Report the channels dropped / renamed per session by harmonization
#'
#' @param long A harmonized \code{PhysioLongitudinal}.
#' @return A named list (one entry per session) with \code{kept}, \code{dropped}
#'   and \code{renamed} channels.
#' @seealso \code{\link{harmonizeChannels}}
#' @export
harmonizeReport <- function(long) {
  stopifnot(methods::is(long, "PhysioLongitudinal"))
  s <- as.list(sessions(long))
  stats::setNames(lapply(s, function(e) {
    # MultiRatePhysioExperiment has no metadata slot; report an empty entry
    if (!methods::is(e, "PhysioExperiment")) {
      return(list(kept = character(0), dropped = character(0),
                  renamed = character(0)))
    }
    h <- S4Vectors::metadata(e)$harmonize
    if (is.null(h)) {
      list(kept = channelNames(e), dropped = character(0),
           renamed = character(0))
    } else h
  }), names(s))
}
