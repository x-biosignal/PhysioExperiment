#' Longitudinal subject/session container
#'
#' \code{PhysioLongitudinal} is a subject-level container that links several
#' recording sessions of the same participant across a rehabilitation timeline -
#' typically \code{baseline}, \code{mid}, \code{discharge} and \code{followup}
#' visits. Each session is a \code{\link{PhysioExperiment}} (or
#' \code{\link{MultiRatePhysioExperiment}}); a design-schema \code{DataFrame}
#' records the visit label, days from baseline and condition of each session, and
#' a one-row subject \code{DataFrame} carries participant-level metadata (id,
#' diagnosis, affected side). Sessions are always kept in chronological order
#' (ascending \code{days_from_baseline}). It mirrors the
#' \pkg{MultiAssayExperiment} pattern, BIDS \code{ses-} entities and the OMOP
#' visit-occurrence model.
#'
#' @slot sessions A \code{SimpleList} of named \code{PhysioExperiment} /
#'   \code{MultiRatePhysioExperiment} sessions.
#' @slot design A \code{DataFrame} with one row per session
#'   (\code{session_id}, \code{visit_label}, \code{days_from_baseline},
#'   \code{condition}).
#' @slot subject A one-row \code{DataFrame} of subject metadata (\code{id},
#'   \code{dx}, \code{side}).
#' @seealso \code{\link{PhysioLongitudinal}} for the constructor,
#'   \code{\link{addSession}}, \code{\link{session}}
#' @name PhysioLongitudinal-class
#' @references Ramos, M., et al. (2017). Software for the integration of
#'   multiomics experiments in Bioconductor. \emph{Cancer Research}, 77(21).
#' @exportClass PhysioLongitudinal
setClass(
  "PhysioLongitudinal",
  representation(sessions = "SimpleList", design = "DataFrame",
                subject = "DataFrame"),
  prototype = list(
    sessions = S4Vectors::SimpleList(),
    design = S4Vectors::DataFrame(
      session_id = character(0), visit_label = character(0),
      days_from_baseline = numeric(0), condition = character(0)),
    subject = S4Vectors::DataFrame()
  ),
  validity = function(object) {
    msgs <- character(0)
    s <- object@sessions
    if (length(s) > 0) {
      if (is.null(names(s)) || any(!nzchar(names(s)))) {
        msgs <- c(msgs, "all sessions must be named")
      } else if (anyDuplicated(names(s))) {
        msgs <- c(msgs, "session names must be unique")
      }
      ok <- vapply(s, function(e)
        methods::is(e, "PhysioExperiment") ||
          methods::is(e, "MultiPhysioExperiment"), logical(1))
      if (!all(ok)) {
        msgs <- c(msgs, "all sessions must be PhysioExperiment or MultiPhysioExperiment")
      }
    }
    d <- object@design
    req <- c("session_id", "visit_label", "days_from_baseline")
    if (!all(req %in% colnames(d))) {
      msgs <- c(msgs, sprintf("design must contain columns: %s",
                              paste(req, collapse = ", ")))
    } else if (nrow(d) != length(s)) {
      msgs <- c(msgs, "design must have one row per session")
    } else if (length(s) > 0 &&
               !identical(as.character(d$session_id), names(s))) {
      # sessions are stored in the same (chronological) order as the design
      msgs <- c(msgs, "design session_id order must match session order")
    }
    if (length(msgs)) msgs else TRUE
  }
)

# recommended controlled vocabulary for visit_label (not enforced)
.PHYSIO_VISITS <- c("baseline", "mid", "discharge", "followup")

.default_design <- function(session_ids, visit_labels = session_ids,
                            days = NULL, conditions = NULL) {
  n <- length(session_ids)
  if (is.null(days)) days <- as.numeric(seq_len(n) - 1L)
  if (is.null(conditions)) conditions <- rep(NA_character_, n)
  S4Vectors::DataFrame(
    session_id = as.character(session_ids),
    visit_label = as.character(visit_labels),
    days_from_baseline = as.numeric(days),
    condition = as.character(conditions)
  )
}

#' Construct a PhysioLongitudinal container
#'
#' @param sessions A named list of \code{PhysioExperiment} /
#'   \code{MultiRatePhysioExperiment} sessions (or pass them as named
#'   \code{...} arguments). Names become the \code{session_id}s.
#' @param ... Additional named sessions.
#' @param design Optional \code{DataFrame} with columns \code{session_id},
#'   \code{visit_label}, \code{days_from_baseline} and (optionally)
#'   \code{condition}. If \code{NULL}, a default is built from the session names
#'   (visit label = name, days = input order).
#' @param subject Optional one-row \code{DataFrame} of subject metadata
#'   (\code{id}, \code{dx}, \code{side}).
#' @return A \code{PhysioLongitudinal} with sessions in chronological order.
#' @seealso \code{\link{sessions}}, \code{\link{addSession}},
#'   \code{\link{design}}
#' @export
#' @examples
#' mk <- function(sr = 250) PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(100 * 2), 100, 2)), samplingRate = sr)
#' pl <- PhysioLongitudinal(
#'   baseline = mk(), discharge = mk(),
#'   design = S4Vectors::DataFrame(
#'     session_id = c("baseline", "discharge"),
#'     visit_label = c("baseline", "discharge"),
#'     days_from_baseline = c(0, 42)),
#'   subject = S4Vectors::DataFrame(id = "sub-01", dx = "stroke", side = "L"))
#' design(pl)
PhysioLongitudinal <- function(sessions = list(), ..., design = NULL,
                               subject = NULL) {
  all_sessions <- c(as.list(sessions), list(...))
  nm <- names(all_sessions)
  if (length(all_sessions) > 0) {
    if (is.null(nm) || any(!nzchar(nm))) {
      stop("all sessions must be named", call. = FALSE)
    }
    ok <- vapply(all_sessions, function(e)
      methods::is(e, "PhysioExperiment") ||
        methods::is(e, "MultiPhysioExperiment"), logical(1))
    if (!all(ok)) {
      stop("all sessions must be PhysioExperiment or MultiPhysioExperiment objects",
           call. = FALSE)
    }
  }

  if (length(all_sessions) > 0 && anyDuplicated(nm)) {
    stop("session names/ids must be unique", call. = FALSE)
  }

  if (is.null(design)) {
    design <- .default_design(nm)
  } else {
    design <- methods::as(design, "DataFrame")
    req <- c("session_id", "visit_label", "days_from_baseline")
    miss <- setdiff(req, colnames(design))
    if (length(miss)) {
      stop(sprintf("design is missing required column(s): %s",
                   paste(miss, collapse = ", ")), call. = FALSE)
    }
    if (!"condition" %in% colnames(design)) design$condition <- NA_character_
    if (anyDuplicated(design$session_id)) {
      stop("design$session_id values must be unique", call. = FALSE)
    }
    if (length(all_sessions) > 0 && !setequal(design$session_id, nm)) {
      stop("design$session_id must match the session names", call. = FALSE)
    }
  }

  if (nrow(design) > 0 && anyNA(design$days_from_baseline)) {
    stop("days_from_baseline must not contain NA", call. = FALSE)
  }

  # chronological order (ascending days_from_baseline)
  if (nrow(design) > 0) {
    design <- design[order(design$days_from_baseline), , drop = FALSE]
    rownames(design) <- NULL
    all_sessions <- all_sessions[design$session_id]
  }

  if (is.null(subject)) subject <- S4Vectors::DataFrame()
  else subject <- methods::as(subject, "DataFrame")

  methods::new("PhysioLongitudinal",
               sessions = do.call(S4Vectors::SimpleList, all_sessions),
               design = design, subject = subject)
}

# ---- accessors --------------------------------------------------------------

#' Access the sessions of a PhysioLongitudinal
#' @param x A \code{PhysioLongitudinal}.
#' @param value A named list / SimpleList of sessions (setter).
#' @return \code{sessions()} a \code{SimpleList}; setter returns the object.
#' @export
setGeneric("sessions", function(x) standardGeneric("sessions"))
#' @rdname sessions
#' @export
setMethod("sessions", "PhysioLongitudinal", function(x) x@sessions)
#' @rdname sessions
#' @export
setGeneric("sessions<-", function(x, value) standardGeneric("sessions<-"))
#' @rdname sessions
#' @export
setReplaceMethod("sessions", "PhysioLongitudinal", function(x, value) {
  x@sessions <- if (methods::is(value, "SimpleList")) value
                else do.call(S4Vectors::SimpleList, as.list(value))
  methods::validObject(x)
  x
})

#' Retrieve a single session by visit label or id
#' @param x A \code{PhysioLongitudinal}.
#' @param label A \code{visit_label} (e.g. \code{"discharge"}) or a
#'   \code{session_id}.
#' @return The matching session, or an error if none/ambiguous.
#' @seealso \code{\link{sessions}}
#' @export
session <- function(x, label) {
  stopifnot(methods::is(x, "PhysioLongitudinal"),
            is.character(label), length(label) == 1L)
  d <- x@design
  # which() drops NA matches so an NA visit_label in another row cannot poison
  # the lookup or shadow the session_id fallback
  id <- d$session_id[which(d$visit_label == label)]
  if (length(id) == 0L && label %in% names(x@sessions)) id <- label
  if (length(id) == 0L) stop(sprintf("no session with label '%s'", label),
                             call. = FALSE)
  x@sessions[[id[1]]]
}

#' The design schema of a PhysioLongitudinal
#'
#' A method for the \pkg{BiocGenerics} \code{design} generic, so it composes
#' cleanly with the rest of the Bioconductor ecosystem.
#'
#' @param object A \code{PhysioLongitudinal}.
#' @param value A design \code{DataFrame} (setter).
#' @param ... Ignored.
#' @return \code{design()} the design \code{DataFrame}.
#' @rdname design
#' @export
setMethod("design", "PhysioLongitudinal", function(object, ...) object@design)
#' @rdname design
#' @export
setReplaceMethod("design", "PhysioLongitudinal", function(object, ..., value) {
  value <- methods::as(value, "DataFrame")
  req <- c("session_id", "visit_label", "days_from_baseline")
  if (!all(req %in% colnames(value))) {
    stop(sprintf("design must contain columns: %s", paste(req, collapse = ", ")),
         call. = FALSE)
  }
  if (!"condition" %in% colnames(value)) value$condition <- NA_character_
  value <- value[order(value$days_from_baseline), , drop = FALSE]
  rownames(value) <- NULL
  object@design <- value
  object@sessions <- object@sessions[value$session_id]   # realign to new design
  methods::validObject(object)
  object
})

#' Subject-level metadata
#' @param x A \code{PhysioLongitudinal}.
#' @param value A one-row subject \code{DataFrame} (setter).
#' @return \code{subjectData()} the subject \code{DataFrame}.
#' @export
setGeneric("subjectData", function(x) standardGeneric("subjectData"))
#' @rdname subjectData
#' @export
setMethod("subjectData", "PhysioLongitudinal", function(x) x@subject)
#' @rdname subjectData
#' @export
setGeneric("subjectData<-", function(x, value) standardGeneric("subjectData<-"))
#' @rdname subjectData
#' @export
setReplaceMethod("subjectData", "PhysioLongitudinal", function(x, value) {
  x@subject <- methods::as(value, "DataFrame")
  x
})

#' Add a session to a PhysioLongitudinal
#'
#' Appends a session and re-sorts the container into chronological order by
#' \code{days_from_baseline}.
#'
#' @param x A \code{PhysioLongitudinal}.
#' @param label The visit label (e.g. \code{"followup"}).
#' @param pe A \code{PhysioExperiment} / \code{MultiRatePhysioExperiment}.
#' @param days_from_baseline Numeric days since the baseline visit.
#' @param condition Optional condition string.
#' @param session_id Session id (defaults to \code{label}).
#' @return The updated \code{PhysioLongitudinal}, re-sorted chronologically.
#' @seealso \code{\link{PhysioLongitudinal}}
#' @export
addSession <- function(x, label, pe, days_from_baseline,
                       condition = NA_character_, session_id = label) {
  stopifnot(methods::is(x, "PhysioLongitudinal"),
            methods::is(pe, "PhysioExperiment") ||
              methods::is(pe, "MultiPhysioExperiment"),
            is.character(label), length(label) == 1L,
            is.numeric(days_from_baseline), length(days_from_baseline) == 1L)
  if (session_id %in% names(x@sessions)) {
    stop(sprintf("session_id '%s' already exists", session_id), call. = FALSE)
  }
  sess <- c(as.list(x@sessions), stats::setNames(list(pe), session_id))
  old <- as.data.frame(x@design, stringsAsFactors = FALSE)
  newrow <- data.frame(session_id = session_id, visit_label = label,
                       days_from_baseline = days_from_baseline,
                       condition = condition, stringsAsFactors = FALSE)
  # align to the existing design's columns (fill any extra columns with NA)
  for (cn in setdiff(colnames(old), colnames(newrow))) newrow[[cn]] <- NA
  newrow <- newrow[, colnames(old), drop = FALSE]
  PhysioLongitudinal(sessions = sess,
                     design = S4Vectors::DataFrame(rbind(old, newrow)),
                     subject = x@subject)
}

# ---- methods ----------------------------------------------------------------

#' Display, size and subsetting for PhysioLongitudinal
#' @param x A \code{PhysioLongitudinal}.
#' @param i Session index/name/logical (for \code{[}).
#' @param j,drop Ignored (present for \code{[} generic compatibility).
#' @param object A \code{PhysioLongitudinal} (for \code{show}).
#' @param ... Ignored.
#' @return \code{length()} the number of sessions; \code{[} a subset
#'   \code{PhysioLongitudinal}; \code{show()} is called for its side effect.
#' @name PhysioLongitudinal-methods
#' @rdname PhysioLongitudinal-methods
NULL

#' @rdname PhysioLongitudinal-methods
#' @export
setMethod("length", "PhysioLongitudinal", function(x) length(x@sessions))

#' @rdname PhysioLongitudinal-methods
#' @export
setMethod("[", "PhysioLongitudinal", function(x, i, ...) {
  sess <- x@sessions[i]
  ids <- names(sess)
  d <- x@design[x@design$session_id %in% ids, , drop = FALSE]
  d <- d[order(d$days_from_baseline), , drop = FALSE]
  rownames(d) <- NULL
  sess <- sess[d$session_id]          # keep sessions aligned to the sorted design
  methods::new("PhysioLongitudinal", sessions = sess, design = d,
               subject = x@subject)
})

#' @rdname PhysioLongitudinal-methods
#' @export
setMethod("show", "PhysioLongitudinal", function(object) {
  cat("class: PhysioLongitudinal\n")
  s <- object@subject
  if (nrow(s) > 0) {
    cat("subject: ",
        paste(vapply(colnames(s), function(cn)
          paste0(cn, "=", as.character(s[[cn]][1])), character(1)),
          collapse = " "), "\n", sep = "")
  }
  d <- object@design
  cat("sessions(", length(object@sessions), "): ",
      paste(d$visit_label, collapse = ", "), "\n", sep = "")
  for (k in seq_len(nrow(d))) {
    cat("  ", d$visit_label[k], " (", d$session_id[k], "): day ",
        d$days_from_baseline[k], "\n", sep = "")
  }
})

# ---- aggregated provenance --------------------------------------------------

#' @rdname provenance
#' @export
setMethod("provenance", "PhysioLongitudinal", function(x) {
  s <- x@sessions
  empty <- cbind(session = character(0),
                 provenance(PhysioExperiment(S4Vectors::SimpleList())))
  if (length(s) == 0) return(empty)
  parts <- lapply(names(s), function(nm) {
    p <- provenance(s[[nm]])
    if (nrow(p) == 0) return(NULL)
    cbind(session = rep(nm, nrow(p)), p, stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) return(empty)
  do.call(rbind, parts)
})

# ---- MultiAssayExperiment interop -------------------------------------------

# Build a MultiAssayExperiment from a PhysioLongitudinal (design/subject stored
# in metadata; MultiRate sessions are flattened via alignStreams()).
.plToMAE <- function(from) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Package 'MultiAssayExperiment' is required for this coercion.",
         call. = FALSE)
  }
  exps <- lapply(as.list(from@sessions), function(s) {
    if (methods::is(s, "MultiPhysioExperiment")) s <- alignStreams(s)
    if (is.null(colnames(s))) {
      colnames(s) <- paste0("ch", seq_len(ncol(s)))
    }
    s
  })
  el <- MultiAssayExperiment::ExperimentList(exps)
  mae <- MultiAssayExperiment::MultiAssayExperiment(experiments = el)
  md <- S4Vectors::metadata(mae)
  md$physio_design <- from@design
  md$physio_subject <- from@subject
  S4Vectors::metadata(mae) <- md
  mae
}

# Rebuild a PhysioLongitudinal from a MultiAssayExperiment.
.maeToPL <- function(from) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Package 'MultiAssayExperiment' is required for this coercion.",
         call. = FALSE)
  }
  exps <- as.list(MultiAssayExperiment::experiments(from))
  sess <- lapply(exps, function(e) {
    if (methods::is(e, "PhysioExperiment")) return(e)
    methods::new("PhysioExperiment",
                 methods::as(e, "SummarizedExperiment"),
                 samplingRate = as.numeric(NA))
  })
  names(sess) <- names(exps)
  md <- S4Vectors::metadata(from)
  design <- md$physio_design
  subject <- md$physio_subject %||% S4Vectors::DataFrame()
  if (is.null(design)) design <- .default_design(names(sess))
  PhysioLongitudinal(sessions = sess, design = design, subject = subject)
}

#' Coerce a PhysioLongitudinal to a MultiAssayExperiment
#'
#' Requires the \pkg{MultiAssayExperiment} package. Each session becomes an
#' experiment (named by \code{session_id}); the design schema and subject
#' metadata are stored in the result's \code{metadata()}.
#'
#' @param x A \code{PhysioLongitudinal}.
#' @return A \code{MultiAssayExperiment}.
#' @seealso \code{\link{PhysioLongitudinal}}
#' @export
asMultiAssayExperiment <- function(x) {
  stopifnot(methods::is(x, "PhysioLongitudinal"))
  .plToMAE(x)
}
