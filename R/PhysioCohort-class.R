#' Multi-subject cohort / study container
#'
#' \code{PhysioCohort} is the multi-subject layer above
#' \code{\link{PhysioLongitudinal}}: it holds many participants, each represented
#' by their own longitudinal timeline of sessions, alongside a subject-level
#' \code{colData} table (id, group/arm, diagnosis, age, sex, side, ...). This is
#' the container a rehabilitation study needs - a cohort or the arms of a trial,
#' as subjects x sessions - and the shape downstream pipelines, trial analyses
#' and prognostic models iterate over (see \code{\link{cohortDesign}}).
#'
#' A bare \code{\link{PhysioExperiment}} / \code{MultiRatePhysioExperiment}
#' passed as a subject is auto-wrapped into a single-session
#' \code{PhysioLongitudinal} (visit label \code{"session"}), so cross-sectional
#' and longitudinal cohorts share one API.
#'
#' @slot subjects A \code{SimpleList} of named \code{PhysioLongitudinal} subjects.
#' @slot colData A \code{DataFrame} with one row per subject; a \code{subject_id}
#'   column aligns to (and is ordered as) \code{names(subjects)}.
#' @slot metadata A \code{list} of cohort-level metadata.
#' @seealso \code{\link{PhysioCohort}} (constructor), \code{\link{cohortDesign}},
#'   \code{\link{subjects}}, \code{\link{PhysioLongitudinal}}
#' @name PhysioCohort-class
#' @exportClass PhysioCohort
setClass(
  "PhysioCohort",
  representation(subjects = "SimpleList", colData = "DataFrame",
                metadata = "list"),
  prototype = list(
    subjects = S4Vectors::SimpleList(),
    colData = S4Vectors::DataFrame(subject_id = character(0)),
    metadata = list()
  ),
  validity = function(object) {
    msgs <- character(0)
    s <- object@subjects
    if (length(s) > 0) {
      if (is.null(names(s)) || any(!nzchar(names(s)))) {
        msgs <- c(msgs, "all subjects must be named")
      } else if (anyDuplicated(names(s))) {
        msgs <- c(msgs, "subject names must be unique")
      }
      if (!all(vapply(s, methods::is, logical(1), "PhysioLongitudinal"))) {
        msgs <- c(msgs, "all subjects must be PhysioLongitudinal objects")
      }
    }
    cd <- object@colData
    if (!"subject_id" %in% colnames(cd)) {
      msgs <- c(msgs, "colData must contain a subject_id column")
    } else if (nrow(cd) != length(s)) {
      msgs <- c(msgs, "colData must have one row per subject")
    } else if (length(s) > 0 &&
               !identical(as.character(cd$subject_id), names(s))) {
      msgs <- c(msgs, "colData$subject_id order must match subject order")
    }
    if (length(msgs)) msgs else TRUE
  }
)

# wrap a bare experiment as a single-session PhysioLongitudinal
.as_pl_subject <- function(x, id) {
  if (methods::is(x, "PhysioLongitudinal")) return(x)
  if (methods::is(x, "PhysioExperiment") ||
      methods::is(x, "MultiPhysioExperiment")) {
    return(PhysioLongitudinal(sessions = list(session = x),
             subject = S4Vectors::DataFrame(id = id)))
  }
  stop(sprintf(paste0("subject '%s' must be a PhysioLongitudinal, ",
                      "PhysioExperiment or MultiPhysioExperiment"), id),
       call. = FALSE)
}

#' Construct a PhysioCohort
#'
#' @param subjects A named list of subjects - each a
#'   \code{\link{PhysioLongitudinal}} or a bare \code{PhysioExperiment} /
#'   \code{MultiRatePhysioExperiment} (auto-wrapped). Names become the
#'   \code{subject_id}s. Subjects may also be passed as named \code{...}.
#' @param ... Additional named subjects.
#' @param colData Optional \code{DataFrame} of subject-level metadata; must have
#'   one row per subject. If it has a \code{subject_id} column it is reordered to
#'   match the subjects, otherwise \code{subject_id} is taken from the names. If
#'   \code{NULL}, a minimal table is built from the names and each subject's own
#'   \code{subjectData()}.
#' @param group Optional vector (length = number of subjects) giving each
#'   subject's group / trial arm; stored as \code{colData$group}.
#' @param metadata Optional cohort-level metadata \code{list}.
#' @return A \code{PhysioCohort}.
#' @seealso \code{\link{cohortDesign}}, \code{\link{subjects}},
#'   \code{\link{addSubject}}
#' @export
#' @examples
#' mk <- function() PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(200), 100, 2)), samplingRate = 250)
#' coh <- PhysioCohort(
#'   "sub-01" = PhysioLongitudinal(baseline = mk(), discharge = mk(),
#'      design = S4Vectors::DataFrame(session_id = c("baseline", "discharge"),
#'        visit_label = c("baseline", "discharge"), days_from_baseline = c(0, 42))),
#'   "sub-02" = mk(),
#'   group = c("treatment", "control"))
#' coh
#' cohortDesign(coh)
PhysioCohort <- function(subjects = list(), ..., colData = NULL,
                         group = NULL, metadata = list()) {
  all_subj <- c(as.list(subjects), list(...))
  nm <- names(all_subj)
  if (length(all_subj) > 0) {
    if (is.null(nm) || any(!nzchar(nm)))
      stop("all subjects must be named", call. = FALSE)
    if (anyDuplicated(nm))
      stop("subject names/ids must be unique", call. = FALSE)
  }
  wrapped <- mapply(.as_pl_subject, all_subj, nm, SIMPLIFY = FALSE)

  if (is.null(colData)) {
    rows <- lapply(seq_along(wrapped), function(i) {
      sd <- subjectData(wrapped[[i]])
      base <- S4Vectors::DataFrame(subject_id = nm[i])
      # carry subject metadata columns (dx, side, ...) except a duplicate id
      extra <- sd[, setdiff(colnames(sd), c("id", "subject_id")), drop = FALSE]
      if (ncol(extra) > 0 && nrow(extra) == 1L) cbind(base, extra) else base
    })
    colData <- if (length(rows)) .rbind_fill_df(rows) else
      S4Vectors::DataFrame(subject_id = character(0))
  } else {
    colData <- methods::as(colData, "DataFrame")
    if (nrow(colData) != length(wrapped))
      stop("colData must have one row per subject", call. = FALSE)
    if ("subject_id" %in% colnames(colData)) {
      if (!setequal(as.character(colData$subject_id), nm))
        stop("colData$subject_id must match the subject names", call. = FALSE)
      colData <- colData[match(nm, as.character(colData$subject_id)), , drop = FALSE]
    } else {
      colData$subject_id <- nm
    }
    rownames(colData) <- NULL
  }

  if (!is.null(group)) {
    if (length(group) != length(wrapped))
      stop("group must have one value per subject", call. = FALSE)
    colData$group <- as.character(group)
  }
  # subject_id first
  colData <- colData[, c("subject_id",
                         setdiff(colnames(colData), "subject_id")), drop = FALSE]

  methods::new("PhysioCohort",
               subjects = do.call(S4Vectors::SimpleList, wrapped),
               colData = colData, metadata = metadata)
}

# rbind DataFrames with differing columns (fill missing with NA)
.rbind_fill_df <- function(dfs) {
  cols <- unique(unlist(lapply(dfs, colnames)))
  filled <- lapply(dfs, function(d) {
    for (cn in setdiff(cols, colnames(d))) d[[cn]] <- NA
    d[, cols, drop = FALSE]
  })
  do.call(rbind, filled)
}

# ---- accessors --------------------------------------------------------------

#' Subjects of a PhysioCohort
#' @param x A \code{PhysioCohort}.
#' @param value A named list / SimpleList of \code{PhysioLongitudinal} (setter).
#' @return \code{subjects()} a \code{SimpleList} of \code{PhysioLongitudinal}.
#' @export
setGeneric("subjects", function(x) standardGeneric("subjects"))
#' @rdname subjects
#' @export
setMethod("subjects", "PhysioCohort", function(x) x@subjects)
#' @rdname subjects
#' @export
setGeneric("subjects<-", function(x, value) standardGeneric("subjects<-"))
#' @rdname subjects
#' @export
setReplaceMethod("subjects", "PhysioCohort", function(x, value) {
  x@subjects <- if (methods::is(value, "SimpleList")) value
                else do.call(S4Vectors::SimpleList, as.list(value))
  methods::validObject(x)
  x
})

#' Retrieve a single subject's longitudinal record by id
#' @param x A \code{PhysioCohort}.
#' @param id A subject id (name).
#' @return The subject's \code{PhysioLongitudinal}.
#' @seealso \code{\link{subjects}}
#' @export
subject <- function(x, id) {
  stopifnot(methods::is(x, "PhysioCohort"), is.character(id), length(id) == 1L)
  if (!id %in% names(x@subjects))
    stop(sprintf("no subject '%s'", id), call. = FALSE)
  x@subjects[[id]]
}

#' Subject-level metadata table of a PhysioCohort
#' @param x A \code{PhysioCohort}.
#' @param value A subject-level \code{DataFrame} (setter); one row per subject.
#' @return \code{cohortData()} the subject-level \code{DataFrame}.
#' @export
setGeneric("cohortData", function(x) standardGeneric("cohortData"))
#' @rdname cohortData
#' @export
setMethod("cohortData", "PhysioCohort", function(x) x@colData)
#' @rdname cohortData
#' @export
setGeneric("cohortData<-", function(x, value) standardGeneric("cohortData<-"))
#' @rdname cohortData
#' @export
setReplaceMethod("cohortData", "PhysioCohort", function(x, value) {
  value <- methods::as(value, "DataFrame")
  if (nrow(value) != length(x@subjects))
    stop("colData must have one row per subject", call. = FALSE)
  if (!"subject_id" %in% colnames(value)) value$subject_id <- names(x@subjects)
  value <- value[match(names(x@subjects), as.character(value$subject_id)), ,
                 drop = FALSE]
  rownames(value) <- NULL
  x@colData <- value
  methods::validObject(x)
  x
})

#' Subject ids, subject count and group labels of a PhysioCohort
#' @param x A \code{PhysioCohort}.
#' @return \code{subjectIds()} a character vector; \code{nSubjects()} an integer;
#'   \code{groups()} the \code{colData$group} vector (or \code{NULL}).
#' @export
subjectIds <- function(x) {
  stopifnot(methods::is(x, "PhysioCohort")); names(x@subjects)
}
#' @rdname subjectIds
#' @export
nSubjects <- function(x) {
  stopifnot(methods::is(x, "PhysioCohort")); length(x@subjects)
}
#' @rdname subjectIds
#' @export
groups <- function(x) {
  stopifnot(methods::is(x, "PhysioCohort"))
  if ("group" %in% colnames(x@colData)) as.character(x@colData$group) else NULL
}

#' Add a subject to a PhysioCohort
#' @param x A \code{PhysioCohort}.
#' @param id The new subject's id (must be unique).
#' @param subject A \code{PhysioLongitudinal} or bare experiment (auto-wrapped).
#' @param group Optional group/arm label for the new subject.
#' @param meta Optional named list of extra subject-level colData values.
#' @return The updated \code{PhysioCohort}.
#' @seealso \code{\link{PhysioCohort}}
#' @export
addSubject <- function(x, id, subject, group = NA_character_, meta = list()) {
  stopifnot(methods::is(x, "PhysioCohort"), is.character(id), length(id) == 1L)
  if (id %in% names(x@subjects))
    stop(sprintf("subject id '%s' already exists", id), call. = FALSE)
  pl <- .as_pl_subject(subject, id)
  subs <- c(as.list(x@subjects), stats::setNames(list(pl), id))
  row <- S4Vectors::DataFrame(subject_id = id)
  if ("group" %in% colnames(x@colData) || !is.na(group)) row$group <- as.character(group)
  for (nm in names(meta)) row[[nm]] <- meta[[nm]]
  cd <- .rbind_fill_df(list(x@colData, row))
  PhysioCohort(subjects = subs, colData = cd, metadata = x@metadata)
}

#' Subset a PhysioCohort by group (or a predicate on colData)
#'
#' @param x A \code{PhysioCohort}.
#' @param group Optional character vector of group label(s) to keep.
#' @param subset Optional logical vector (length = number of subjects) or an
#'   expression evaluated in \code{cohortData(x)} selecting subjects to keep.
#' @return A \code{PhysioCohort} with the selected subjects.
#' @seealso \code{\link{PhysioCohort}}
#' @export
subsetCohort <- function(x, group = NULL, subset = NULL) {
  stopifnot(methods::is(x, "PhysioCohort"))
  keep <- rep(TRUE, length(x@subjects))
  if (!is.null(group)) {
    g <- groups(x)
    if (is.null(g)) stop("cohort has no 'group' column", call. = FALSE)
    keep <- keep & g %in% group
  }
  sub_expr <- substitute(subset)
  if (!is.null(sub_expr)) {
    val <- eval(sub_expr, as.data.frame(x@colData), parent.frame())
    if (!is.logical(val) || length(val) != length(x@subjects))
      stop("'subset' must evaluate to a logical of length nSubjects", call. = FALSE)
    keep <- keep & !is.na(val) & val
  }
  x[which(keep)]
}

# ---- methods ----------------------------------------------------------------

#' Size, subsetting and display for PhysioCohort
#' @param x A \code{PhysioCohort}.
#' @param i Subject index / name / logical (for \code{[}).
#' @param j,drop Ignored (present for \code{[} generic compatibility).
#' @param object A \code{PhysioCohort} (for \code{show}).
#' @param ... Ignored.
#' @return \code{length()} the number of subjects; \code{[} a subset
#'   \code{PhysioCohort}; \code{show()} is called for its side effect.
#' @name PhysioCohort-methods
#' @rdname PhysioCohort-methods
NULL

#' @rdname PhysioCohort-methods
#' @export
setMethod("length", "PhysioCohort", function(x) length(x@subjects))

#' @rdname PhysioCohort-methods
#' @export
setMethod("[", "PhysioCohort", function(x, i, ...) {
  subs <- x@subjects[i]
  ids <- names(subs)
  cd <- x@colData[match(ids, as.character(x@colData$subject_id)), , drop = FALSE]
  rownames(cd) <- NULL
  methods::new("PhysioCohort", subjects = subs, colData = cd,
               metadata = x@metadata)
})

#' @rdname PhysioCohort-methods
#' @export
setMethod("show", "PhysioCohort", function(object) {
  n <- length(object@subjects)
  cat("class: PhysioCohort\n")
  cat("subjects: ", n, "\n", sep = "")
  g <- groups(object)
  if (!is.null(g)) {
    tab <- table(g)
    cat("groups: ",
        paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", "),
        "\n", sep = "")
  }
  ns <- vapply(object@subjects, length, integer(1))
  if (n > 0) {
    cat("sessions/subject: ", min(ns), "-", max(ns),
        " (total ", sum(ns), ")\n", sep = "")
    cat("colData(", nrow(object@colData), "): ",
        paste(colnames(object@colData), collapse = ", "), "\n", sep = "")
    show_ids <- names(object@subjects)
    if (n > 6) show_ids <- c(show_ids[1:5], sprintf("... (%d more)", n - 5))
    cat("  ", paste(show_ids, collapse = ", "), "\n", sep = "")
  }
})

#' Long design table of a cohort (subjects x sessions)
#'
#' Flattens the whole cohort into one tidy \code{data.frame} with a row per
#' (subject, session): \code{subject_id}, any subject-level colData (e.g.
#' \code{group}), and the session's \code{session_id}, \code{visit_label},
#' \code{days_from_baseline} and \code{condition}. This is the iteration unit for
#' pipelines, longitudinal models and trial analyses.
#'
#' @param x A \code{PhysioCohort}.
#' @return A \code{data.frame}, one row per subject-session.
#' @seealso \code{\link{PhysioCohort}}, \code{\link{cohortData}}
#' @export
cohortDesign <- function(x) {
  stopifnot(methods::is(x, "PhysioCohort"))
  cd <- as.data.frame(x@colData, stringsAsFactors = FALSE)
  ids <- names(x@subjects)
  parts <- lapply(seq_along(ids), function(i) {
    d <- as.data.frame(design(x@subjects[[i]]), stringsAsFactors = FALSE)
    if (nrow(d) == 0L) return(NULL)
    meta <- cd[i, , drop = FALSE]; rownames(meta) <- NULL
    cbind(meta[rep(1L, nrow(d)), , drop = FALSE], d,
          row.names = NULL, stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0L)
    return(data.frame(subject_id = character(0), session_id = character(0),
                      visit_label = character(0), days_from_baseline = numeric(0),
                      stringsAsFactors = FALSE))
  do.call(rbind, parts)
}

# ---- aggregated provenance --------------------------------------------------

#' @rdname provenance
#' @export
setMethod("provenance", "PhysioCohort", function(x) {
  s <- x@subjects
  empty <- cbind(subject = character(0),
                 provenance(PhysioExperiment(S4Vectors::SimpleList())))
  if (length(s) == 0) return(empty)
  parts <- lapply(names(s), function(nm) {
    p <- provenance(s[[nm]])
    if (nrow(p) == 0) return(NULL)
    cbind(subject = rep(nm, nrow(p)), p, stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) return(empty)
  do.call(rbind, parts)
})

# ---- hierarchical feature aggregation --------------------------------------

.physio_aggregation_hash <- function(x) {
  fields <- c("data", "counts", "source", "source_rows", "features", "strata",
              "level", "steps")
  if (!is.null(x$extraction)) fields <- c(fields, "extraction")
  digest::digest(x[fields], algo = "sha256")
}

#' Aggregate features through trial, session, subject and cohort levels
#'
#' Summarizes scalar features while preserving observation identity. Higher
#' targets are reached one level at a time: cycles to trials, trials to sessions,
#' sessions to subjects, and subjects to a cohort. With the default mean, each
#' immediate child receives equal weight, regardless of how many observations
#' contributed to that child. These are descriptive summaries, not models or
#' estimates of sampling uncertainty.
#'
#' @param x A nonempty data.frame or S4Vectors DataFrame of features, or an
#'   unmodified result of this function or [extractTrialFeatures()].
#'   Extracted features retain their trial selections and sample references in
#'   `extraction`; `source_rows` indexes the rows of `extraction$data`.
#'   Tables require `subject_id` and the
#'   consecutive keys down to their observation level: `session_id`, `trial_id`,
#'   `cycle_id`. For example, trial-level input has subject, session and trial
#'   keys, with one row per trial and stratum. The deepest key determines the
#'   input level. Keys must be nonmissing, nonempty character, factor or finite
#'   numeric columns. Only keys, strata and selected features are retained.
#' @param level Required target: `"trial"`, `"session"`, `"subject"` or
#'   `"cohort"`. Must be strictly above the input level.
#' @param features Names of numeric scalar feature columns, required for a new
#'   table. Inherited when continuing a previous aggregation result.
#' @param strata Additional columns to keep separate at every level, such as
#'   `c("side", "condition")`. Defaults to no strata for a new table; inherited
#'   for a previous result. Strata cannot be changed during a chained operation.
#'   Include modality/channel identifiers here when using a long feature table.
#' @param FUN Function, or function name, reducing a nonempty numeric vector to
#'   one finite numeric value. Defaults to `mean`; `median` and `sum` are other
#'   examples. Applied separately to each feature at each intervening level.
#' @param na.rm Logical. Missing features cause an error by default. If TRUE,
#'   omit missing children separately for each feature; an all-missing group is
#'   retained with NA. Infinite values always cause an error.
#' @param ... Additional arguments to FUN. Weight vectors tied to original rows
#'   are not supported: vector lengths change at successive levels.
#' @return A `physio_aggregation` list with `data` (one row per target unit and
#'   stratum), `features`, `strata`, `level`, `source` (the retained original
#'   input table), and `source_rows` (a list mapping each output row to rows of
#'   source, including rows with missing features). `steps` records each reached
#'   level's data, source mapping, method text, arguments and missing-value policy.
#'   Each step's `counts` has output-row and feature identifiers, `n_units`
#'   (immediate children), `n_used`, `n_missing`, and `n_source` (mapped original
#'   rows, not an effective sample size). `counts` also exposes the final step.
#'   A checksum detects changed data or lineage before further aggregation.
#' @details
#' Repeated session/trial names in different parents remain distinct. Duplicate
#' full observation keys, including strata, are rejected. Identifiers are
#' normalized to character. Group order follows first appearance in the input.
#'
#' To give trials equal weight, the mean of a session with trial means 1 and 9
#' is 5 even if the trials contain different numbers of cycles. Requesting a
#' subject or cohort directly performs the same intervening steps as chaining
#' results with the same FUN and arguments. For nonlinear FUN, this is a nested
#' summary (e.g. a median of trial medians), not a pooled summary. Custom
#' functions should be deterministic and depend only on their supplied inputs;
#' recorded function text does not capture external state or closure values.
#'
#' Features must have compatible units and meaning within each group; this
#' function cannot infer those from numeric columns. It does not align or
#' average raw waveforms, construct trial containers, infer missing IDs, or
#' establish statistical independence. Absent trials are not counted; only
#' supplied rows and their missing feature values can be audited.
#'
#' For a modality returned by PhysioMoCap's `summarizeCycleFeatures()`, combine
#' its key table and feature block with `cbind(z$keys$emg, z$blocks$emg)` and
#' specify the feature names and `strata = "side"`. Different modalities can
#' be summarized separately using the same hierarchy and strata.
#' @examples
#' cycles <- data.frame(subject_id = "person1", session_id = "session1",
#'   trial_id = c("trial1", "trial1", "trial2"),
#'   cycle_id = c("cycle1", "cycle2", "cycle1"), amplitude = c(0, 2, 9))
#' trials <- aggregatePhysioFeatures(cycles, "trial", "amplitude")
#' sessions <- aggregatePhysioFeatures(trials, "session")
#' sessions$data                         # mean(c(1, 9)) = 5
#' sessions$counts                       # two trials, three original cycles
#' aggregatePhysioFeatures(cycles, "session", "amplitude")$data
#' @md
#' @export
aggregatePhysioFeatures <- function(x, level, features = NULL, strata = NULL,
                                   FUN = mean, na.rm = FALSE, ...) {
  levels <- c("cycle", "trial", "session", "subject", "cohort")
  hierarchy <- c(subject = "subject_id", session = "session_id",
                 trial = "trial_id", cycle = "cycle_id")
  if (missing(level)) stop("Specify a target level.", call. = FALSE)
  level <- match.arg(level, levels[-1L])
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("na.rm must be one nonmissing logical value.", call. = FALSE)
  method <- paste(deparse(substitute(FUN)), collapse = " ")
  fun <- match.fun(FUN)
  args <- list(...)
  if ("weights" %in% names(args) || "w" %in% names(args))
    stop("Row-specific weights are not supported across hierarchy levels.",
         call. = FALSE)
  name_vector <- function(z) is.character(z) && !anyNA(z) &&
    all(nzchar(trimws(z))) && !anyDuplicated(z)

  extraction <- NULL
  if (inherits(x, "physio_trial_features")) {
    if (!identical(x$checksum, .trial_features_hash(x)))
      stop("Extracted features were modified; repeat extraction.", call. = FALSE)
    if (!is.null(features) && !identical(features, x$features))
      stop("features must match the extraction result.", call. = FALSE)
    if (!is.null(strata) && !identical(strata, x$strata))
      stop("strata must match the extraction result, including channel.", call. = FALSE)
    features <- x$features; strata <- x$strata
    extraction <- x
    x <- x$data
  }

  if (inherits(x, "physio_aggregation")) {
    if (!identical(x$checksum, .physio_aggregation_hash(x)))
      stop("Aggregation result was modified; start from a new feature table.",
           call. = FALSE)
    if (!is.null(features) && !identical(features, x$features))
      stop("features cannot change when continuing an aggregation.", call. = FALSE)
    if (!is.null(strata) && !identical(strata, x$strata))
      stop("strata cannot change when continuing an aggregation.", call. = FALSE)
    features <- x$features; strata <- x$strata
    current <- x$level; data <- x$data; source <- x$source
    members <- x$source_rows; steps <- x$steps
    extraction <- x$extraction
  } else {
    if (!(is.data.frame(x) || methods::is(x, "DataFrame")))
      stop("x must be a feature table or aggregation result.", call. = FALSE)
    data <- as.data.frame(x)
    if (!nrow(data) || !name_vector(names(data)))
      stop("Feature table must be nonempty with unique column names.", call. = FALSE)
    if (!name_vector(features) || !length(features))
      stop("Specify unique feature column names.", call. = FALSE)
    if (is.null(strata)) strata <- character()
    if (!name_vector(strata) || any(strata %in% hierarchy))
      stop("strata must be unique column names outside the hierarchy keys.", call. = FALSE)
    present <- unname(hierarchy[hierarchy %in% names(data)])
    if (!length(present) ||
        !identical(present, unname(hierarchy[seq_along(present)])))
      stop("Hierarchy keys must start at subject_id without gaps.", call. = FALSE)
    keys <- c(present, strata)
    if (any(features %in% keys) || !all(c(keys, features) %in% names(data)))
      stop("Keys and features must be present and must not overlap.", call. = FALSE)
    data <- data[, c(keys, features), drop = FALSE]
    rownames(data) <- NULL
    for (key in keys) {
      v <- data[[key]]
      if (!is.null(dim(v)) ||
          !(is.character(v) || is.factor(v) || (is.numeric(v) && !is.object(v))) ||
          anyNA(v) || (is.numeric(v) && any(!is.finite(v))))
        stop("Invalid or missing identity in column: ", key, call. = FALSE)
      data[[key]] <- as.character(v)
      if (any(!nzchar(trimws(data[[key]]))))
        stop("Empty identity in column: ", key, call. = FALSE)
    }
    if (anyDuplicated(data[keys]))
      stop("Duplicate observation keys; include required strata or resolve duplicates.",
           call. = FALSE)
    current <- names(hierarchy)[length(present)]
    source <- data
    members <- as.list(seq_len(nrow(data)))
    steps <- list()
  }
  for (feature in features) {
    v <- data[[feature]]
    if (!is.numeric(v) || is.complex(v) || is.object(v) || !is.null(dim(v)) ||
        any(is.infinite(v)))
      stop("Features must be plain numeric columns without infinity: ", feature,
           call. = FALSE)
    if (!na.rm && anyNA(v))
      stop("Missing feature values; use na.rm = TRUE to omit explicitly.", call. = FALSE)
  }
  first <- match(current, levels); last <- match(level, levels)
  if (last <= first)
    stop("Target level must be strictly above the input level.", call. = FALSE)

  for (target in levels[seq.int(first + 1L, last)]) {
    by <- c(if (target != "cohort") unname(hierarchy[seq_len(match(target,
                           names(hierarchy)))]), strata)
    if (length(by)) {
      # Encode each key separately: delimiters inside user IDs cannot collide.
      encoded <- lapply(data[by], function(v) match(v, unique(v)))
      labels <- do.call(paste, c(encoded, sep = ":"))
      group <- match(labels, unique(labels))
    } else group <- rep.int(1L, nrow(data))
    indices <- unname(split(seq_len(nrow(data)),
                            factor(group, levels = seq_len(max(group)))))
    out <- data[vapply(indices, `[`, integer(1), 1L), by, drop = FALSE]
    rownames(out) <- NULL
    next_members <- lapply(indices, function(i) unlist(members[i], use.names = FALSE))
    counts <- vector("list", length(indices) * length(features)); pos <- 0L
    for (feature in features) {
      values <- numeric(length(indices))
      for (i in seq_along(indices)) {
        v <- data[[feature]][indices[[i]]]
        used <- v[!is.na(v)]
        if (!length(used)) value <- NA_real_ else {
          value <- tryCatch(do.call(fun, c(list(used), args)), error = function(e)
            stop("FUN failed at ", target, " row ", i, ", feature ", feature,
                 ": ", conditionMessage(e), call. = FALSE))
          if (!is.numeric(value) || is.complex(value) || is.object(value) ||
              length(value) != 1L || !is.null(dim(value)) || !is.finite(value))
            stop("FUN must return one finite numeric value (", target,
                 " row ", i, ", feature ", feature, ").", call. = FALSE)
        }
        values[i] <- value
        pos <- pos + 1L
        counts[[pos]] <- data.frame(output_row = i, feature = feature,
          n_units = length(v), n_used = length(used), n_missing = sum(is.na(v)),
          n_source = length(next_members[[i]]), stringsAsFactors = FALSE)
      }
      out[[feature]] <- values
    }
    counts <- do.call(rbind, counts)
    steps[[target]] <- list(data = out, counts = counts, source_rows = next_members,
      method = method, function_text = paste(deparse(fun), collapse = "\n"),
      arguments = args, na.rm = na.rm)
    data <- out; members <- next_members
  }
  result <- list(data = data, counts = counts, source = source, source_rows = members,
                 features = features, strata = strata, level = level, steps = steps)
  if (!is.null(extraction)) result$extraction <- extraction
  result$checksum <- .physio_aggregation_hash(result)
  structure(result, class = "physio_aggregation")
}
