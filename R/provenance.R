#' Provenance / audit trail for PhysioExperiment objects
#'
#' Every analysis operation that returns a modified \code{PhysioExperiment}
#' should record an append-only, timestamped, agent-attributed provenance entry
#' following the W3C PROV data model (Lebo et al. 2013). Each entry captures a
#' PROV \emph{activity} (the operation), the \emph{entity} it generated, the
#' inputs it \emph{used}, the responsible \emph{agent}, its start/end times, and
#' the parameters (also serialized as JSON for downstream export).
#'
#' The log is stored in the object's \code{metadata()} under the key
#' \code{"provenance"}. Storing it in metadata (rather than a dedicated S4 slot)
#' is deliberate: objects serialized before provenance existed, or by any other
#' \code{SummarizedExperiment} tool, deserialize cleanly and simply report an
#' empty log, with no need for a class-version \code{updateObject} migration.
#'
#' @name provenance
#' @references
#' Lebo, T., Sahoo, S., & McGuinness, D. (2013). PROV-O: The PROV Ontology.
#' W3C Recommendation.
#' @seealso \code{\link{logStep}}, \code{\link{withProvenance}},
#'   \code{\link{recordActivity}}
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' pe <- logStep(pe, "filterSignals", params = list(low = 1, high = 40))
#' provenance(pe)
NULL

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.provenanceEntries <- function(x) {
  p <- S4Vectors::metadata(x)[["provenance"]]
  if (is.null(p)) list() else p
}

# Human-readable "name=value, ..." rendering of a parameter list.
.paramsToString <- function(params) {
  if (is.null(params) || length(params) == 0L) return("")
  nms <- names(params)
  parts <- vapply(seq_along(params), function(i) {
    v <- params[[i]]
    vs <- if (length(v) > 1L) paste0("[", paste(format(v), collapse = ","), "]") else format(v)[1]
    nm <- if (is.null(nms)) "" else nms[i]
    if (is.null(nm) || is.na(nm) || nm == "") vs else paste0(nm, "=", vs)
  }, character(1))
  paste(parts, collapse = ", ")
}

# Minimal JSON serialization of a parameter list (PROV params-as-json), so the
# audit trail is machine-readable without adding a JSON package dependency.
.paramsToJson <- function(params) {
  if (is.null(params) || length(params) == 0L) return("{}")
  esc <- function(s) vapply(enc2utf8(as.character(s)), function(value) {
    if (is.na(value)) return(NA_character_)
    code <- utf8ToInt(value)
    paste0(vapply(code, function(k) {
      if (k == 34L) return('\\"')
      if (k == 92L) return('\\\\')
      if (k < 32L) return(sprintf("\\u%04x", k))
      intToUtf8(k)
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  render <- function(v) {
    if (is.null(v) || length(v) == 0L) return("null")
    if (is.list(v)) return(.paramsToJson(v))
    if (is.character(v) || is.factor(v)) {
      q <- ifelse(is.na(v), "null", paste0('"', esc(v), '"'))
    } else if (is.logical(v)) {
      q <- ifelse(is.na(v), "null", tolower(as.character(v)))
    } else {
      q <- ifelse(is.na(v), "null", format(v, digits = 17, trim = TRUE, scientific = FALSE))
    }
    if (length(v) > 1L) paste0("[", paste(q, collapse = ","), "]") else q
  }
  nms <- names(params)
  if (is.null(nms)) nms <- rep("", length(params))
  parts <- vapply(seq_along(params), function(i) {
    key <- if (nzchar(nms[i])) nms[i] else as.character(i)
    paste0('"', esc(key), '":', render(params[[i]]))
  }, character(1))
  paste0("{", paste(parts, collapse = ","), "}")
}

# The responsible PROV agent, "user@host", from the session.
.provAgent <- function() {
  u <- tryCatch(unname(Sys.info()[["user"]]), error = function(e) NA_character_) %||% NA_character_
  h <- tryCatch(unname(Sys.info()[["nodename"]]), error = function(e) NA_character_) %||% NA_character_
  if (is.na(u)) NA_character_ else if (is.na(h)) u else paste0(u, "@", h)
}

# Build a fully-populated PROV-O activity record.
.buildEntry <- function(activity, params = list(), input_assay = NA_character_,
                        output_assay = NA_character_, agent = NA_character_,
                        package = NA_character_, version = NA_character_,
                        started = NULL, ended = NULL) {
  if (is.null(ended)) ended <- Sys.time()
  if (is.null(started)) started <- ended
  if (length(agent) != 1L || is.na(agent)) agent <- .provAgent()
  user <- if (length(agent) == 1L && !is.na(agent)) sub("@.*$", "", agent) else NA_character_
  entity <- paste0("pe:", activity, "@", format(ended, "%Y-%m-%dT%H:%M:%OS3"))
  list(
    activity      = activity,                       # prov:Activity
    step          = activity,                       # legacy alias
    entity        = entity,                         # prov:Entity (generated)
    used          = as.character(input_assay %||% NA_character_),   # prov:used
    generated     = as.character(output_assay %||% NA_character_),  # prov:wasGeneratedBy
    agent         = agent,                          # prov:wasAssociatedWith
    user          = user,                           # legacy alias
    startedAtTime = started,                        # prov:startedAtTime
    endedAtTime   = ended,                          # prov:endedAtTime
    timestamp     = ended,                          # legacy alias
    params        = params,
    params_json   = .paramsToJson(params),          # params-as-json
    package       = package,                        # software agent
    version       = version                         # software agent version
  )
}

#' Get or set the provenance log of a PhysioExperiment
#'
#' @param x A \code{PhysioExperiment} object.
#' @param value A provenance entry list (used by the setter).
#' @return \code{provenance()} returns a \code{data.frame} with one row per
#'   recorded PROV activity. Columns include the PROV-O fields \code{activity},
#'   \code{entity}, \code{used}, \code{generated}, \code{agent},
#'   \code{startedAtTime}, \code{endedAtTime}, \code{params_json}, plus the
#'   back-compatible \code{step}, \code{timestamp}, \code{user}, \code{package},
#'   \code{version}, and \code{params}. Empty if none recorded.
#' @export
setGeneric("provenance", function(x) standardGeneric("provenance"))

#' @rdname provenance
#' @export
setMethod("provenance", "PhysioExperiment", function(x) {
  p <- .provenanceEntries(x)
  if (length(p) == 0L) {
    return(data.frame(
      step = character(0), activity = character(0), entity = character(0),
      used = character(0), generated = character(0), agent = character(0),
      user = character(0), package = character(0), version = character(0),
      startedAtTime = as.POSIXct(character(0)),
      endedAtTime = as.POSIXct(character(0)),
      timestamp = as.POSIXct(character(0)),
      params = character(0), params_json = character(0),
      stringsAsFactors = FALSE
    ))
  }
  chr <- function(field) vapply(p, function(e) as.character(e[[field]] %||% NA_character_), character(1))
  tim <- function(field, fallback = "timestamp") as.POSIXct(vapply(p, function(e) {
    v <- e[[field]] %||% (e[[fallback]] %||% NA)
    as.numeric(v)
  }, numeric(1)), origin = "1970-01-01", tz = "")
  data.frame(
    step          = vapply(p, function(e) e$step %||% e$activity %||% NA_character_, character(1)),
    activity      = vapply(p, function(e) e$activity %||% e$step %||% NA_character_, character(1)),
    entity        = chr("entity"),
    used          = chr("used"),
    generated     = chr("generated"),
    agent         = vapply(p, function(e) e$agent %||% e$user %||% NA_character_, character(1)),
    user          = chr("user"),
    package       = chr("package"),
    version       = chr("version"),
    startedAtTime = tim("startedAtTime"),
    endedAtTime   = tim("endedAtTime"),
    timestamp     = tim("timestamp"),
    params        = vapply(p, function(e) .paramsToString(e$params), character(1)),
    params_json   = vapply(p, function(e) e$params_json %||% .paramsToJson(e$params), character(1)),
    stringsAsFactors = FALSE
  )
})

#' @rdname provenance
#' @export
setGeneric("provenance<-", function(x, value) standardGeneric("provenance<-"))

#' @rdname provenance
#' @export
setReplaceMethod("provenance", "PhysioExperiment", function(x, value) {
  S4Vectors::metadata(x)[["provenance"]] <- value
  x
})

#' Append a provenance activity
#'
#' Appends one PROV activity to the object's audit trail. Two forms are
#' supported: a high-level form that builds a full PROV-O record from named
#' fields, and a low-level form that appends a pre-built entry list.
#'
#' @param x A \code{PhysioExperiment}.
#' @param entry Either a pre-built entry \code{list} (low-level form) or a
#'   character scalar naming the \code{activity} (equivalent to passing
#'   \code{activity=}).
#' @param activity Character scalar naming the PROV activity (the operation).
#' @param params Named list of parameters to record.
#' @param input_assay,output_assay The assay(s) the activity used / generated
#'   (PROV \code{used} / \code{wasGeneratedBy}).
#' @param agent The responsible agent (defaults to \code{user@host}).
#' @param software_version,package Software agent name and version.
#' @return \code{x} with the activity appended to its provenance log.
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' pe <- appendProvenance(pe, activity = "filterSignals",
#'                        params = list(low = 1, high = 40),
#'                        input_assay = "raw", output_assay = "filtered")
#' provenance(pe)$activity
#' @export
appendProvenance <- function(x, entry = NULL, activity = NULL, params = list(),
                             input_assay = NA_character_, output_assay = NA_character_,
                             agent = NA_character_, software_version = NA_character_,
                             package = NA_character_) {
  stopifnot(methods::is(x, "PhysioExperiment"))
  if (is.character(entry)) {           # positional high-level form: entry IS the activity
    activity <- entry
    entry <- NULL
  }
  if (is.null(entry)) {
    stopifnot(is.character(activity), length(activity) == 1L, !is.na(activity))
    entry <- .buildEntry(activity, params = params, input_assay = input_assay,
                         output_assay = output_assay, agent = agent,
                         package = package, version = software_version)
  }
  stopifnot(is.list(entry))
  p <- .provenanceEntries(x)
  p[[length(p) + 1L]] <- entry
  S4Vectors::metadata(x)[["provenance"]] <- p
  x
}

#' Record an analysis step in the provenance log
#'
#' Appends a timestamped, agent-attributed PROV activity. Call this from any
#' operation that returns a modified \code{PhysioExperiment}.
#'
#' @param x A \code{PhysioExperiment}.
#' @param step Character scalar naming the operation (e.g. \code{"filterSignals"}).
#' @param params Named list of parameters to record.
#' @param package,version Optional originating package name and version.
#' @return \code{x} with the step appended (the input is unchanged; a modified
#'   copy is returned).
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' pe <- logStep(pe, "filterSignals", params = list(low = 1, high = 40))
#' provenance(pe)
#' @export
logStep <- function(x, step, params = list(),
                    package = NA_character_, version = NA_character_) {
  stopifnot(
    methods::is(x, "PhysioExperiment"),
    is.character(step), length(step) == 1L, !is.na(step)
  )
  appendProvenance(x, entry = .buildEntry(step, params = params,
                                          package = package, version = version))
}

#' Carry a provenance log onto a derived object and record a step
#'
#' When an operation constructs a NEW \code{PhysioExperiment} from an input
#' (rather than modifying it in place), use this to copy the input's provenance
#' onto the result and append the current step in one call.
#'
#' @param from The source object whose provenance should be carried forward.
#' @param to The newly derived object.
#' @param step,params,package,version As in \code{\link{logStep}}.
#' @return \code{to} carrying \code{from}'s provenance plus the new step.
#' @seealso \code{\link{recordActivity}} for the expression-wrapping form.
#' @examples
#' from <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' from <- logStep(from, "import")
#' to <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' to <- withProvenance(from, to, "resample", params = list(to = 50))
#' provenance(to)
#' @export
withProvenance <- function(from, to, step, params = list(),
                           package = NA_character_, version = NA_character_) {
  stopifnot(methods::is(from, "PhysioExperiment"), methods::is(to, "PhysioExperiment"))
  S4Vectors::metadata(to)[["provenance"]] <- .provenanceEntries(from)
  logStep(to, step, params = params, package = package, version = version)
}

#' Append a provenance step, capturing an optional seed
#'
#' A thin convenience over \code{\link{logStep}} that records an append-only
#' provenance entry on the object's metadata log (never dropping prior entries),
#' optionally capturing an integer \code{seed} in the entry parameters so the
#' step is reproducible under a fixed seed.
#'
#' @param pe A \code{PhysioExperiment} object.
#' @param step Character step / activity name.
#' @param params Named list of parameters recorded with the step.
#' @param seed Optional integer seed captured in the entry parameters.
#' @param package,version Optional producing-package name and version.
#' @return \code{pe} with the entry appended to its provenance log.
#' @seealso \code{\link{logStep}}, \code{\link{provenance}}
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' pe <- addProvenance(pe, "conformalInterval", seed = 42)
#' provenance(pe)
#' @export
addProvenance <- function(pe, step, params = list(), seed = NULL,
                          package = NA_character_, version = NA_character_) {
  stopifnot(methods::is(pe, "PhysioExperiment"))
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed), length(seed) == 1L, !is.na(seed),
              seed == as.integer(seed))
    params$seed <- as.integer(seed)
  }
  logStep(pe, step, params = params, package = package, version = version)
}

#' Record a PROV activity around an operation
#'
#' Evaluates \code{expr} (an operation that transforms \code{x} and returns a
#' \code{PhysioExperiment}), timing it to fill the PROV \code{startedAtTime} /
#' \code{endedAtTime}, and appends one activity to the result. The input's
#' provenance log is carried onto the result if the operation dropped it.
#'
#' @param x The input \code{PhysioExperiment}.
#' @param activity Character scalar naming the PROV activity.
#' @param expr An expression, evaluated lazily, that returns the modified object
#'   (e.g. \code{filterSignals(x, low = 1)}).
#' @param params Named list of parameters to record.
#' @param input_assay,output_assay,agent,software_version,package As in
#'   \code{\link{appendProvenance}}.
#' @return The object returned by \code{expr}, with the timed activity appended.
#' @seealso \code{\link{withProvenance}}, \code{\link{logStep}}
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' scale2 <- function(p) { SummarizedExperiment::assay(p, "raw") <-
#'   SummarizedExperiment::assay(p, "raw") * 2; p }
#' pe <- recordActivity(pe, "rescale", scale2(pe), params = list(gain = 2))
#' provenance(pe)$activity
#' @export
recordActivity <- function(x, activity, expr, params = list(),
                           input_assay = NA_character_, output_assay = NA_character_,
                           agent = NA_character_, software_version = NA_character_,
                           package = NA_character_) {
  stopifnot(methods::is(x, "PhysioExperiment"),
            is.character(activity), length(activity) == 1L, !is.na(activity))
  started <- Sys.time()
  result <- expr                 # force the promise here so we time the ops call
  ended <- Sys.time()
  if (!methods::is(result, "PhysioExperiment")) {
    stop("`expr` must evaluate to a PhysioExperiment", call. = FALSE)
  }
  # carry the input's log onto the result if the operation dropped history
  if (length(.provenanceEntries(result)) < length(.provenanceEntries(x))) {
    S4Vectors::metadata(result)[["provenance"]] <- .provenanceEntries(x)
  }
  entry <- .buildEntry(activity, params = params, input_assay = input_assay,
                       output_assay = output_assay, agent = agent,
                       package = package, version = software_version,
                       started = started, ended = ended)
  appendProvenance(result, entry = entry)
}

#' Deterministic hash of the provenance audit trail
#'
#' Returns a stable hash of the *semantic* content of the provenance log - the
#' ordered sequence of processing steps and their parameters - deliberately
#' excluding wall-clock timestamps and the machine-specific agent/user. The same
#' pipeline (same steps, same parameters) therefore always hashes to the same
#' value, while any change to a step or its parameters changes the hash. Useful
#' as a reproducibility fingerprint for a processed object.
#'
#' @param x A \code{PhysioExperiment} (or any object with a
#'   \code{\link{provenance}} method).
#' @param algo Hash algorithm passed to \code{digest::digest} (default
#'   \code{"xxhash64"}).
#' @return A single character hash string.
#' @seealso \code{\link{provenance}}, \code{\link{logStep}}
#' @examples
#' pe <- PhysioExperiment(assays = list(raw = matrix(rnorm(40), 10, 4)),
#'                        samplingRate = 100)
#' pe <- logStep(pe, "filterSignals", params = list(low = 1, high = 40))
#' provenanceHash(pe)
#' @importFrom digest digest
#' @export
provenanceHash <- function(x, algo = "xxhash64") {
  p <- provenance(x)
  # deterministic content only: drop timestamps, the machine-specific agent/user,
  # and the entity id (which embeds the wall-clock time). The step identity is
  # preserved by the step/activity/params columns. Hash the columns structurally
  # (as a list) so no field value can collide across a delimiter.
  det <- setdiff(names(p),
                 c("startedAtTime", "endedAtTime", "timestamp", "agent", "user",
                   "entity"))
  digest::digest(as.list(p[det]), algo = algo)
}
