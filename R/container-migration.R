# Migration of containers saved before the multimodal classes were unified.
#
# Two shapes exist in the wild:
#   * the Core container (slots streams/clock) -- still valid, nothing to do;
#   * the PhysioCrossModal container (slots experiments/alignment/sampleMap/
#     couplingResults) -- structurally invalid under the unified definition.
#
# The second one still LOADS: R runs no validity check on readRDS(), so the
# caller is handed a broken object with no error and only finds out when a slot
# is first touched. Its payload survives as ordinary attributes, which is what
# makes conversion possible. The entry point therefore has to be both an
# updateObject() method (idiomatic, dispatches on the stored class) and an
# explicit loader (because nothing calls updateObject() by itself).

#' Is this a container saved before the classes were unified?
#'
#' @param x Any object.
#' @return \code{TRUE} when \code{x} carries the pre-unification
#'   \code{experiments}/\code{alignment} slot layout.
#' @keywords internal
.is_legacy_container <- function(x) {
  a <- names(attributes(x))
  !is.null(a) && all(c("experiments", "alignment") %in% a)
}

#' Convert a pre-unification multimodal container
#'
#' Rebuilds a container saved when the multimodal class carried
#' \code{experiments} / \code{alignment} / \code{sampleMap} /
#' \code{couplingResults} into the unified \code{\link{MultiPhysioExperiment}}.
#'
#' Two things are deliberately not converted:
#' \itemize{
#'   \item The old model had **no absolute origin**, so \code{clock$t0} is set to
#'     \code{NA} rather than to a fabricated zero.
#'   \item \code{sampleMap} and the cached \code{couplingResults} have no
#'     counterpart in the unified model. They are carried into
#'     \code{clock$migrated_from} with their origin rather than dropped, and the
#'     cache is **not** reused: it was keyed without reference to the signals or
#'     the clock, so nothing here can show it still matches its inputs.
#' }
#' The declared sampling rates in the old alignment table are checked against
#' the real streams; a disagreement is reported rather than silently resolved.
#'
#' @param object A legacy container (or any object; non-legacy input is
#'   returned unchanged).
#' @param verbose Emit a message describing what was carried over.
#' @return A \code{\link{MultiPhysioExperiment}}.
#' @seealso \code{\link{readPhysioRDS}}
#' @export
#' @examples
#' # round-trips a current object unchanged
#' m <- MultiPhysioExperiment(a = PhysioExperiment(
#'   assays = list(raw = matrix(0, 10, 2)), samplingRate = 10))
#' identical(migrateContainer(m), m)
migrateContainer <- function(object, verbose = TRUE) {
  if (!.is_legacy_container(object)) return(object)

  exps <- attr(object, "experiments")
  al <- as.data.frame(attr(object, "alignment"), stringsAsFactors = FALSE)
  sm <- attr(object, "sampleMap")
  cr <- attr(object, "couplingResults")
  nm <- names(exps)

  rates <- vapply(exps, function(p) as.numeric(samplingRate(p)), numeric(1))

  # The old table is validated against the streams it claims to describe before
  # anything is adopted from it. Filling a missing row with zero would assert a
  # simultaneous start the source never recorded -- the same silent completion
  # the constructor refuses -- and a duplicate or unknown row means the table
  # and the data disagree about what was recorded.
  if (nrow(al) == 0L || !all(c("modality", "offset") %in% names(al))) {
    stop(paste0("this container's alignment table is missing or has no ",
                "'modality'/'offset' columns, so the start offsets cannot be ",
                "recovered. Supply them explicitly with ",
                "MultiPhysioExperiment(streams = experiments(x), offsets = ...)."),
         call. = FALSE)
  }
  mods <- as.character(al$modality)
  problems <- character(0)
  if (anyDuplicated(mods)) {
    problems <- c(problems, sprintf("more than one row for %s",
                                    paste(unique(mods[duplicated(mods)]), collapse = ", ")))
  }
  if (length(setdiff(nm, mods))) {
    problems <- c(problems, sprintf("no row for %s",
                                    paste(setdiff(nm, mods), collapse = ", ")))
  }
  if (length(setdiff(mods, nm))) {
    problems <- c(problems, sprintf("rows for streams that do not exist: %s",
                                    paste(setdiff(mods, nm), collapse = ", ")))
  }
  raw_off <- stats::setNames(as.numeric(al$offset), mods)
  if (any(!is.finite(raw_off[intersect(mods, nm)]))) {
    problems <- c(problems, "non-finite offsets")
  }
  if (length(problems)) {
    stop(sprintf(paste0("this container's alignment table does not describe its ",
                        "own streams (%s). The start offsets cannot be recovered ",
                        "from it, and assuming zero would assert a simultaneous ",
                        "start the recording never claimed. Supply them ",
                        "explicitly with MultiPhysioExperiment(streams = ",
                        "experiments(x), offsets = ...)."),
                 paste(problems, collapse = "; ")), call. = FALSE)
  }
  offs <- raw_off[nm]

  rate_mismatch <- character(0)
  if ("samplingRate" %in% names(al)) {
    declared <- stats::setNames(as.numeric(al$samplingRate), mods)[nm]
    bad <- which(is.finite(declared) & abs(declared - rates[nm]) > 1e-9)
    if (length(bad)) {
      rate_mismatch <- sprintf("%s: table says %g Hz, stream is %g Hz",
                               nm[bad], declared[bad], rates[nm][bad])
    }
  }

  out <- MultiPhysioExperiment(streams = exps, t0 = NA_real_, offsets = offs)
  out@clock$migrated_from <- list(
    class = "MultiPhysioExperiment (PhysioCrossModal, pre-unification)",
    absolute_origin = "absent in the source model; t0 recorded as NA, not fabricated",
    alignment = al,
    sampleMap = sm,
    couplingResults = cr,
    cache_reuse = "not reused: the old cache key did not cover the signals or the clock",
    rate_mismatch = rate_mismatch)

  if (isTRUE(verbose)) {
    message(sprintf(
      paste0("migrated a pre-unification container: %d stream(s); t0 unknown (NA); ",
             "%d sampleMap row(s) and %d cached result(s) set aside unused%s"),
      length(exps), NROW(sm), length(cr),
      if (length(rate_mismatch))
        sprintf("; ALIGNMENT DISAGREES WITH THE DATA: %s",
                paste(rate_mismatch, collapse = "; ")) else ""))
  }
  out
}

#' @rdname migrateContainer
#' @param ... Ignored.
#' @param verbose Emit a message describing what was carried over.
#' @export
setMethod("updateObject", "MultiPhysioExperiment",
  function(object, ..., verbose = FALSE) migrateContainer(object, verbose = verbose))

#' Read a saved object, migrating a pre-unification container
#'
#' \code{readRDS()} performs no validity check, so a container saved before the
#' multimodal classes were unified comes back structurally invalid and says
#' nothing about it; the failure only surfaces when a slot is first touched.
#' This reader detects that layout and converts it, reporting what it did.
#'
#' The file is only read - it is never rewritten. Save the returned object
#' somewhere new if the converted form should persist.
#'
#' @param file Path to an \code{.rds} file.
#' @param migrate Convert a legacy container (default). When \code{FALSE} the
#'   object is returned exactly as stored, and a legacy container is reported
#'   with a warning rather than silently handed over as invalid.
#' @param verbose Describe the migration.
#' @return The stored object, migrated when applicable.
#' @seealso \code{\link{migrateContainer}}
#' @export
readPhysioRDS <- function(file, migrate = TRUE, verbose = TRUE) {
  obj <- readRDS(file)
  if (!.is_legacy_container(obj)) return(obj)
  if (!isTRUE(migrate)) {
    warning(sprintf(paste0("'%s' holds a pre-unification container. It is not ",
                           "valid under the current class definition; call ",
                           "migrateContainer() on it."), file), call. = FALSE)
    return(obj)
  }
  migrateContainer(obj, verbose = verbose)
}
