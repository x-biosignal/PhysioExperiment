# =============================================================================
# Plugin / registration API
#
# PhysioExperiment hosts a lightweight, package-private registry so that domain
# packages (PhysioIO, PhysioAnalysis, ...) can advertise the file readers /
# writers and named operations they provide without PhysioExperiment taking a hard
# dependency on them. Downstream packages call the register*() verbs from their
# own .onLoad(); any package can then discover or dispatch via available*() /
# get*(). See ?`physio-registry`.
# =============================================================================

# Resolve the package's registry store, an environment anchored in a session
# option so a PhysioExperiment namespace reload (devtools::load_all, or
# unloadNamespace + library) re-resolves to the SAME environment instead of a
# fresh one -- so registrations made by already-loaded consumer packages, whose
# own .onLoad does not re-run, are not silently lost. Parent is emptyenv() so
# nothing leaks in via lexical scoping.
#
# This MUST run at runtime (from .onLoad and lazily from the verbs), NOT as a
# top-level binding: a lazy-loaded top-level value is deserialized without
# re-running its initializer, so an options() side effect placed there would
# never fire in an installed package.
.registry_env <- function() {
  e <- getOption("PhysioExperiment.registry")
  if (!is.environment(e)) {
    e <- new.env(parent = emptyenv())
    options(PhysioExperiment.registry = e)
  }
  e
}

# Ensure the three stores exist and return the store environment. Idempotent, so
# it is safe to call from .onLoad on every (re)load and lazily from every
# registry verb -- the lazy call also guards the .onLoad ordering edge (a
# consumer that somehow runs before PhysioExperiment's .onLoad).
.ensure_registry <- function() {
  e <- .registry_env()
  for (kind in c("readers", "writers", "operations")) {
    if (is.null(e[[kind]])) assign(kind, list(), envir = e)
  }
  invisible(e)
}

# Validate + normalize a registry key to a single trimmed lower-case string so
# lookups are case- and surrounding-whitespace-insensitive ("BrainVision",
# "brainvision" and " brainvision " are the same reader).
.registry_key <- function(x, what = "key") {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("registry %s must be a single non-empty string", what),
         call. = FALSE)
  }
  x <- trimws(x)
  if (!nzchar(x)) {
    stop(sprintf("registry %s must be a single non-empty string", what),
         call. = FALSE)
  }
  tolower(x)
}

# Shared set/get/remove engine for all three kinds. `entry` is a list carrying
# `fn` plus kind-specific metadata (ext or modality).
.registry_set <- function(kind, key, entry, overwrite, what) {
  e <- .ensure_registry()
  key <- .registry_key(key, what)
  if (!is.function(entry$fn)) {
    stop("`fn` must be a function", call. = FALSE)
  }
  store <- e[[kind]]
  if (!is.null(store[[key]]) && !isTRUE(overwrite)) {
    stop(sprintf(
      "%s '%s' is already registered; pass overwrite = TRUE to replace it.",
      what, key), call. = FALSE)
  }
  store[[key]] <- entry
  assign(kind, store, envir = e)
  invisible(entry$fn)
}

.registry_get <- function(kind, key, what) {
  e <- .ensure_registry()
  key <- .registry_key(key, what)
  entry <- e[[kind]][[key]]
  if (is.null(entry)) {
    avail <- names(e[[kind]])
    stop(sprintf("no %s registered for '%s'.%s", what, key,
                 if (length(avail)) {
                   paste0(" Registered: ", paste(sort(avail), collapse = ", "), ".")
                 } else " Registry is empty."),
         call. = FALSE)
  }
  entry$fn
}

.registry_remove <- function(kind, key, what) {
  e <- .ensure_registry()
  key <- .registry_key(key, what)
  store <- e[[kind]]
  existed <- !is.null(store[[key]])
  store[[key]] <- NULL
  assign(kind, store, envir = e)
  invisible(existed)
}

# Build the discovery data.frame for a kind; `meta` is the extra column name.
.registry_table <- function(kind, keycol, meta) {
  e <- .ensure_registry()
  store <- e[[kind]]
  keys <- names(store)
  if (is.null(keys)) keys <- character(0)   # empty list(): names() is NULL
  vals <- vapply(store, function(e) {
    v <- e[[meta]]
    v <- v[!is.na(v)]                        # drop NA tags rather than print "NA"
    if (length(v) == 0) NA_character_ else paste(v, collapse = ",")
  }, character(1), USE.NAMES = FALSE)
  out <- data.frame(keys, vals, stringsAsFactors = FALSE)
  names(out) <- c(keycol, meta)
  rownames(out) <- NULL
  out[order(out[[keycol]]), , drop = FALSE]
}


#' PhysioExperiment plugin / registration API
#'
#' A lightweight, package-private registry so that domain packages can advertise
#' the file readers/writers and named operations they provide without PhysioExperiment
#' depending on them. Downstream packages call the `register*()` verbs from their
#' own `.onLoad()` (with `overwrite = TRUE`, so re-loading a session is
#' idempotent); any package discovers or dispatches through `available*()` and
#' `get*()`.
#'
#' @section Extension contract:
#' A reader or writer is a function whose first argument is a file path (plus
#' `...`). An operation is any function, keyed by `name` and tagged with an
#' optional `modality` (e.g. `"eeg"`). Keys are matched case-insensitively and
#' with surrounding whitespace trimmed; the `available*()` discovery tables
#' therefore report keys in their normalized (lower-cased) form. Registering a
#' key that already exists errors unless `overwrite = TRUE`. A typical consumer
#' registers in its `.onLoad` with `overwrite = TRUE`, which keeps registration
#' idempotent across dev reloads (`devtools::load_all`):
#'
#' ```r
#' .onLoad <- function(libname, pkgname) {
#'   PhysioExperiment::registerReader("brainvision", readBrainVision,
#'                              ext = c("vhdr", "eeg"), overwrite = TRUE)
#' }
#' ```
#'
#' The store is anchored in a session option, so it survives a PhysioExperiment reload
#' and is shared across packages. Resolution is last-writer-wins: if two packages
#' register the same key with `overwrite = TRUE`, the one loaded later prevails;
#' `available*()` shows which function is currently active.
#'
#' @name physio-registry
#' @family plugin-api
#' @keywords internal
NULL


#' Register and dispatch file readers
#'
#' @param format Case-insensitive format key (e.g. `"brainvision"`).
#' @param fn The reader function; its first argument should be a file path.
#' @param ext Optional character vector of file extensions handled (no dot).
#' @param overwrite If `FALSE` (default), registering an existing `format`
#'   errors; `TRUE` replaces it (use this in a package `.onLoad`).
#' @return `registerReader()` invisibly returns `fn`; `getReader()` returns the
#'   registered function; `availableReaders()` returns a data.frame of
#'   `format`/`ext` (the `format` column holds normalized, lower-cased keys, and
#'   the frame has zero rows when nothing is registered); `unregisterReader()`
#'   invisibly returns `TRUE` if a reader was removed.
#' @family plugin-api
#' @examples
#' registerReader("demo_csv", function(file, ...) read.csv(file, ...),
#'                ext = "csv", overwrite = TRUE)
#' availableReaders()
#' r <- getReader("DEMO_CSV")   # case-insensitive
#' unregisterReader("demo_csv")
#' @export
registerReader <- function(format, fn, ext = NULL, overwrite = FALSE) {
  .registry_set("readers", format, list(fn = fn, ext = ext), overwrite, "reader")
}

#' @rdname registerReader
#' @export
getReader <- function(format) .registry_get("readers", format, "reader")

#' @rdname registerReader
#' @export
availableReaders <- function() .registry_table("readers", "format", "ext")

#' @rdname registerReader
#' @export
unregisterReader <- function(format) .registry_remove("readers", format, "reader")


#' Register and dispatch file writers
#'
#' @inheritParams registerReader
#' @param fn The writer function; its first arguments should be the object to
#'   write and a file path.
#' @return `registerWriter()` invisibly returns `fn`; `getWriter()` returns the
#'   registered function; `availableWriters()` returns a data.frame of
#'   `format`/`ext` (the `format` column holds normalized, lower-cased keys, and
#'   the frame has zero rows when nothing is registered); `unregisterWriter()`
#'   invisibly returns `TRUE` if a writer was removed.
#' @family plugin-api
#' @examples
#' registerWriter("demo_csv", function(x, file, ...) write.csv(x, file, ...),
#'                ext = "csv", overwrite = TRUE)
#' availableWriters()
#' unregisterWriter("demo_csv")
#' @export
registerWriter <- function(format, fn, ext = NULL, overwrite = FALSE) {
  .registry_set("writers", format, list(fn = fn, ext = ext), overwrite, "writer")
}

#' @rdname registerWriter
#' @export
getWriter <- function(format) .registry_get("writers", format, "writer")

#' @rdname registerWriter
#' @export
availableWriters <- function() .registry_table("writers", "format", "ext")

#' @rdname registerWriter
#' @export
unregisterWriter <- function(format) .registry_remove("writers", format, "writer")


#' Register and dispatch named operations
#'
#' @param name Case-insensitive operation key.
#' @param fn The operation function.
#' @param modality For `registerOperation()`, an optional modality tag (e.g.
#'   `"eeg"`, `"ecg"`; a character vector is allowed for multi-modality ops).
#'   For `availableOperations()`, an optional modality to filter the table to
#'   operations tagged with it (`NULL`, the default, returns all).
#' @param overwrite If `FALSE` (default), registering an existing `name` errors;
#'   `TRUE` replaces it.
#' @return `registerOperation()` invisibly returns `fn`; `getOperation()`
#'   returns the registered function; `availableOperations()` returns a
#'   data.frame of `name`/`modality` (the `name` column holds normalized,
#'   lower-cased keys); `unregisterOperation()` invisibly returns `TRUE` if an
#'   operation was removed.
#' @family plugin-api
#' @examples
#' registerOperation("demo_detrend", function(x) x - mean(x),
#'                   modality = "generic", overwrite = TRUE)
#' availableOperations()
#' availableOperations(modality = "generic")
#' op <- getOperation("demo_detrend")
#' unregisterOperation("demo_detrend")
#' @export
registerOperation <- function(name, fn, modality = NULL, overwrite = FALSE) {
  .registry_set("operations", name, list(fn = fn, modality = modality),
                overwrite, "operation")
}

#' @rdname registerOperation
#' @export
getOperation <- function(name) .registry_get("operations", name, "operation")

#' @rdname registerOperation
#' @export
availableOperations <- function(modality = NULL) {
  tbl <- .registry_table("operations", "name", "modality")
  if (!is.null(modality)) {
    modality <- .registry_key(modality, "modality")
    keep <- vapply(tbl$modality, function(m) {
      !is.na(m) && modality %in% tolower(trimws(strsplit(m, ",", fixed = TRUE)[[1]]))
    }, logical(1), USE.NAMES = FALSE)
    tbl <- tbl[keep, , drop = FALSE]
    rownames(tbl) <- NULL
  }
  tbl
}

#' @rdname registerOperation
#' @export
unregisterOperation <- function(name) {
  .registry_remove("operations", name, "operation")
}
