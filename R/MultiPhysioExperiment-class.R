#' Multimodal container for PhysioExperiment streams
#'
#' \code{MultiPhysioExperiment} holds several \code{PhysioExperiment} streams
#' recorded together - the classic motion-capture situation of, say, 100 Hz
#' kinematics alongside 1000 Hz force-plate analog and 2000 Hz EMG, or the NWB
#' \code{TimeSeries} / LSL-XDF / C3D POINT+ANALOG pattern. Streams that happen
#' to share a sampling rate use the same class; nothing about it is specific to
#' mixed rates.
#'
#' Each stream keeps its own sampling rate and length. The container adds one
#' thing: a shared \code{clock} that says how the streams line up in time. There
#' is deliberately no second alignment table - the clock is the single source of
#' truth, and \code{\link{alignment}} is a view derived from it.
#'
#' Construction never resamples and never estimates synchronisation. Holding the
#' time correspondence and converting the signals onto a common grid
#' (\code{\link{resampleToCommon}}) are separate steps.
#'
#' @section The clock:
#' \describe{
#'   \item{\code{t0}}{Origin of the shared clock, in seconds. \code{NA} means the
#'     recording has no known absolute origin; that absence is preserved rather
#'     than replaced by a fabricated zero.}
#'   \item{\code{offsets}}{Named numeric, one entry per stream, giving where each
#'     stream starts in seconds relative to \code{t0}. Negative values are
#'     allowed (a stream that started before the origin).}
#'   \item{\code{reference_rate}}{Default grid rate used when the signals are
#'     explicitly converted to a common rate. It never overrides a stream's own
#'     sampling rate.}
#'   \item{\code{offsets_assumed}}{\code{TRUE} when the offsets were not supplied
#'     and simultaneous start was assumed. A known common clock and an assumed
#'     one are different claims, so they are recorded differently.}
#'   \item{\code{schema}}{Integer schema version, for migration.}
#' }
#' Extra elements are preserved untouched, which is where measured timestamps,
#' gap records or drift corrections belong.
#'
#' Sample \code{i} of a stream sits at \code{offset + (i - 1) / samplingRate}
#' seconds \emph{relative to \code{t0}}. \code{\link{streamTimeIndex}} returns
#' exactly that and does not add \code{t0}.
#'
#' @slot streams A \code{SimpleList} of named \code{PhysioExperiment} objects.
#' @slot clock A list describing the shared clock (see above).
#' @seealso \code{\link{MultiPhysioExperiment}} for the constructor,
#'   \code{\link{timeWindow}}, \code{\link{resampleToCommon}},
#'   \code{\link{alignStreams}}
#' @name MultiPhysioExperiment-class
#' @exportClass MultiPhysioExperiment
setClass(
  "MultiPhysioExperiment",
  representation(streams = "SimpleList", clock = "list"),
  prototype = list(
    streams = S4Vectors::SimpleList(),
    clock = list(t0 = 0, reference_rate = NA_real_, offsets = numeric(0),
                 offsets_assumed = TRUE, schema = 2L)
  ),
  validity = function(object) .validate_multiphysio(object)
)

#' Legacy multi-rate container
#'
#' \code{MultiRatePhysioExperiment} is the former name of what is now
#' \code{\link{MultiPhysioExperiment-class}}. It survives as an empty subclass so
#' that objects saved under the old name keep loading, validating and
#' dispatching, and so that code testing \code{is(x, "MultiRatePhysioExperiment")}
#' keeps working. It adds no slots and no second clock model.
#'
#' New code should use \code{\link{MultiPhysioExperiment}}.
#'
#' @name MultiRatePhysioExperiment-class
#' @exportClass MultiRatePhysioExperiment
setClass("MultiRatePhysioExperiment", contains = "MultiPhysioExperiment")

# ---- validity ---------------------------------------------------------------

.validate_multiphysio <- function(object) {
  msgs <- character(0)
  s <- object@streams
  cl <- object@clock
  nm <- names(s)

  if (length(s) > 0) {
    if (is.null(nm) || any(!nzchar(nm))) {
      msgs <- c(msgs, "all streams must be named")
      nm <- character(0)
    } else if (anyDuplicated(nm)) {
      msgs <- c(msgs, sprintf("stream names must be unique; duplicated: %s",
                              paste(unique(nm[duplicated(nm)]), collapse = ", ")))
    }
    ok_type <- vapply(s, methods::is, logical(1), "PhysioExperiment")
    if (!all(ok_type)) {
      msgs <- c(msgs, "all streams must be PhysioExperiment objects")
    } else {
      rates <- vapply(as.list(s), function(e) as.numeric(samplingRate(e)), numeric(1))
      bad <- !is.finite(rates) | rates <= 0
      if (any(bad)) {
        msgs <- c(msgs, sprintf("sampling rates must be finite and positive; bad: %s",
                                paste(nm[bad], collapse = ", ")))
      }
    }
  }

  if (!is.list(cl)) {
    return(c(msgs, "clock must be a list"))
  }
  required <- c("t0", "reference_rate", "offsets")
  missing_keys <- setdiff(required, names(cl))
  if (length(missing_keys)) {
    msgs <- c(msgs, sprintf("clock is missing: %s", paste(missing_keys, collapse = ", ")))
    return(if (length(msgs)) msgs else TRUE)
  }

  t0 <- cl$t0
  if (!is.numeric(t0) || length(t0) != 1L) {
    msgs <- c(msgs, "clock$t0 must be a numeric scalar")
  } else if (!is.na(t0) && !is.finite(t0)) {
    msgs <- c(msgs, "clock$t0 must be finite (or NA when there is no absolute origin)")
  }

  rr <- cl$reference_rate
  if (!is.numeric(rr) || length(rr) != 1L) {
    msgs <- c(msgs, "clock$reference_rate must be a numeric scalar (possibly NA)")
  } else if (!is.na(rr) && (!is.finite(rr) || rr <= 0)) {
    msgs <- c(msgs, "clock$reference_rate must be finite and positive, or NA")
  }

  off <- cl$offsets
  if (!is.numeric(off)) {
    msgs <- c(msgs, "clock$offsets must be numeric")
  } else if (length(s) > 0 && length(nm) > 0) {
    onm <- names(off)
    if (is.null(onm)) {
      msgs <- c(msgs, "clock$offsets must be named")
    } else {
      missing_off <- setdiff(nm, onm)
      unknown_off <- setdiff(onm, nm)
      if (length(missing_off)) {
        msgs <- c(msgs, sprintf("clock$offsets has no entry for: %s",
                                paste(missing_off, collapse = ", ")))
      }
      if (length(unknown_off)) {
        msgs <- c(msgs, sprintf("clock$offsets names unknown streams: %s",
                                paste(unknown_off, collapse = ", ")))
      }
      if (anyDuplicated(onm)) {
        msgs <- c(msgs, "clock$offsets names must be unique")
      }
    }
    if (any(!is.finite(off))) {
      msgs <- c(msgs, "clock$offsets must all be finite")
    }
  } else if (length(s) == 0L && length(off) != 0L) {
    msgs <- c(msgs, "an empty container must have no offsets")
  }

  if (length(msgs)) msgs else TRUE
}

# ---- construction -----------------------------------------------------------

.empty_clock <- function() {
  list(t0 = 0, reference_rate = NA_real_,
       offsets = stats::setNames(numeric(0), character(0)),
       offsets_assumed = TRUE, schema = 2L)
}

# Build the canonical clock, refusing to guess. `offsets` may be NULL (assume a
# simultaneous start and say so) but a partially specified set is an error: the
# spec is explicit that an incomplete clock must not be silently zero-filled.
.build_clock <- function(nm, rates, t0, offsets, reference_rate, assumed = NULL) {
  if (is.null(reference_rate)) {
    reference_rate <- if (length(rates)) max(rates, na.rm = TRUE) else NA_real_
  }
  if (is.null(offsets)) {
    off <- stats::setNames(rep(0, length(nm)), nm)
    assumed <- assumed %||% TRUE
  } else {
    if (is.null(names(offsets)) && length(offsets) == length(nm)) {
      names(offsets) <- nm
    }
    onm <- names(offsets)
    if (is.null(onm)) {
      stop("'offsets' must be named (one entry per stream)", call. = FALSE)
    }
    # Checked before `offsets[nm]` reorders them: indexing by name keeps the
    # first match, so a duplicate would silently disappear along with whichever
    # value the caller meant.
    if (anyDuplicated(onm)) {
      stop(sprintf("'offsets' names a stream more than once: %s",
                   paste(unique(onm[duplicated(onm)]), collapse = ", ")),
           call. = FALSE)
    }
    unknown <- setdiff(onm, nm)
    if (length(unknown)) {
      stop(sprintf("'offsets' names streams that do not exist: %s",
                   paste(unknown, collapse = ", ")), call. = FALSE)
    }
    missing_off <- setdiff(nm, onm)
    if (length(missing_off)) {
      stop(sprintf(paste0("'offsets' is incomplete: no offset for %s. ",
                          "Supply every stream's offset, or omit 'offsets' ",
                          "entirely to declare a simultaneous start."),
                   paste(missing_off, collapse = ", ")), call. = FALSE)
    }
    off <- offsets[nm]
    assumed <- assumed %||% FALSE
  }
  list(t0 = t0, reference_rate = reference_rate, offsets = off,
       offsets_assumed = assumed, schema = 2L)
}

#' Construct a MultiPhysioExperiment
#'
#' Holds several \code{PhysioExperiment} streams on one shared clock. Nothing is
#' resampled and no synchronisation is estimated.
#'
#' Omitting \code{offsets} declares that the streams started together; that
#' assumption is recorded in the clock (\code{offsets_assumed}) rather than being
#' indistinguishable from a measured alignment. A partially specified
#' \code{offsets} is an error rather than being completed with zeros.
#'
#' @param streams A named list of \code{PhysioExperiment} streams (or pass them
#'   as named \code{...} arguments).
#' @param ... Additional named \code{PhysioExperiment} streams.
#' @param t0 Origin of the shared clock in seconds (default 0). Use \code{NA}
#'   when the recording has no known absolute origin.
#' @param offsets Named numeric of per-stream start offsets in seconds relative
#'   to \code{t0}. Negative values are allowed. \code{NULL} (default) assumes a
#'   simultaneous start.
#' @param reference_rate Default grid rate in Hz for
#'   \code{\link{resampleToCommon}}. Defaults to the highest stream rate.
#' @param clock Optional pre-built clock list, used instead of \code{t0} /
#'   \code{offsets} / \code{reference_rate}.
#' @param experiments Legacy alias for \code{streams}.
#' @param alignment Legacy alignment \code{DataFrame} (columns \code{modality},
#'   \code{samplingRate}, \code{offset}). Its offsets are adopted; its declared
#'   rates are checked against the real streams.
#' @return A \code{MultiPhysioExperiment}.
#' @seealso \code{\link{streamRates}}, \code{\link{timeWindow}},
#'   \code{\link{resampleToCommon}}
#' @export
#' @examples
#' kin <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(100 * 3), 100, 3)), samplingRate = 100)
#' emg <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(2000 * 2), 2000, 2)), samplingRate = 2000)
#' mpe <- MultiPhysioExperiment(kinematics = kin, emg = emg)
#' streamRates(mpe)
#'
#' # a measured alignment: EMG started 137 ms after the clock origin
#' MultiPhysioExperiment(streams = list(kinematics = kin, emg = emg),
#'                       offsets = c(kinematics = 0, emg = 0.137))
MultiPhysioExperiment <- function(streams = list(), ..., t0 = 0, offsets = NULL,
                                  reference_rate = NULL, clock = NULL,
                                  experiments = NULL, alignment = NULL) {
  .new_multiphysio("MultiPhysioExperiment", streams, list(...), t0, offsets,
                   reference_rate, clock, experiments, alignment,
                   explicit_t0 = !missing(t0))
}

#' @rdname MultiPhysioExperiment
#' @description \code{MultiRatePhysioExperiment()} is the former name, kept as a
#'   compatibility entry point. It builds the same container and returns the
#'   legacy subclass so that existing \code{is()} tests keep passing.
#' @export
MultiRatePhysioExperiment <- function(streams = list(), ..., t0 = 0,
                                      offsets = NULL, reference_rate = NULL,
                                      clock = NULL, experiments = NULL,
                                      alignment = NULL) {
  .new_multiphysio("MultiRatePhysioExperiment", streams, list(...), t0, offsets,
                   reference_rate, clock, experiments, alignment,
                   explicit_t0 = !missing(t0))
}

.new_multiphysio <- function(Class, streams, dots, t0, offsets, reference_rate,
                             clock, experiments, alignment, explicit_t0) {
  # Old and new spellings may not disagree silently.
  if (!is.null(experiments) && length(experiments)) {
    if (length(streams)) {
      stop("give either 'streams' or the legacy 'experiments', not both",
           call. = FALSE)
    }
    streams <- experiments
  }
  if (!is.null(alignment)) {
    if (!is.null(offsets)) {
      stop("give either 'offsets' or the legacy 'alignment', not both",
           call. = FALSE)
    }
    if (!is.null(clock)) {
      stop("give either 'clock' or the legacy 'alignment', not both", call. = FALSE)
    }
  }

  all_streams <- c(as.list(streams), dots)
  nm <- names(all_streams)
  if (length(all_streams) > 0) {
    if (is.null(nm) || any(!nzchar(nm))) {
      stop("all streams must be named", call. = FALSE)
    }
    if (anyDuplicated(nm)) {
      stop(sprintf("stream names must be unique; duplicated: %s",
                   paste(unique(nm[duplicated(nm)]), collapse = ", ")),
           call. = FALSE)
    }
    if (!all(vapply(all_streams, methods::is, logical(1), "PhysioExperiment"))) {
      stop("all streams must be PhysioExperiment objects", call. = FALSE)
    }
  } else {
    nm <- character(0)
  }
  sl <- do.call(S4Vectors::SimpleList, all_streams)
  rates <- if (length(all_streams)) {
    vapply(all_streams, function(e) as.numeric(samplingRate(e)), numeric(1))
  } else stats::setNames(numeric(0), character(0))

  assumed <- NULL
  if (!is.null(alignment)) {
    adopted <- .offsets_from_alignment(alignment, nm, rates)
    offsets <- adopted$offsets
    assumed <- FALSE
    if (is.null(reference_rate)) reference_rate <- adopted$reference_rate
  }

  if (is.null(clock)) {
    if (length(all_streams) == 0L) {
      clock <- .empty_clock()
      if (explicit_t0) clock$t0 <- t0
    } else {
      clock <- .build_clock(nm, rates, t0, offsets, reference_rate, assumed)
    }
  }
  methods::new(Class, streams = sl, clock = clock)
}

# Adopt a legacy alignment table, checking it against the real signals instead
# of trusting it: a stored alignment can disagree with its own streams.
.offsets_from_alignment <- function(alignment, nm, rates) {
  df <- as.data.frame(alignment, stringsAsFactors = FALSE)
  if (!all(c("modality", "offset") %in% names(df))) {
    stop("'alignment' must have at least 'modality' and 'offset' columns",
         call. = FALSE)
  }
  mods <- as.character(df$modality)
  if (anyDuplicated(mods)) {
    stop(sprintf("'alignment' has more than one row for: %s",
                 paste(unique(mods[duplicated(mods)]), collapse = ", ")),
         call. = FALSE)
  }
  unknown <- setdiff(mods, nm)
  if (length(unknown)) {
    stop(sprintf("'alignment' names streams that do not exist: %s",
                 paste(unknown, collapse = ", ")), call. = FALSE)
  }
  missing_mod <- setdiff(nm, mods)
  if (length(missing_mod)) {
    stop(sprintf("'alignment' has no row for: %s", paste(missing_mod, collapse = ", ")),
         call. = FALSE)
  }
  off <- stats::setNames(as.numeric(df$offset), mods)[nm]
  if (any(!is.finite(off))) {
    stop("'alignment' offsets must all be finite", call. = FALSE)
  }
  ref <- NULL
  if ("samplingRate" %in% names(df)) {
    declared <- stats::setNames(as.numeric(df$samplingRate), mods)[nm]
    bad <- which(is.finite(declared) & abs(declared - rates[nm]) > 1e-9)
    if (length(bad)) {
      stop(sprintf(paste0("'alignment' disagrees with the streams it describes: ",
                          "%s. Fix the table or the data; they cannot both stand."),
                   paste(sprintf("%s declares %g Hz but the stream is %g Hz",
                                 nm[bad], declared[bad], rates[nm][bad]),
                         collapse = "; ")), call. = FALSE)
    }
  }
  list(offsets = off, reference_rate = ref)
}

# ---- accessors --------------------------------------------------------------

#' Access the streams of a MultiPhysioExperiment
#' @param x A \code{MultiPhysioExperiment}.
#' @param value A named list / SimpleList of \code{PhysioExperiment} streams.
#' @return \code{streams()} a \code{SimpleList}; setter returns the updated object.
#' @export
setGeneric("streams", function(x) standardGeneric("streams"))
#' @rdname streams
#' @export
setMethod("streams", "MultiPhysioExperiment", function(x) x@streams)

#' @rdname streams
#' @export
setGeneric("streams<-", function(x, value) standardGeneric("streams<-"))
#' @rdname streams
#' @export
setReplaceMethod("streams", "MultiPhysioExperiment", function(x, value) {
  sl <- if (methods::is(value, "SimpleList")) value
        else do.call(S4Vectors::SimpleList, as.list(value))
  # The clock must follow the streams rather than be left describing the old
  # set; offsets for surviving streams are kept, new streams are assumed to
  # start with the clock and that assumption is recorded.
  old <- x@clock$offsets
  nm <- names(sl)
  off <- stats::setNames(rep(0, length(nm)), nm)
  keep <- intersect(nm, names(old))
  off[keep] <- old[keep]
  x@streams <- sl
  x@clock$offsets <- off
  if (length(setdiff(nm, names(old)))) x@clock$offsets_assumed <- TRUE
  methods::validObject(x)
  x
})

#' Names of the streams
#' @param x A \code{MultiPhysioExperiment}.
#' @return Character vector of stream names.
#' @export
streamNames <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  names(x@streams)
}

#' Per-stream sampling rates
#' @param x A \code{MultiPhysioExperiment}.
#' @return Named numeric of sampling rates (Hz), one per stream.
#' @export
streamRates <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  s <- as.list(x@streams)
  if (length(s) == 0) return(stats::setNames(numeric(0), character(0)))
  vapply(s, function(e) as.numeric(samplingRate(e)), numeric(1))
}

#' The shared clock
#' @param x A \code{MultiPhysioExperiment}.
#' @return The clock list (\code{t0}, \code{reference_rate}, \code{offsets},
#'   \code{offsets_assumed}, \code{schema}).
#' @export
commonClock <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  x@clock
}

#' Number of streams
#' @param x A \code{MultiPhysioExperiment}.
#' @return Integer count of streams.
#' @export
nStreams <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  length(x@streams)
}

# ---- legacy CrossModal accessors -------------------------------------------

#' Legacy accessors for the multimodal container
#'
#' These are the names the container carried when it lived in
#' \pkg{PhysioCrossModal}. They are kept so existing code runs unchanged and
#' return the same types they always did: \code{experiments()} a plain named
#' \code{list} (not a \code{SimpleList}), \code{modalities()} a character
#' vector, \code{samplingRates()} a named numeric, \code{nModalities()} an
#' integer.
#'
#' \code{alignment()} is a \emph{view} computed from the clock, not a stored
#' second copy. Assigning to it writes back into the clock, so the two can never
#' disagree.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param value For \code{experiments<-} a named list of streams; for
#'   \code{alignment<-} a \code{DataFrame} with \code{modality} and
#'   \code{offset} columns.
#' @return As described above; setters return the modified object.
#' @name multiphysio-legacy-accessors
NULL

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("experiments", function(x) standardGeneric("experiments"))
#' @rdname multiphysio-legacy-accessors
#' @export
setMethod("experiments", "MultiPhysioExperiment", function(x) as.list(x@streams))

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("experiments<-", function(x, value) standardGeneric("experiments<-"))
#' @rdname multiphysio-legacy-accessors
#' @export
setReplaceMethod("experiments", "MultiPhysioExperiment", function(x, value) {
  `streams<-`(x, value)
})

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("modalities", function(x) standardGeneric("modalities"))
#' @rdname multiphysio-legacy-accessors
#' @export
setMethod("modalities", "MultiPhysioExperiment", function(x) names(x@streams))

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("samplingRates", function(x) standardGeneric("samplingRates"))
#' @rdname multiphysio-legacy-accessors
#' @export
setMethod("samplingRates", "MultiPhysioExperiment", function(x) streamRates(x))

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("nModalities", function(x) standardGeneric("nModalities"))
#' @rdname multiphysio-legacy-accessors
#' @export
setMethod("nModalities", "MultiPhysioExperiment", function(x) length(x@streams))

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("alignment", function(x) standardGeneric("alignment"))
#' @rdname multiphysio-legacy-accessors
#' @export
setMethod("alignment", "MultiPhysioExperiment", function(x) {
  nm <- names(x@streams)
  if (length(nm) == 0L) return(S4Vectors::DataFrame())
  S4Vectors::DataFrame(
    modality = nm,
    samplingRate = unname(streamRates(x)[nm]),
    offset = unname(x@clock$offsets[nm]))
})

#' @rdname multiphysio-legacy-accessors
#' @export
setGeneric("alignment<-", function(x, value) standardGeneric("alignment<-"))
#' @rdname multiphysio-legacy-accessors
#' @export
setReplaceMethod("alignment", "MultiPhysioExperiment", function(x, value) {
  nm <- names(x@streams)
  adopted <- .offsets_from_alignment(value, nm, streamRates(x))
  x@clock$offsets <- adopted$offsets
  x@clock$offsets_assumed <- FALSE
  methods::validObject(x)
  x
})

# ---- display / size / extraction -------------------------------------------

#' Display, size and stream-extraction methods for MultiPhysioExperiment
#'
#' @param object,x A \code{MultiPhysioExperiment}.
#' @param i Stream name or index (for \code{[[}).
#' @param j,... Ignored.
#' @return \code{length()} the number of streams; \code{dim()} a matrix of
#'   per-stream \code{c(nsamples, nchannels)}; \code{[[} the selected
#'   \code{PhysioExperiment} stream; \code{show()} is called for its side effect.
#' @name MultiPhysioExperiment-methods
#' @rdname MultiPhysioExperiment-methods
NULL

#' @rdname MultiPhysioExperiment-methods
#' @export
setMethod("length", "MultiPhysioExperiment", function(x) length(x@streams))

#' @rdname MultiPhysioExperiment-methods
#' @export
setMethod("names", "MultiPhysioExperiment", function(x) names(x@streams))

#' @rdname MultiPhysioExperiment-methods
#' @export
setMethod("[[", "MultiPhysioExperiment", function(x, i, j, ...) x@streams[[i]])

#' @rdname MultiPhysioExperiment-methods
#' @export
setMethod("dim", "MultiPhysioExperiment", function(x) {
  s <- as.list(x@streams)
  if (length(s) == 0) return(matrix(integer(0), 0, 2,
                                    dimnames = list(NULL, c("nsamples", "nchannels"))))
  d <- t(vapply(s, function(e) {
    a <- SummarizedExperiment::assay(e, defaultAssay(e))
    dm <- dim(a)
    c(dm[1], if (length(dm) >= 2) dm[2] else NA_integer_)
  }, integer(2)))
  colnames(d) <- c("nsamples", "nchannels")
  rownames(d) <- names(s)
  d
})

#' @rdname MultiPhysioExperiment-methods
#' @export
setMethod("show", "MultiPhysioExperiment", function(object) {
  cat("class: ", class(object), "\n", sep = "")
  s <- object@streams
  cat("streams(", length(s), "): ", paste(names(s), collapse = ", "), "\n", sep = "")
  cl <- object@clock
  if (length(cl) > 0) {
    t0 <- cl$t0 %||% 0
    cat("clock: t0=", if (is.na(t0)) "NA (no absolute origin)" else t0,
        " reference_rate=", cl$reference_rate %||% NA, " Hz",
        if (isTRUE(cl$offsets_assumed)) "  [offsets assumed, not measured]" else "",
        "\n", sep = "")
  }
  for (nm in names(s)) {
    e <- s[[nm]]
    a <- SummarizedExperiment::assay(e, defaultAssay(e))
    off <- if (!is.null(cl$offsets) && nm %in% names(cl$offsets)) cl$offsets[[nm]] else 0
    cat("  ", nm, ": ", paste(dim(a), collapse = " x "),
        " @ ", samplingRate(e), " Hz (offset ", off, " s)\n", sep = "")
  }
})

# ---- clock ------------------------------------------------------------------

# ---- measured sample times --------------------------------------------------

# Column names that carry a stream's MEASURED sample times, in seconds relative
# to the shared clock origin, in order of authority. Readers that receive real
# timestamps (LSL/XDF, for instance) record them here; reconstructing times from
# a start offset and a nominal rate would silently invent a uniform grid the
# recording does not have.
.MEASURED_TIME_COLUMNS <- c("time_from_t0", "xdf_time", "time_seconds")

#' Measured sample times of a stream, if it has any
#'
#' @param pe A \code{PhysioExperiment}.
#' @return Numeric vector of measured times in seconds relative to the clock
#'   origin, or \code{NULL} when the stream carries none and its times are
#'   regular.
#' @keywords internal
.measured_times <- function(pe) {
  rd <- SummarizedExperiment::rowData(pe)
  if (is.null(rd) || nrow(rd) == 0L) return(NULL)
  for (nm in .MEASURED_TIME_COLUMNS) {
    if (nm %in% colnames(rd)) {
      tt <- as.numeric(rd[[nm]])
      if (length(tt) == nrow(rd) && any(is.finite(tt))) return(tt)
    }
  }
  NULL
}

#' Does a stream carry measured sample times?
#'
#' Regular streams put sample \code{i} at \code{offset + (i - 1) / rate}. A
#' stream that carries measured timestamps - because the acquisition reported
#' them, or because it has gaps or drift - is not on that grid, and operations
#' that can only work with a regular grid must say so rather than approximate.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param stream Stream name, or \code{NULL} for all streams.
#' @return Logical, named when \code{stream} is \code{NULL}.
#' @seealso \code{\link{streamTimeIndex}}
#' @export
hasMeasuredTimes <- function(x, stream = NULL) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  one <- function(nm) !is.null(.measured_times(x@streams[[nm]]))
  if (is.null(stream)) {
    return(stats::setNames(vapply(names(x@streams), one, logical(1)),
                           names(x@streams)))
  }
  if (!stream %in% names(x@streams)) {
    stop(sprintf("unknown stream '%s'", stream), call. = FALSE)
  }
  one(stream)
}

# Refuse rather than approximate: an operation that assumes a regular grid must
# not silently run on a stream whose real times are irregular.
.require_regular <- function(x, what, streams = names(x@streams)) {
  irregular <- streams[vapply(streams, function(nm)
    !is.null(.measured_times(x@streams[[nm]])), logical(1))]
  if (length(irregular)) {
    stop(sprintf(paste0("%s assumes a regular sampling grid, but %s carr%s ",
                        "measured sample times. Convert onto an explicit grid ",
                        "first, or operate on the streams individually."),
                 what, paste(irregular, collapse = ", "),
                 if (length(irregular) == 1L) "ies" else "y"), call. = FALSE)
  }
  invisible(TRUE)
}

#' Sample times of a stream on the shared clock
#'
#' Returns \code{offset + (i - 1) / samplingRate} for each sample, i.e. seconds
#' \emph{relative to} \code{clock$t0}. It deliberately does not add \code{t0};
#' use \code{commonClock(x)$t0} when an absolute time is needed.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param stream Stream name (or \code{NULL} for a named list of all streams).
#' @return Numeric time vector (seconds from \code{t0}) for the stream, or a
#'   named list of such vectors when \code{stream} is \code{NULL}.
#' @seealso \code{\link{timeWindow}}, \code{\link{resampleToCommon}}
#' @export
streamTimeIndex <- function(x, stream = NULL) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  cl <- x@clock
  one <- function(nm) {
    e <- x@streams[[nm]]
    # Measured times win. Reconstructing from offset + rate would impose a
    # uniform grid on a recording that may have gaps or drift.
    mt <- .measured_times(e)
    if (!is.null(mt)) return(mt)
    a <- SummarizedExperiment::assay(e, defaultAssay(e))
    n <- dim(a)[1]
    off <- if (!is.null(cl$offsets) && nm %in% names(cl$offsets)) cl$offsets[[nm]] else 0
    r <- samplingRate(e)
    off + (seq_len(n) - 1) / r
  }
  if (is.null(stream)) {
    return(stats::setNames(lapply(names(x@streams), one), names(x@streams)))
  }
  if (!stream %in% names(x@streams)) {
    stop(sprintf("unknown stream '%s'", stream), call. = FALSE)
  }
  one(stream)
}

# ---- time windows -----------------------------------------------------------

# Boundary tolerance. Sample times are built as offset + (i-1)/rate, so they
# carry double-rounding error of order eps*|t| (~1e-13 s at t = 1000 s). A fixed
# 1 ns floor plus a relative term absorbs that without ever admitting a sample
# that genuinely lies outside the window: the fastest rates in this ecosystem
# are ~10 kHz, i.e. 100 us between samples, five orders of magnitude above the
# floor.
.window_tol <- function(start, end) 1e-9 + 1e-12 * max(abs(start), abs(end))

# Re-base a stream's events onto a window.
#
# Event onsets are seconds from that stream's own first sample. When a window
# moves the first sample, the onsets have to move with it, and events that fall
# outside the retained span are no longer events of this signal. The full
# original list is kept as history so nothing is lost, clearly separated from
# the events that apply to the signal now.
.window_events <- function(pe, old_start, new_start, span, tol) {
  ev <- tryCatch(getEvents(pe), error = function(e) NULL)
  if (is.null(ev) || nEvents(pe) == 0L) return(pe)
  tab <- as.data.frame(ev@events, stringsAsFactors = FALSE)
  if (!"onset" %in% names(tab) || nrow(tab) == 0L) return(pe)

  abs_on <- as.numeric(tab$onset) + old_start
  dur <- if ("duration" %in% names(tab)) as.numeric(tab$duration) else rep(0, nrow(tab))
  dur[!is.finite(dur)] <- 0
  keep <- which(abs_on + dur >= span[1] - tol & abs_on <= span[2] + tol)
  straddles <- which(abs_on < span[1] - tol & abs_on + dur >= span[1] - tol)

  out <- tab[keep, , drop = FALSE]
  if (nrow(out) > 0L) {
    out$onset <- abs_on[keep] - new_start
    pe <- setEvents(pe, PhysioEvents(
      onset = out$onset,
      duration = if ("duration" %in% names(out)) out$duration else rep(0, nrow(out)),
      type = if ("type" %in% names(out)) as.character(out$type) else rep(NA_character_, nrow(out)),
      value = if ("value" %in% names(out)) out$value else rep(NA, nrow(out))))
  } else {
    pe <- setEvents(pe, PhysioEvents(onset = numeric(0), duration = numeric(0),
                                     type = character(0), value = character(0)))
  }
  md <- S4Vectors::metadata(pe)
  md$events_before_window <- list(
    events = tab,
    onset_origin = "seconds from the stream's first sample BEFORE the window",
    window = span,
    n_kept = length(keep),
    n_straddling_start = length(straddles))
  S4Vectors::metadata(pe) <- md
  pe
}

#' Select a time window across all streams
#'
#' Selects, from every stream, the samples whose time on the shared clock falls
#' in the closed interval \code{[start, end]}, expressed in seconds relative to
#' \code{clock$t0}. Selection uses each sample's actual time, so a stream's
#' offset is honoured rather than ignored.
#'
#' \code{t0} is preserved and each stream's offset is updated to the time of its
#' first retained sample, so the result sits on the same clock as the input.
#' The input object is not modified.
#'
#' A stream that does not overlap the window is an error by default, naming the
#' streams concerned: silently returning a container with fewer streams than
#' asked for has been a source of wrong answers. Pass \code{drop_empty = TRUE}
#' to exclude them deliberately; the exclusion and its reason are then recorded
#' in the result's provenance.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param start,end Window bounds in seconds relative to \code{clock$t0}
#'   (closed interval, \code{start <= end}).
#' @param streams Optional character vector of stream names to keep.
#' @param drop_empty Exclude non-overlapping streams instead of failing.
#' @param complete_only Keep only trials lying wholly inside the window. By
#'   default a trial overlapping the window is kept, truncated to it and marked,
#'   because the trial happened; the excluded ones are recorded either way.
#' @return A \code{MultiPhysioExperiment} carrying the selected samples.
#' @seealso \code{\link{streamTimeIndex}}, \code{\link{resampleToCommon}}
#' @export
#' @examples
#' a <- PhysioExperiment(S4Vectors::SimpleList(raw = matrix(rnorm(200), 100, 2)),
#'                       samplingRate = 100)
#' b <- PhysioExperiment(S4Vectors::SimpleList(raw = matrix(rnorm(500), 250, 2)),
#'                       samplingRate = 250)
#' mpe <- MultiPhysioExperiment(streams = list(a = a, b = b),
#'                              offsets = c(a = 0, b = 0.2))
#' w <- timeWindow(mpe, 0.3, 0.5)
#' vapply(streamTimeIndex(w), function(t) t[1], numeric(1))
timeWindow <- function(x, start, end, streams = NULL, drop_empty = FALSE,
                       complete_only = FALSE) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  if (!is.numeric(start) || length(start) != 1L || !is.finite(start) ||
      !is.numeric(end) || length(end) != 1L || !is.finite(end)) {
    stop("'start' and 'end' must be finite numeric scalars", call. = FALSE)
  }
  if (start > end) {
    stop(sprintf("'start' (%g) must not be greater than 'end' (%g)", start, end),
         call. = FALSE)
  }
  nm <- names(x@streams)
  if (!is.null(streams)) {
    unknown <- setdiff(streams, nm)
    if (length(unknown)) {
      stop(sprintf("unknown stream(s): %s", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }
    nm <- streams
  }
  tol <- .window_tol(start, end)

  kept <- list(); offs <- numeric(0); dropped <- character(0)
  for (s in nm) {
    e <- x@streams[[s]]
    tt <- streamTimeIndex(x, s)
    sel <- which(tt >= start - tol & tt <= end + tol)
    if (length(sel) == 0L) {
      dropped <- c(dropped, s)
      next
    }
    sub <- e[sel, ]
    kept[[s]] <- .window_events(sub, old_start = tt[1], new_start = tt[sel[1]],
                                span = c(start, end), tol = tol)
    offs[s] <- tt[sel[1]]
  }
  if (length(dropped) && !isTRUE(drop_empty)) {
    stop(sprintf(paste0("no samples of %s fall in [%g, %g]. Widen the window, ",
                        "or pass drop_empty = TRUE to exclude the stream on purpose."),
                 paste(dropped, collapse = ", "), start, end), call. = FALSE)
  }
  if (length(kept) == 0L) {
    stop(sprintf("no stream has samples in [%g, %g]", start, end), call. = FALSE)
  }

  out <- .new_multiphysio(class(x)[[1]], kept, list(), x@clock$t0, offs,
                          x@clock$reference_rate, NULL, NULL, NULL,
                          explicit_t0 = TRUE)
  # Selecting a window does not turn an assumed simultaneous start into a
  # measured one: the new offsets are derived from the old ones and inherit
  # exactly their certainty.
  out@clock$offsets_assumed <- x@clock$offsets_assumed %||% TRUE
  # Everything the clock carried beyond the core fields -- migration record,
  # source identifiers, gap tables -- belongs to the recording, not to the
  # window, so it is carried across rather than dropped.
  # The trial tables are excluded from the blind carry-over: an interval table
  # copied unchanged would keep asserting a trial the window just removed.
  extra <- setdiff(names(x@clock), c(names(out@clock), .MAINTAINED_CLOCK_KEYS))
  if (length(extra)) out@clock[extra] <- x@clock[extra]
  out@clock$selected_window <- c(start = start, end = end)
  out <- .window_trial_tables(out, x, start, end, complete_only = complete_only)
  # A window removes the sample that bounded a straddling gap from outside, so
  # the gap can no longer be found in the subset. Record it before that happens.
  out <- .window_stream_gaps(out, x, start, end)
  if (length(dropped)) {
    out@clock$dropped_streams <- stats::setNames(
      rep(sprintf("no samples in [%g, %g]", start, end), length(dropped)), dropped)
  }
  out
}

#' Subset a MultiPhysioExperiment by time and stream
#'
#' \code{x[c(start, end), streams]} is shorthand for
#' \code{\link{timeWindow}(x, start, end, streams)}.
#'
#' This is the former \pkg{PhysioCrossModal} \code{[} method. Its meaning is
#' preserved, with one deliberate correction: the old implementation computed
#' indices as \code{floor(t * rate) + 1} from the rate alone, which ignored each
#' stream's start offset and could admit one sample lying before \code{start}.
#' Selection is now made on each sample's actual time on the shared clock. For
#' windows whose bounds land on sample times - including all zero-offset data
#' with grid-aligned bounds - the selection is unchanged.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param i Numeric vector of length 2: \code{c(start, end)} in seconds.
#' @param j Character vector of stream names.
#' @param ... Ignored.
#' @param drop Ignored; present for signature compatibility.
#' @return A \code{MultiPhysioExperiment}.
#' @export
setMethod("[", c("MultiPhysioExperiment", "ANY", "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    sel <- if (missing(j)) NULL else {
      if (!is.character(j)) {
        stop("j must be a character vector of stream names", call. = FALSE)
      }
      j
    }
    if (missing(i)) {
      if (is.null(sel)) return(x)
      unknown <- setdiff(sel, names(x@streams))
      if (length(unknown)) {
        stop(sprintf("unknown stream(s): %s", paste(unknown, collapse = ", ")),
             call. = FALSE)
      }
      keep <- as.list(x@streams)[sel]
      out <- .new_multiphysio(class(x)[[1]], keep, list(), x@clock$t0,
                              x@clock$offsets[sel], x@clock$reference_rate,
                              NULL, NULL, NULL, explicit_t0 = TRUE)
      out@clock$offsets_assumed <- x@clock$offsets_assumed %||% TRUE
      extra <- setdiff(names(x@clock), c(names(out@clock), .MAINTAINED_CLOCK_KEYS))
      if (length(extra)) out@clock[extra] <- x@clock[extra]
      # intervals naming a dropped stream are no longer intervals of this object
      it <- x@clock$trial_intervals
      if (!is.null(it) && nrow(it)) {
        out@clock$trial_intervals <- it[as.character(it$stream) %in% sel, , drop = FALSE]
      }
      if (!is.null(x@clock$trials)) out@clock$trials <- x@clock$trials
      sg <- x@clock$stream_gaps
      if (!is.null(sg) && nrow(sg)) {
        out@clock$stream_gaps <- sg[as.character(sg$stream) %in% sel, , drop = FALSE]
      }
      return(out)
    }
    if (!is.numeric(i) || length(i) != 2L) {
      stop("i must be a numeric vector of length 2 giving [start, end] in seconds",
           call. = FALSE)
    }
    timeWindow(x, i[1], i[2], streams = sel)
  }
)

# ---- common-grid conversion -------------------------------------------------

#' Resample all streams onto a single common-rate view
#'
#' Interpolates every stream onto a shared time grid at \code{rate}, honouring
#' each stream's start offset on the shared clock, and returns a single
#' \code{PhysioExperiment} whose channels are the union of all streams'
#' channels (prefixed with the stream name). Grid positions before/after a
#' stream's coverage are \code{NA}.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @param rate Target sampling rate in Hz (default: the clock reference rate).
#' @return A single-rate \code{PhysioExperiment} with an \code{"aligned"} assay.
#' @seealso \code{\link{alignStreams}}, \code{\link{streamTimeIndex}}
#' @export
#' @examples
#' kin <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(100 * 2), 100, 2)), samplingRate = 100)
#' emg <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(1000 * 2), 1000, 2)), samplingRate = 1000)
#' mpe <- MultiPhysioExperiment(kin = kin, emg = emg)
#' aligned <- resampleToCommon(mpe, 1000)
#' dim(SummarizedExperiment::assay(aligned, "aligned"))
resampleToCommon <- function(x, rate = NULL) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  s <- x@streams
  if (length(s) == 0) stop("no streams to align", call. = FALSE)
  cl <- x@clock
  if (is.null(rate)) rate <- cl$reference_rate
  if (is.null(rate) || is.na(rate) || rate <= 0) {
    stop("a positive target 'rate' is required", call. = FALSE)
  }

  info <- lapply(names(s), function(nm) {
    e <- s[[nm]]
    a <- SummarizedExperiment::assay(e, defaultAssay(e))
    if (length(dim(a)) != 2L) {
      stop(sprintf("stream '%s' must have a 2D (time x channel) assay", nm),
           call. = FALSE)
    }
    r <- samplingRate(e)
    # Interpolation is driven by each sample's real time, measured when the
    # stream carries measurements and reconstructed otherwise.
    list(data = a, times = streamTimeIndex(x, nm), r = r)
  })
  names(info) <- names(s)

  t_start <- min(vapply(info, function(i) i$times[1], numeric(1)))
  t_end <- max(vapply(info, function(i) i$times[length(i$times)], numeric(1)))
  grid <- seq(t_start, t_end, by = 1 / rate)
  N <- length(grid)

  cols <- list(); coldata <- list()
  for (nm in names(info)) {
    i <- info[[nm]]
    d <- i$data
    ch <- ncol(d)
    lo <- i$times[1]; hi <- i$times[length(i$times)]
    within <- grid >= lo - 1e-9 & grid <= hi + 1e-9
    m <- matrix(NA_real_, N, ch)
    for (cc in seq_len(ch)) {
      m[within, cc] <- stats::approx(i$times, d[, cc], xout = grid[within],
                                     rule = 2)$y
    }
    labs <- colnames(d)
    if (is.null(labs)) labs <- paste0("ch", seq_len(ch))
    colnames(m) <- paste0(nm, ".", labs)
    cols[[nm]] <- m
    coldata[[nm]] <- S4Vectors::DataFrame(stream = nm, label = labs, rate = i$r)
  }
  merged <- do.call(cbind, cols)
  cd <- do.call(rbind, coldata)

  out <- PhysioExperiment(
    assays = S4Vectors::SimpleList(aligned = merged),
    colData = cd,
    metadata = list(common_clock = cl, grid_start = t_start,
                    source_streams = names(s)),
    samplingRate = rate
  )
  appendProvenance(out, activity = "resampleToCommon",
                   params = list(rate = rate, n_streams = length(s)),
                   output_assay = "aligned",
                   software_version = as.character(utils::packageVersion("PhysioExperiment")))
}

#' Align all streams to the reference rate
#'
#' Convenience wrapper for \code{resampleToCommon(x, reference_rate)} using the
#' shared clock's reference rate.
#'
#' @param x A \code{MultiPhysioExperiment}.
#' @return A single-rate \code{PhysioExperiment}.
#' @seealso \code{\link{resampleToCommon}}
#' @export
alignStreams <- function(x) {
  stopifnot(methods::is(x, "MultiPhysioExperiment"))
  resampleToCommon(x, x@clock$reference_rate)
}

# ---- aggregated provenance --------------------------------------------------

#' @rdname provenance
#' @export
setMethod("provenance", "MultiPhysioExperiment", function(x) {
  s <- x@streams
  if (length(s) == 0) {
    return(cbind(stream = character(0),
                 provenance(PhysioExperiment(S4Vectors::SimpleList()))))
  }
  parts <- lapply(names(s), function(nm) {
    p <- provenance(s[[nm]])
    if (nrow(p) == 0) return(NULL)
    cbind(stream = rep(nm, nrow(p)), p, stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    return(cbind(stream = character(0),
                 provenance(PhysioExperiment(S4Vectors::SimpleList()))))
  }
  do.call(rbind, parts)
})
