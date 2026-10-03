#' NormativeReference: stratified normative values for a metric
#'
#' Holds published reference (normative) values for a single metric, stratified
#' by one or more grouping variables (e.g. age band, sex). Downstream code turns
#' an observed value into a z-score or a percent-of-predicted against the
#' matching stratum, which is the basis for most rehabilitation cut-offs
#' (6MWT, gait speed, grip strength, spirometry, ...).
#'
#' @slot metric Character name of the metric (e.g. \code{"gait_speed"}).
#' @slot strata \code{data.frame} with one row per stratum. Must contain numeric
#'   \code{mean} and \code{sd} columns; may contain \code{n}; any remaining
#'   columns are the stratification keys used for matching.
#' @slot source Character citation for the reference values.
#' @slot version Character version tag for the reference set.
#' @slot unit Character measurement unit.
#' @seealso \code{\link{zScore}}, \code{\link{percentPredicted}}
#' @exportClass NormativeReference
setClass(
  "NormativeReference",
  representation(
    metric = "character",
    strata = "data.frame",
    source = "character",
    version = "character",
    unit = "character"
  ),
  prototype(
    metric = NA_character_, strata = data.frame(),
    source = NA_character_, version = NA_character_, unit = NA_character_
  ),
  validity = function(object) {
    cols <- names(object@strata)
    if (!all(c("mean", "sd") %in% cols)) {
      return("'strata' must contain 'mean' and 'sd' columns")
    }
    if (!is.numeric(object@strata$mean) || !is.numeric(object@strata$sd)) {
      return("'mean' and 'sd' columns must be numeric")
    }
    TRUE
  }
)

#' Construct a NormativeReference
#'
#' @param metric Character metric name.
#' @param strata \code{data.frame} with one row per stratum; must include numeric
#'   \code{mean} and \code{sd} columns (optionally \code{n}); remaining columns
#'   are matching keys.
#' @param source Character citation (default \code{NA}).
#' @param version Character version tag (default \code{NA}).
#' @param unit Character unit (default \code{NA}).
#' @return A \code{NormativeReference} object.
#' @examples
#' ref <- NormativeReference(
#'   "gait_speed",
#'   strata = data.frame(
#'     sex = c("M", "F"), mean = c(1.34, 1.24), sd = c(0.20, 0.19), n = c(50, 50)
#'   ),
#'   source = "Bohannon 2011", unit = "m/s"
#' )
#' zScore(ref, 1.10, by = list(sex = "M"))
#' @export
NormativeReference <- function(metric, strata, source = NA_character_,
                               version = NA_character_, unit = NA_character_) {
  new("NormativeReference",
      metric = as.character(metric), strata = as.data.frame(strata),
      source = as.character(source), version = as.character(version),
      unit = as.character(unit))
}

## Return the single stratum row matching `by` (a named list/vector of keys).
## `by = NULL` is allowed only when there is exactly one stratum.
.matchStratum <- function(ref, by) {
  st <- ref@strata
  if (is.null(by) || length(by) == 0L) {
    if (nrow(st) != 1L) {
      stop("'by' is required when the reference has more than one stratum")
    }
    return(st[1L, , drop = FALSE])
  }
  keys <- names(by)
  missing_keys <- setdiff(keys, names(st))
  if (length(missing_keys)) {
    stop("unknown stratum key(s): ", paste(missing_keys, collapse = ", "))
  }
  keep <- rep(TRUE, nrow(st))
  for (k in keys) {
    keep <- keep & as.character(st[[k]]) == as.character(by[[k]])
  }
  hit <- st[keep, , drop = FALSE]
  if (nrow(hit) == 0L) stop("no stratum matches the requested keys")
  if (nrow(hit) > 1L) stop("stratum keys matched more than one row; be more specific")
  hit
}

#' Normative comparisons for an observed value
#'
#' @param ref A \code{NormativeReference}.
#' @param value Numeric observed value(s).
#' @param by Named list or vector selecting the stratum (e.g.
#'   \code{list(sex = "M", age = "60-69")}); \code{NULL} when the reference has a
#'   single stratum.
#' @return \code{zScore()} the standard score \eqn{(value - mean) / sd};
#'   \code{percentPredicted()} \eqn{100 \times value / mean}.
#' @name NormativeReference-compare
#' @examples
#' ref <- NormativeReference(
#'   "gait_speed",
#'   strata = data.frame(
#'     sex = c("M", "F"), mean = c(1.34, 1.24), sd = c(0.20, 0.19)
#'   ),
#'   source = "Bohannon 2011", unit = "m/s"
#' )
#' zScore(ref, 1.10, by = list(sex = "M"))
#' percentPredicted(ref, 1.10, by = list(sex = "M"))
NULL

#' @rdname NormativeReference-compare
#' @export
setGeneric("zScore", function(ref, value, by = NULL) standardGeneric("zScore"))

#' @rdname NormativeReference-compare
#' @export
setMethod("zScore", "NormativeReference", function(ref, value, by = NULL) {
  row <- .matchStratum(ref, by)
  (value - row$mean) / row$sd
})

#' @rdname NormativeReference-compare
#' @export
setGeneric("percentPredicted",
           function(ref, value, by = NULL) standardGeneric("percentPredicted"))

#' @rdname NormativeReference-compare
#' @export
setMethod("percentPredicted", "NormativeReference", function(ref, value, by = NULL) {
  row <- .matchStratum(ref, by)
  100 * value / row$mean
})

#' @param object A \code{NormativeReference} to display.
#' @return The \code{show} method returns \code{NULL} invisibly and is called for
#'   the side effect of printing a compact summary.
#' @rdname NormativeReference-class
setMethod("show", "NormativeReference", function(object) {
  unit_txt <- if (is.na(object@unit)) "" else paste0(" (", object@unit, ")")
  cat(sprintf("<NormativeReference> %s%s\n", object@metric, unit_txt))
  cat("  strata:", nrow(object@strata), "\n")
  if (!is.na(object@source)) cat("  source:", object@source, "\n")
})
