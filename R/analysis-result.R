#' AnalysisResult: a uniform container for ecosystem analysis outputs
#'
#' A lightweight S4 container so every analysis (HRV, synergies, gait indices,
#' ...) returns a consistently-shaped object that reporting and downstream code
#' can consume uniformly.
#'
#' @slot type Character tag identifying the analysis (e.g. \code{"hrv_time"}).
#' @slot result Named list holding the payload.
#' @slot parameters Named list of parameters used to produce the result.
#' @slot provenance Optional \code{data.frame} lineage (see \code{\link{provenance}}).
#' @slot estimate The point estimate the result carries (any type).
#' @slot uncertainty Named list describing the interval, with a \code{type} in
#'   \code{none}, \code{conformal}, \code{bayes}, \code{bootstrap}, \code{ncp} or \code{analytic},
#'   plus (typically) \code{level}, \code{lower} and \code{upper}.
#' @slot method Character label of the estimation method.
#' @slot estimand Named list of ICH E9(R1) estimand attributes (population,
#'   treatment, endpoint / variable, summary measure, intercurrent-event
#'   handling).
#' @seealso \code{\link{PhysioBiomarker}}
#' @exportClass AnalysisResult
setClass(
  "AnalysisResult",
  representation(
    type = "character",
    result = "list",
    parameters = "list",
    provenance = "data.frame",
    estimate = "ANY",
    uncertainty = "list",
    method = "character",
    estimand = "list"
  ),
  prototype(
    type = NA_character_, result = list(),
    parameters = list(), provenance = data.frame(),
    estimate = NULL, uncertainty = list(),
    method = NA_character_, estimand = list()
  ),
  validity = function(object) {
    u <- object@uncertainty
    if (length(u)) {
      allowed <- c("none", "conformal", "bayes", "bootstrap", "ncp", "analytic")
      if (is.null(u$type) || length(u$type) != 1L || is.na(u$type) ||
          !u$type %in% allowed) {
        return(paste0("uncertainty$type must be one of: ",
                      paste(allowed, collapse = ", ")))
      }
    }
    e <- object@estimand
    if (length(e) && (is.null(names(e)) || any(!nzchar(names(e))))) {
      return("estimand must be a fully named list of E9(R1) attributes")
    }
    TRUE
  }
)

#' Construct an AnalysisResult
#'
#' @param type Character analysis tag.
#' @param result Named list payload.
#' @param parameters Named list of parameters.
#' @param provenance Optional lineage \code{data.frame}.
#' @param estimate Optional point estimate carried by the result.
#' @param uncertainty Optional named list describing the interval (with a
#'   \code{type} of none/conformal/bayes/bootstrap/ncp/analytic).
#' @param method Optional character estimation-method label.
#' @param estimand Optional named list of ICH E9(R1) estimand attributes.
#' @return An \code{AnalysisResult} object.
#' @examples
#' AnalysisResult("hrv_time", result = list(sdnn = 42, rmssd = 30))
#' AnalysisResult("rom", estimate = 118,
#'   uncertainty = list(type = "conformal", level = 0.9, lower = 104,
#'                      upper = 132),
#'   estimand = list(population = "post-op knee", summary_measure = "median"))
#' @export
AnalysisResult <- function(type, result = list(), parameters = list(),
                           provenance = data.frame(), estimate = NULL,
                           uncertainty = list(), method = NA_character_,
                           estimand = list()) {
  new("AnalysisResult",
      type = as.character(type), result = as.list(result),
      parameters = as.list(parameters), provenance = provenance,
      estimate = estimate, uncertainty = as.list(uncertainty),
      method = as.character(method), estimand = as.list(estimand))
}

#' PhysioBiomarker: a single computed biomarker with metadata
#'
#' Extends \code{\link{AnalysisResult}} with a scalar value, unit, optional
#' confidence interval, and interpretation - the unit reporting consumes.
#'
#' @slot name Character biomarker name.
#' @slot value Numeric scalar value.
#' @slot unit Character measurement unit.
#' @slot ci Numeric length-2 confidence interval, or length-0 if none.
#' @slot interpretation Optional character interpretation label.
#' @slot reference_range Numeric length-2 published reference (normal) range, or
#'   length-0 if none.
#' @slot reliability Named list of reliability indices, typically \code{icc},
#'   \code{sem}, and \code{mdc} (see \code{\link{icc}}, \code{\link{sem}},
#'   \code{\link{mdc}}).
#' @slot provenance_info Named list describing how the value was computed,
#'   typically \code{assay}, \code{band}, \code{method}, and
#'   \code{software_version}.
#' @exportClass PhysioBiomarker
setClass(
  "PhysioBiomarker",
  contains = "AnalysisResult",
  representation(
    name = "character", value = "numeric",
    unit = "character", ci = "numeric", interpretation = "character",
    reference_range = "numeric", reliability = "list",
    provenance_info = "list"
  ),
  prototype(
    name = NA_character_, value = NA_real_, unit = NA_character_,
    ci = numeric(0), interpretation = NA_character_,
    reference_range = numeric(0), reliability = list(),
    provenance_info = list()
  ),
  validity = function(object) {
    if (length(object@value) != 1L) return("'value' must be a scalar")
    if (!length(object@ci) %in% c(0L, 2L)) return("'ci' must have length 0 or 2")
    if (!length(object@reference_range) %in% c(0L, 2L)) {
      return("'reference_range' must have length 0 or 2")
    }
    TRUE
  }
)

#' Construct a PhysioBiomarker
#'
#' @param name Character biomarker name.
#' @param value Numeric scalar value.
#' @param unit Character unit (default \code{NA}).
#' @param ci Numeric length-2 confidence interval (default none).
#' @param interpretation Optional character interpretation.
#' @param parameters Named list of parameters used.
#' @return A \code{PhysioBiomarker} object.
#' @examples
#' PhysioBiomarker("SDNN", 42, unit = "ms", ci = c(38, 46))
#' @export
PhysioBiomarker <- function(name, value, unit = NA_character_, ci = numeric(0),
                            interpretation = NA_character_, parameters = list()) {
  new("PhysioBiomarker",
      type = "biomarker", result = list(value = value),
      parameters = as.list(parameters), provenance = data.frame(),
      name = as.character(name), value = as.numeric(value),
      unit = as.character(unit), ci = as.numeric(ci),
      interpretation = as.character(interpretation))
}

#' Accessors for analysis results
#'
#' @param x An \code{AnalysisResult} or \code{PhysioBiomarker}.
#' @return \code{resultType()} the type tag; \code{resultValue()} the payload
#'   (or scalar value for a biomarker); \code{biomarkerValue()} the scalar value.
#' @name AnalysisResult-accessors
#' @examples
#' res <- AnalysisResult("hrv_time", result = list(sdnn = 42, rmssd = 30))
#' resultType(res)
#' resultValue(res)
#'
#' bm <- PhysioBiomarker("SDNN", 42, unit = "ms")
#' biomarkerValue(bm)
NULL

#' @rdname AnalysisResult-accessors
#' @export
setGeneric("resultType", function(x) standardGeneric("resultType"))

#' @rdname AnalysisResult-accessors
#' @export
setMethod("resultType", "AnalysisResult", function(x) x@type)

#' @rdname AnalysisResult-accessors
#' @export
setGeneric("resultValue", function(x) standardGeneric("resultValue"))

#' @rdname AnalysisResult-accessors
#' @export
setMethod("resultValue", "AnalysisResult", function(x) x@result)

#' @rdname AnalysisResult-accessors
#' @export
setMethod("resultValue", "PhysioBiomarker", function(x) x@value)

#' @rdname AnalysisResult-accessors
#' @export
setGeneric("biomarkerValue", function(x) standardGeneric("biomarkerValue"))

#' @rdname AnalysisResult-accessors
#' @export
setMethod("biomarkerValue", "PhysioBiomarker", function(x) x@value)

#' Estimand / uncertainty carrier accessors
#'
#' @param x An \code{AnalysisResult}.
#' @return \code{estimateOf()} the point estimate; \code{uncertaintyOf()} the
#'   interval list; \code{provenanceOf()} the provenance; \code{estimandOf()} the
#'   ICH E9(R1) estimand list.
#' @name AnalysisResult-carrier
#' @examples
#' r <- AnalysisResult("rom", estimate = 118,
#'   uncertainty = list(type = "conformal", level = 0.9, lower = 104,
#'                      upper = 132))
#' estimateOf(r)
#' uncertaintyOf(r)
NULL

# Read a slot tolerantly: a pre-extension object deserialized under the new
# class definition lacks the carrier slots, so fall back to the default rather
# than erroring (no updateObject() call required to read it).
.arSlot <- function(object, name, default) {
  tryCatch(methods::slot(object, name), error = function(e) default)
}

#' @rdname AnalysisResult-carrier
#' @export
setGeneric("estimateOf", function(x) standardGeneric("estimateOf"))
#' @rdname AnalysisResult-carrier
#' @export
setMethod("estimateOf", "AnalysisResult", function(x) .arSlot(x, "estimate", NULL))

#' @rdname AnalysisResult-carrier
#' @export
setGeneric("uncertaintyOf", function(x) standardGeneric("uncertaintyOf"))
#' @rdname AnalysisResult-carrier
#' @export
setMethod("uncertaintyOf", "AnalysisResult",
          function(x) .arSlot(x, "uncertainty", list()))

#' @rdname AnalysisResult-carrier
#' @export
setGeneric("provenanceOf", function(x) standardGeneric("provenanceOf"))
#' @rdname AnalysisResult-carrier
#' @export
setMethod("provenanceOf", "AnalysisResult",
          function(x) .arSlot(x, "provenance", data.frame()))

#' @rdname AnalysisResult-carrier
#' @export
setGeneric("estimandOf", function(x) standardGeneric("estimandOf"))
#' @rdname AnalysisResult-carrier
#' @export
setMethod("estimandOf", "AnalysisResult",
          function(x) .arSlot(x, "estimand", list()))

#' @param object An object to display.
#' @return The \code{show} methods return \code{NULL} invisibly and are called
#'   for the side effect of printing a compact summary.
#' @rdname AnalysisResult-class
setMethod("show", "AnalysisResult", function(object) {
  cat("<AnalysisResult>", object@type, "\n")
  estimate <- .arSlot(object, "estimate", NULL)
  if (!is.null(estimate)) {
    cat("  estimate:", paste(format(estimate), collapse = ", "), "\n")
  }
  method <- .arSlot(object, "method", NA_character_)
  if (length(method) == 1L && !is.na(method)) {
    cat("  method:", method, "\n")
  }
  u <- .arSlot(object, "uncertainty", list())
  if (length(u) && !is.null(u$type)) {
    lvl <- if (is.numeric(u$level) && length(u$level) == 1L) {
      sprintf(" %.0f%%", 100 * u$level)
    } else {
      ""
    }
    band <- if (!is.null(u$lower) && !is.null(u$upper)) {
      sprintf(" [%s, %s]", paste(format(u$lower), collapse = ","),
              paste(format(u$upper), collapse = ","))
    } else {
      ""
    }
    cat(sprintf("  uncertainty: %s%s%s\n", u$type[1], lvl, band))
  }
  estimand <- .arSlot(object, "estimand", list())
  if (length(estimand)) {
    cat("  estimand:", paste(names(estimand), collapse = ", "), "\n")
  }
  if (length(object@result)) {
    cat("  fields:", paste(names(object@result), collapse = ", "), "\n")
  }
  if (nrow(object@provenance)) {
    cat("  provenance:", nrow(object@provenance), "entr(ies)\n")
  }
})

#' @param object An object to display.
#' @return The \code{show} method for \code{PhysioBiomarker} returns \code{NULL}
#'   invisibly and is called for the side effect of printing a compact summary.
#' @rdname PhysioBiomarker-class
setMethod("show", "PhysioBiomarker", function(object) {
  ci_txt <- if (length(object@ci) == 2L) {
    sprintf(" [%.3g, %.3g]", object@ci[1], object@ci[2])
  } else ""
  unit_txt <- if (is.na(object@unit)) "" else paste0(" ", object@unit)
  cat(sprintf("<PhysioBiomarker> %s = %.4g%s%s\n",
              object@name, object@value, unit_txt, ci_txt))
  # Reliability indices, when characterised.
  rel <- object@reliability
  rel_bits <- Filter(Negate(is.null),
                     list(icc = rel$icc, sem = rel$sem, mdc = rel$mdc))
  if (length(rel_bits)) {
    cat("  reliability:",
        paste(sprintf("%s=%.3g", toupper(names(rel_bits)),
                      unlist(rel_bits)), collapse = ", "), "\n")
  }
  # Normative percentile derived from the reference range (treated as a 95%
  # reference interval) so a single marker prints its standing.
  if (length(object@reference_range) == 2L && is.finite(object@value)) {
    pct <- .biomarker_reference_percentile(object@value, object@reference_range)
    cat(sprintf("  normative: %.0f%s percentile (ref %.3g-%.3g)\n",
                pct, .ordinal_suffix(pct),
                object@reference_range[1], object@reference_range[2]))
  }
  if (!is.na(object@interpretation)) {
    cat("  interpretation:", object@interpretation, "\n")
  }
})
