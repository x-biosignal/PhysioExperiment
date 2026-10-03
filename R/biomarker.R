# Reliability-characterised clinical-marker output and a normative scaffold.
#
# physioBiomarker() enriches the PhysioBiomarker S4 class (analysis-result.R)
# with a reference range, reliability indices (ICC/SEM/MDC, per COSMIN), and
# measurement provenance, so a single reported marker carries everything a
# clinical reader needs to judge it. normativeLookup() turns an observed marker
# into a z-score / percentile against seeded published reference values.

## Percentile of a value against a reference range treated as a 95% reference
## interval: mean at the midpoint, sd = half-width / 1.96.
.biomarker_reference_percentile <- function(value, ref) {
  if (length(ref) != 2L) return(NA_real_)
  mu <- mean(ref)
  sdv <- abs(diff(ref)) / (2 * 1.959964)
  if (!is.finite(sdv) || sdv <= 0) return(NA_real_)
  100 * stats::pnorm((value - mu) / sdv)
}

## English ordinal suffix for a (rounded) number: 1 -> "st", 2 -> "nd", ...
.ordinal_suffix <- function(n) {
  n <- round(n)
  if (is.na(n)) return("th")
  if ((n %% 100L) %in% 11:13) return("th")
  switch(as.character(n %% 10L), "1" = "st", "2" = "nd", "3" = "rd", "th")
}

#' Construct a reliability-characterised biomarker
#'
#' Builds a [PhysioBiomarker] carrying not just the value but the metadata a
#' clinical reader needs: a confidence interval, a published reference range,
#' reliability indices (ICC / SEM / MDC, per COSMIN reporting), and the
#' measurement provenance (assay, band, method, software version). The value
#' prints with its CI and, when a reference range is supplied, its normative
#' percentile; [as.data.frame()] flattens every field for tabular reporting.
#'
#' @param value Numeric scalar value.
#' @param name Character biomarker name (e.g. \code{"DAR"}).
#' @param unit Character measurement unit (default \code{NA}).
#' @param ci Numeric length-2 confidence interval, or \code{NULL} for none.
#' @param reference_range Numeric length-2 published reference (normal) range,
#'   or \code{NULL} for none. Interpreted as a 95 percent reference interval when
#'   computing the printed normative percentile.
#' @param reliability Named list of reliability indices, typically \code{icc},
#'   \code{sem}, and \code{mdc} (see [icc()], [sem()], [mdc()]).
#' @param provenance Named list describing how the value was computed, typically
#'   \code{assay}, \code{band}, \code{method}, and \code{software_version}.
#' @param interpretation Optional character interpretation label.
#' @return A [PhysioBiomarker] object.
#' @seealso [PhysioBiomarker()], [normativeLookup()], [is.PhysioBiomarker()],
#'   [icc()], [sem()], [mdc()]
#' @examples
#' bm <- physioBiomarker(
#'   value = 2.35, name = "DAR", unit = "ratio",
#'   ci = c(1.90, 2.80), reference_range = c(0.5, 1.2),
#'   reliability = list(icc = 0.82, sem = 0.15, mdc = 0.42),
#'   provenance = list(assay = "psd", band = "delta/alpha",
#'                     method = "welch", software_version = "1.0.0")
#' )
#' bm
#' as.data.frame(bm)
#' @rdname physioBiomarker-constructor
#' @export
physioBiomarker <- function(value, name, unit = NA_character_, ci = NULL,
                            reference_range = NULL, reliability = list(),
                            provenance = list(),
                            interpretation = NA_character_) {
  ci <- if (is.null(ci)) numeric(0) else as.numeric(ci)
  reference_range <- if (is.null(reference_range)) numeric(0) else
    as.numeric(reference_range)
  methods::new("PhysioBiomarker",
      type = "biomarker", result = list(value = value),
      parameters = list(), provenance = data.frame(),
      name = as.character(name), value = as.numeric(value),
      unit = as.character(unit), ci = ci,
      interpretation = as.character(interpretation),
      reference_range = reference_range,
      reliability = as.list(reliability),
      provenance_info = as.list(provenance))
}

#' Test whether an object is a PhysioBiomarker
#'
#' @param x Any object.
#' @return \code{TRUE} if \code{x} is a [PhysioBiomarker], otherwise \code{FALSE}.
#' @seealso [physioBiomarker()]
#' @examples
#' is.PhysioBiomarker(physioBiomarker(1, "x"))
#' is.PhysioBiomarker(42)
#' @export
is.PhysioBiomarker <- function(x) methods::is(x, "PhysioBiomarker")

#' Format a PhysioBiomarker as a compact string
#'
#' @param x A [PhysioBiomarker].
#' @param ... Ignored.
#' @return A length-1 character string \code{"name = value unit [ci]"}.
#' @method format PhysioBiomarker
#' @export
format.PhysioBiomarker <- function(x, ...) {
  ci_txt <- if (length(x@ci) == 2L)
    sprintf(" [%.3g, %.3g]", x@ci[1], x@ci[2]) else ""
  unit_txt <- if (is.na(x@unit)) "" else paste0(" ", x@unit)
  sprintf("%s = %.4g%s%s", x@name, x@value, unit_txt, ci_txt)
}

#' Coerce a PhysioBiomarker to a one-row data.frame
#'
#' Flattens every field - value, unit, CI, reference range, reliability
#' (ICC/SEM/MDC), and provenance (assay/band/method/software version) - into a
#' single row so biomarkers tabulate and round-trip losslessly.
#'
#' @param x A [PhysioBiomarker].
#' @param row.names Optional row names passed to [base::data.frame()].
#' @param optional Ignored; present for S3 generic compatibility.
#' @param ... Ignored.
#' @return A one-row \code{data.frame} with the flattened fields.
#' @method as.data.frame PhysioBiomarker
#' @export
as.data.frame.PhysioBiomarker <- function(x, row.names = NULL, optional = FALSE,
                                          ...) {
  g <- function(l, k) if (!is.null(l[[k]])) l[[k]] else NA
  rel <- x@reliability
  prov <- x@provenance_info
  data.frame(
    name = x@name,
    value = x@value,
    unit = x@unit,
    ci_lower = if (length(x@ci) == 2L) x@ci[1] else NA_real_,
    ci_upper = if (length(x@ci) == 2L) x@ci[2] else NA_real_,
    ref_lower = if (length(x@reference_range) == 2L) x@reference_range[1] else NA_real_,
    ref_upper = if (length(x@reference_range) == 2L) x@reference_range[2] else NA_real_,
    icc = as.numeric(g(rel, "icc")),
    sem = as.numeric(g(rel, "sem")),
    mdc = as.numeric(g(rel, "mdc")),
    prov_assay = as.character(g(prov, "assay")),
    prov_band = as.character(g(prov, "band")),
    prov_method = as.character(g(prov, "method")),
    prov_software_version = as.character(g(prov, "software_version")),
    interpretation = x@interpretation,
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

# Package-level cache for the shipped normative tables.
.normative_cache <- new.env(parent = emptyenv())

## Read and combine every CSV under inst/extdata/normative (cached). Returns a
## zero-row template when the directory is absent so lookups degrade to NA.
.load_normative <- function() {
  if (!is.null(.normative_cache$tab)) return(.normative_cache$tab)
  template <- data.frame(
    marker = character(0), age_min = numeric(0), age_max = numeric(0),
    montage = character(0), mean = numeric(0), sd = numeric(0),
    unit = character(0), source = character(0), stringsAsFactors = FALSE)
  dir <- system.file("extdata", "normative", package = "PhysioExperiment")
  tab <- template
  if (nzchar(dir) && dir.exists(dir)) {
    files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    parts <- lapply(files, function(f)
      tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
               error = function(e) NULL))
    parts <- Filter(Negate(is.null), parts)
    if (length(parts)) tab <- do.call(rbind, parts)
  }
  .normative_cache$tab <- tab
  tab
}

#' Look up normative values for a biomarker
#'
#' Compares an observed marker to seeded published normative reference values
#' (read from \code{inst/extdata/normative/*.csv}), returning a z-score and
#' percentile. This is a scaffold seeded with a few quantitative-EEG markers
#' (delta-alpha ratio, brain symmetry index); extend the CSVs to add markers.
#' When the marker, age band, or montage is unknown the z-score and percentile
#' are returned as \code{NA} without raising an error.
#'
#' @param marker A [PhysioBiomarker] (its name and value are used), or a
#'   character marker name (then only reference values are returned, with the
#'   z-score/percentile \code{NA}).
#' @param age Optional numeric age in years used to select an age-stratified
#'   reference row.
#' @param montage Optional character montage used to select a montage-specific
#'   reference row.
#' @return A named list: \code{marker}, \code{value}, \code{mean}, \code{sd},
#'   \code{unit}, \code{source}, \code{z_score}, \code{percentile}, and
#'   \code{matched} (logical). Unknown lookups return \code{NA} fields with
#'   \code{matched = FALSE}.
#' @seealso [physioBiomarker()], [NormativeReference()], [zScore()]
#' @examples
#' bm <- physioBiomarker(2.35, "DAR")
#' normativeLookup(bm, age = 40)
#' normativeLookup("UNKNOWN_MARKER")   # returns NA fields, no error
#' @export
normativeLookup <- function(marker, age = NULL, montage = NULL) {
  if (is.PhysioBiomarker(marker)) {
    name <- marker@name
    value <- marker@value
  } else {
    name <- as.character(marker)[1]
    value <- NA_real_
  }
  na_result <- list(marker = name, value = value, mean = NA_real_,
                    sd = NA_real_, unit = NA_character_, source = NA_character_,
                    z_score = NA_real_, percentile = NA_real_, matched = FALSE)

  tab <- .load_normative()
  if (!nrow(tab)) return(na_result)

  keep <- tolower(tab$marker) == tolower(name)
  if (!is.null(montage)) {
    keep <- keep & (tolower(tab$montage) == tolower(as.character(montage)) |
                      tolower(tab$montage) == "any")
  }
  if (!is.null(age)) {
    keep <- keep & (age >= tab$age_min & age <= tab$age_max)
  }
  hit <- tab[keep, , drop = FALSE]
  if (nrow(hit) != 1L) return(na_result)   # unknown or ambiguous -> graceful NA

  z <- if (is.finite(value) && is.finite(hit$sd) && hit$sd > 0)
    (value - hit$mean) / hit$sd else NA_real_
  pct <- if (is.finite(z)) 100 * stats::pnorm(z) else NA_real_
  list(marker = name, value = value, mean = hit$mean, sd = hit$sd,
       unit = hit$unit, source = hit$source, z_score = z,
       percentile = pct, matched = TRUE)
}
