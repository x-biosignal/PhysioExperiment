# Per-assay sampling-rate tags on a single PhysioExperiment.
#
# These are the lightweight representation for the common case where several
# assays of one object were produced at different rates (e.g. a raw assay and a
# decimated one). When streams genuinely differ in length/duration, the
# canonical representation is the MultiRatePhysioExperiment container
# (see [MultiRatePhysioExperiment]); assaySamplingRates() bridges the two by
# recording per-assay rates in metadata without changing the object's shape.

#' Per-assay sampling rates
#'
#' Returns the sampling rate associated with each assay of a
#' \code{PhysioExperiment}. If no per-assay rates have been recorded, all assays
#' are assumed to share the object's main \code{\link{samplingRate}}.
#'
#' For genuinely multi-rate acquisitions (streams of different length) prefer the
#' \code{\link{MultiRatePhysioExperiment}} container; \code{assaySamplingRates()}
#' is the lightweight per-assay tag for single-object, equal-length assays.
#'
#' @param x A \code{PhysioExperiment} object.
#' @return A named numeric vector of sampling rates (Hz), one per assay.
#' @references Crochiere, R. E. & Rabiner, L. R. (1983). Multirate Digital
#'   Signal Processing. Prentice Hall.
#' @seealso \code{\link{setAssaySamplingRate}},
#'   \code{\link{MultiRatePhysioExperiment}}
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)), samplingRate = 100)
#' assaySamplingRates(pe)
assaySamplingRates <- function(x) {
  stopifnot(methods::is(x, "PhysioExperiment"))
  rates <- S4Vectors::metadata(x)$assay_sampling_rates
  if (is.null(rates)) {
    assay_names <- SummarizedExperiment::assayNames(x)
    rates <- rep(samplingRate(x), length(assay_names))
    names(rates) <- assay_names
  }
  rates
}

#' Set the sampling rate of a specific assay
#'
#' Records a per-assay sampling rate in the object's metadata; useful after an
#' assay has been resampled to a rate different from the main one.
#'
#' @param x A \code{PhysioExperiment} object.
#' @param assay_name Name of the assay.
#' @param rate Sampling rate for the assay in Hz.
#' @return The \code{PhysioExperiment} with updated per-assay rate metadata.
#' @seealso \code{\link{assaySamplingRates}},
#'   \code{\link{MultiRatePhysioExperiment}}
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2),
#'                         decimated = matrix(rnorm(20), 10, 2)),
#'   samplingRate = 100)
#' pe <- setAssaySamplingRate(pe, "decimated", 50)
#' assaySamplingRates(pe)
setAssaySamplingRate <- function(x, assay_name, rate) {
  stopifnot(methods::is(x, "PhysioExperiment"))
  if (!assay_name %in% SummarizedExperiment::assayNames(x)) {
    stop("Assay not found: ", assay_name, call. = FALSE)
  }
  meta <- S4Vectors::metadata(x)
  rates <- meta$assay_sampling_rates
  if (is.null(rates)) {
    assay_names <- SummarizedExperiment::assayNames(x)
    rates <- rep(samplingRate(x), length(assay_names))
    names(rates) <- assay_names
  }
  rates[[assay_name]] <- rate
  meta$assay_sampling_rates <- rates
  S4Vectors::metadata(x) <- meta
  x
}
