#' PhysioExperiment class definition
#'
#' The `PhysioExperiment` class extends `SummarizedExperiment` to store
#' multi-modal physiological signal data alongside metadata such as sampling
#' rate.  This file defines the class, its validity checks, and the
#' user-facing constructor.
#'
#' @slot samplingRate Numeric scalar describing the acquisition frequency in Hz.
#' @exportClass PhysioExperiment
setClass(
  "PhysioExperiment",
  contains = "SummarizedExperiment",
  slots = list(
    samplingRate = "numeric"
  ),
  prototype = list(
    samplingRate = as.numeric(NA)
  ),
  validity = function(object) {
    sr <- object@samplingRate
    if (length(sr) > 1) {
      return("'samplingRate' must be a scalar numeric value")
    }
    if (length(sr) == 1 && !is.na(sr) && sr <= 0) {
      return("'samplingRate' must be a positive numeric value")
    }
    TRUE
  }
)

#' Construct a PhysioExperiment object
#'
#' @param assays A `SimpleList` (or coercible object) of assay arrays.
#' @param rowData Feature-level metadata as a `DataFrame`.
#' @param colData Sample-level metadata as a `DataFrame`.
#' @param metadata Optional experiment-level metadata list.
#' @param samplingRate Numeric scalar sampling rate in Hz.
#' @return A `PhysioExperiment` instance.
#' @export
#' @examples
#' # Create a simple PhysioExperiment with random EEG-like data
#' # 1000 time points, 4 channels
#' eeg_data <- matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)
#' colnames(eeg_data) <- c("Fz", "Cz", "Pz", "Oz")
#'
#' pe <- PhysioExperiment(
#'   assays = list(raw = eeg_data),
#'   colData = S4Vectors::DataFrame(
#'     label = c("Fz", "Cz", "Pz", "Oz"),
#'     type = rep("EEG", 4)
#'   ),
#'   samplingRate = 250
#' )
#' pe
#'
#' # Access sampling rate
#' samplingRate(pe)
#'
#' # Create with multiple assays
#' pe2 <- PhysioExperiment(
#'   assays = list(raw = eeg_data, filtered = eeg_data * 0.5),
#'   samplingRate = 500
#' )
PhysioExperiment <- function(
    assays = S4Vectors::SimpleList(),
    rowData = NULL,
    colData = NULL,
    metadata = list(),
    samplingRate = as.numeric(NA)) {
  # Build args for SummarizedExperiment, only including non-NULL values
  se_args <- list(assays = assays, metadata = metadata)
  if (!is.null(rowData) && nrow(rowData) > 0) {
    se_args$rowData <- rowData
  }
  if (!is.null(colData) && nrow(colData) > 0) {
    se_args$colData <- colData
  }
  se <- do.call(SummarizedExperiment::SummarizedExperiment, se_args)
  # Ensure samplingRate is numeric (NA is logical by default)
  samplingRate <- as.numeric(samplingRate)
  methods::new("PhysioExperiment", se, samplingRate = samplingRate)
}
