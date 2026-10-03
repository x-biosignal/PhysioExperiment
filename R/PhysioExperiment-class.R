#' PhysioExperiment class definition
#'
#' The `PhysioExperiment` class extends `SummarizedExperiment` to store
#' multi-modal physiological signal data alongside metadata such as sampling
#' rate.  This file defines the class, its validity checks, and the
#' user-facing constructor.
#'
#' @slot samplingRate Numeric scalar describing the acquisition frequency in Hz.
#'
#' @seealso \code{\link{PhysioExperiment}} for the constructor,
#'   \code{\link{samplingRate}} for accessing the sampling rate,
#'   \code{\link{channelInfo}} for channel metadata
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
#' Creates a new \code{PhysioExperiment} instance, which extends
#' \code{SummarizedExperiment} with a \code{samplingRate} slot for
#' physiological signal data.
#'
#' @param assays A `SimpleList` (or coercible object) of assay arrays.
#' @param rowData Feature-level metadata as a `DataFrame`.
#' @param colData Sample-level metadata as a `DataFrame`.
#' @param metadata Optional experiment-level metadata list.
#' @param samplingRate Numeric scalar sampling rate in Hz.
#' @param provenance Optional. Either a character source identifier (e.g. a file
#'   path or dataset id) - in which case an initial PROV \code{"import"} activity
#'   recording \code{wasDerivedFrom} that source is seeded - or a pre-built
#'   provenance log (a list of entries) to attach. \code{NULL} (default) leaves
#'   the object with an empty audit trail. See \code{\link{provenance}}.
#' @return A \code{PhysioExperiment} object containing the supplied assays,
#'   row/column metadata, and sampling rate.
#' @seealso \code{\link{samplingRate}} for accessing the sampling rate,
#'   \code{\link{defaultAssay}} for retrieving the first assay name,
#'   \code{\link{channelInfo}} for channel metadata,
#'   \code{\link{setEvents}} for attaching event information
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
    samplingRate = as.numeric(NA),
    provenance = NULL) {
  # Build args for SummarizedExperiment, only including non-NULL values
  se_args <- list(assays = assays, metadata = metadata)
  if (!is.null(rowData) && nrow(rowData) > 0) {
    se_args$rowData <- rowData
  }
  if (!is.null(colData) && nrow(colData) > 0) {
    se_args$colData <- colData
  }
  se <- do.call(SummarizedExperiment::SummarizedExperiment, se_args)
  pe <- methods::new("PhysioExperiment", se, samplingRate = samplingRate)

  # Optionally seed the provenance audit trail (see R/provenance.R).
  if (!is.null(provenance)) {
    if (is.character(provenance) && length(provenance) == 1L) {
      pe <- appendProvenance(pe, activity = "import",
                             params = list(wasDerivedFrom = provenance),
                             input_assay = provenance)
    } else if (is.list(provenance)) {
      S4Vectors::metadata(pe)[["provenance"]] <- provenance
    } else {
      stop("`provenance` must be a source-id string or a list of entries",
           call. = FALSE)
    }
  }
  pe
}
