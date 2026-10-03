#' Accessors for PhysioExperiment
#'
#' These helper functions expose common slots and derived quantities for
#' `PhysioExperiment` objects.

#' Get or set sampling rate
#'
#' @param x A PhysioExperiment object.
#' @param value Numeric scalar for the new sampling rate in Hz.
#' @return For \code{samplingRate(x)}: a numeric scalar giving the sampling
#'   rate in Hz. For \code{samplingRate(x) <- value}: the modified
#'   \code{PhysioExperiment} object (returned invisibly).
#' @seealso \code{\link{PhysioExperiment}} for the constructor,
#'   \code{\link{defaultAssay}} for the default assay name,
#'   \code{\link{duration}} for signal duration,
#'   \code{\link{timeIndex}} for time point vector
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
#' @export
#' @examples
#' # Create example data
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(100), nrow = 10)),
#'   samplingRate = 250
#' )
#'
#' # Get sampling rate
#' samplingRate(pe)
#'
#' # Set sampling rate
#' samplingRate(pe) <- 500
#' samplingRate(pe)
setGeneric("samplingRate", function(x) standardGeneric("samplingRate"))

#' @rdname samplingRate
#' @export
setMethod("samplingRate", "PhysioExperiment", function(x) x@samplingRate)

#' @rdname samplingRate
#' @export
setGeneric("samplingRate<-", function(x, value) standardGeneric("samplingRate<-"))

#' @rdname samplingRate
#' @export
setReplaceMethod("samplingRate", "PhysioExperiment", function(x, value) {
  x@samplingRate <- value
  methods::validObject(x)
  x
})

#' Retrieve the default assay name
#'
#' Returns the name of the first assay in the \code{PhysioExperiment} object,
#' which is treated as the default assay for operations that do not specify
#' an assay explicitly.
#'
#' @param x A `PhysioExperiment` instance.
#' @return Character scalar naming the first assay, or \code{NA_character_}
#'   when no assays are present.
#' @seealso \code{\link{PhysioExperiment}} for the constructor,
#'   \code{\link{samplingRate}} for the sampling rate accessor,
#'   \code{\link{timeIndex}} for time point vector
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(100), nrow = 10), filtered = matrix(0, 10, 10)),
#'   samplingRate = 250
#' )
#' defaultAssay(pe)
#' @export
defaultAssay <- function(x) {
  ans <- SummarizedExperiment::assayNames(x)
  if (length(ans)) ans[[1]] else NA_character_
}
