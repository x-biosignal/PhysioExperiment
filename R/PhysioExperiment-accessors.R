#' Accessors for PhysioExperiment
#'
#' These helper functions expose common slots and derived quantities for
#' `PhysioExperiment` objects.

#' Get or set sampling rate
#'
#' @param x A PhysioExperiment object.
#' @param value Numeric scalar for the new sampling rate in Hz.
#' @return The sampling rate in Hz.
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

#' @export
setMethod("samplingRate", "PhysioExperiment", function(x) x@samplingRate)

#' @export
setGeneric("samplingRate<-", function(x, value) standardGeneric("samplingRate<-"))

#' @export
setReplaceMethod("samplingRate", "PhysioExperiment", function(x, value) {
  x@samplingRate <- value
  methods::validObject(x)
  x
})

#' Retrieve the default assay name
#'
#' @param x A `PhysioExperiment` instance.
#' @return Character scalar naming the first assay or `NA_character_` when absent.
#' @export
defaultAssay <- function(x) {
  ans <- SummarizedExperiment::assayNames(x)
  if (length(ans)) ans[[1]] else NA_character_
}
