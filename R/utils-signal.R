#' Time index helper
#'
#' Computes a time vector for the default assay using the object's sampling
#' rate.
#'
#' @param x A `PhysioExperiment` instance.
#' @return Numeric vector of time points in seconds.
#' @export
timeIndex <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    return(numeric())
  }
  data <- SummarizedExperiment::assay(x, assay_name)
  n <- dim(data)[1]
  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    return(seq_len(n))
  }
  (seq_len(n) - 1) / sr
}
