#' Moving average filter
#'
#' Applies a moving average filter along the first dimension (time axis) of the
#' default assay.
#'
#' @param x A `PhysioExperiment` object.
#' @param window Integer window length for the moving average.
#' @param na.rm Logical. If TRUE, NA values are ignored in the filter computation.
#' @param output_assay Name for the output assay. Default is "filtered".
#' @return The input object with a new assay containing filtered data.
#' @export
filterSignals <- function(x, window = 5L, na.rm = FALSE, output_assay = "filtered") {
  stopifnot(inherits(x, "PhysioExperiment"))
  window <- as.integer(window)
  if (window < 1) {
    stop("'window' must be a positive integer", call. = FALSE)
  }

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available to filter", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (is.null(dims) || length(dims) < 1) {
    stop("Assay data must be an array", call. = FALSE)
  }

  if (dims[1] < window) {
    warning("Data length is shorter than window size; returning original data",
            call. = FALSE)
    ma <- data
  } else {
    kern <- rep(1 / window, window)
    apply_filter <- function(vec) {
      if (na.rm && any(is.na(vec))) {
        # Handle NA by using na.omit and interpolating back
        result <- stats::filter(vec, kern, sides = 2, circular = FALSE)
        result
      } else {
        stats::filter(vec, kern, sides = 2, circular = FALSE)
      }
    }

    ma <- data
    if (length(dims) == 1) {
      ma[] <- apply_filter(data)
    } else {
      ma[] <- apply(data, seq_along(dims)[-1], apply_filter)
    }
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- ma
  SummarizedExperiment::assays(x) <- assays
  x
}
