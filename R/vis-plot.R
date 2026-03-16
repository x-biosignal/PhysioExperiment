#' Plot a signal channel
#'
#' Generates a simple line plot for the selected channel and sample from the
#' default assay. Supports both 2D (time x channel) and 3D (time x channel x sample)
#' assay arrays.
#'
#' @param x A `PhysioExperiment` object.
#' @param channel Integer index for the channel.
#' @param sample Integer index for the sample (only used for 3D arrays).
#' @param assay_name Optional assay name. If NULL, uses the default assay.
#' @return A `ggplot` object.
#' @export
plotSignal <- function(x, channel = 1L, sample = 1L, assay_name = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (is.null(assay_name)) {
    assay_name <- defaultAssay(x)
  }

  if (is.na(assay_name)) {
    stop("No assays available for plotting", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  ndim <- length(dims)

  if (ndim < 2) {
    stop("Assay must be at least two-dimensional", call. = FALSE)
  }

  channel <- as.integer(channel)
  sample <- as.integer(sample)

  # Extract signal based on dimensionality

  if (ndim == 2) {
    # 2D: time x channel
    if (channel < 1 || channel > dims[2]) {
      stop("Channel index out of range", call. = FALSE)
    }
    signal <- data[, channel, drop = TRUE]
  } else if (ndim == 3) {
    # 3D: time x channel x sample
    if (channel < 1 || channel > dims[2]) {
      stop("Channel index out of range", call. = FALSE)
    }
    if (sample < 1 || sample > dims[3]) {
      stop("Sample index out of range", call. = FALSE)
    }
    signal <- data[, channel, sample, drop = TRUE]
  } else {
    stop("Assay must be 2D or 3D array", call. = FALSE)
  }

  t <- timeIndex(x)
  df <- data.frame(time = t, amplitude = signal)

  ggplot2::ggplot(df, ggplot2::aes(time, amplitude)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time (s)", y = "Amplitude", title = "Signal trace")
}
