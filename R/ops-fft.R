#' Fast Fourier transform helper
#'
#' Computes the discrete Fourier transform along the time axis of the default
#' assay and stores the magnitude spectrum in a new assay named `"fft"`.
#'
#' @param x A `PhysioExperiment` object.
#' @return The modified object containing an FFT assay.
#' @export
fftSignals <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available for FFT", call. = FALSE)
  }
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  fft_data <- data
  if (length(dims) >= 1) {
    fft_apply <- function(vec) Mod(stats::fft(vec))
    fft_data[] <- apply(data, seq_along(dims)[-1], fft_apply)
  }
  assays <- SummarizedExperiment::assays(x)
  assays[["fft"]] <- fft_data
  SummarizedExperiment::assays(x) <- assays
  x
}
