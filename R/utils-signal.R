#' Time index helper
#'
#' Computes a time vector for the default assay using the object's sampling
#' rate.
#'
#' @param x A `PhysioExperiment` instance.
#' @return Numeric vector of time points in seconds.
#' @references
#' Oostenveld R, Fries P, Maris E, Schoffelen JM (2011). "FieldTrip: Open
#' source software for advanced analysis of MEG, EEG, and invasive
#' electrophysiological data." \emph{Computational Intelligence and
#' Neuroscience}, 2011, 156869.
#' @seealso \code{\link{samplingRate}} for the sampling rate,
#'   \code{\link{duration}} for signal duration,
#'   \code{\link{timeToSamples}} for converting times to sample indices
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(40), nrow = 10, ncol = 4)),
#'   samplingRate = 100
#' )
#' head(timeIndex(pe))
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


#' Weighted phase-lag index estimator (raw and debiased)
#'
#' Computes the weighted phase-lag index (wPLI) and its unbiased (debiased)
#' estimator from the imaginary part of a cross-spectrum, following Vinck et al.
#' (2011, Eq. 6). This is the single source of the wPLI debiasing math shared by
#' \pkg{PhysioEEG} (\code{eegWPLI}) and \pkg{PhysioCrossModal}
#' (\code{weightedPLI}), so the two agree exactly on identical input.
#'
#' The estimators, over the imaginary cross-spectrum values \eqn{X_j} (one per
#' window, taper, or time sample), are
#' \deqn{\mathrm{wPLI} = \frac{|\sum_j X_j|}{\sum_j |X_j|}}
#' \deqn{\mathrm{debiased\ wPLI} =
#'   \frac{(\sum_j X_j)^2 - \sum_j X_j^2}{(\sum_j |X_j|)^2 - \sum_j X_j^2}.}
#' The debiased numerator and denominator subtract the diagonal self-terms, so
#' the estimator is unbiased: for independent signals it is distributed around 0
#' rather than being inflated toward positive values.
#'
#' @param imag Numeric vector of imaginary cross-spectrum values.
#' @param debiased Logical; also compute the debiased estimator (default TRUE).
#' @return A list with \code{wpli} (raw wPLI, from 0 to 1), \code{wpli_debiased}
#'   (the debiased estimator, or \code{NA} if \code{debiased = FALSE} or fewer
#'   than two values), and \code{n} (the number of values used).
#' @references
#' Vinck, M., Oostenveld, R., van Wingerden, M., Battaglia, F., & Pennartz,
#' C.M.A. (2011). "An improved index of phase-synchronization for
#' electrophysiological data in the presence of volume-conduction, noise and
#' sample-size bias." \emph{NeuroImage}, 55(4), 1548-1565.
#' \doi{10.1016/j.neuroimage.2011.01.055}
#' @examples
#' set.seed(1)
#' wpliEstimate(rnorm(500))
#' @export
wpliEstimate <- function(imag, debiased = TRUE) {
  imag <- as.numeric(imag)
  n <- length(imag)
  abs_imag <- abs(imag)
  sum_abs <- sum(abs_imag)

  wpli <- if (sum_abs < .Machine$double.eps) 0 else abs(sum(imag)) / sum_abs

  wpli_debiased <- NA_real_
  if (debiased && n > 1) {
    sum_imag <- sum(imag)
    sum_imag_sq <- sum(imag^2)
    denom <- sum_abs^2 - sum_imag_sq
    wpli_debiased <- if (abs(denom) < .Machine$double.eps) {
      0
    } else {
      (sum_imag^2 - sum_imag_sq) / denom
    }
  }

  list(wpli = wpli, wpli_debiased = wpli_debiased, n = as.integer(n))
}
