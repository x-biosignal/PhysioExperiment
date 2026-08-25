# Helper functions for testthat

#' Create a test PhysioExperiment with 2D data
#' @param n_time Number of time points
#' @param n_channels Number of channels
#' @param sr Sampling rate in Hz
#' @return PhysioExperiment object
make_pe_2d <- function(n_time = 1000, n_channels = 4, sr = 250) {
  data <- matrix(rnorm(n_time * n_channels), nrow = n_time, ncol = n_channels)
  colnames(data) <- paste0("Ch", seq_len(n_channels))

  PhysioExperiment(
    assays = list(raw = data),
    colData = S4Vectors::DataFrame(
      label = paste0("Ch", seq_len(n_channels)),
      type = rep("EEG", n_channels)
    ),
    samplingRate = sr
  )
}

#' Create a test PhysioExperiment with 3D data (multi-sample)
#' @param n_time Number of time points
#' @param n_channels Number of channels
#' @param n_samples Number of samples
#' @param sr Sampling rate in Hz
#' @return PhysioExperiment object
make_pe_3d <- function(n_time = 100, n_channels = 4, n_samples = 3, sr = 250) {
  data <- array(rnorm(n_time * n_channels * n_samples),
                dim = c(n_time, n_channels, n_samples))

  PhysioExperiment(
    assays = list(raw = data),
    colData = S4Vectors::DataFrame(
      label = paste0("Ch", seq_len(n_channels)),
      type = rep("EEG", n_channels)
    ),
    samplingRate = sr
  )
}
