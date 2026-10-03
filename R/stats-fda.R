#' Functional Data Analysis (FDA) for Biomechanics
#'
#' Functions for functional data analysis including functional PCA (fPCA),
#' functional regression, and curve registration. These methods treat
#' biomechanical waveforms as continuous functions.

#' Functional Principal Component Analysis (fPCA)
#'
#' Performs functional PCA on waveform data to identify the main modes
#' of variation in movement patterns.
#'
#' @param x A PhysioExperiment object or matrix (time x observations).
#' @param n_components Number of principal components to retain.
#' @param smooth Logical; if TRUE, smooths the data before analysis.
#' @param smooth_param Smoothing parameter (higher = smoother).
#'
#' @return A list of class "fpca_result" containing:
#'   \item{scores}{PC scores for each observation (observations x components)}
#'   \item{loadings}{PC loadings/eigenfunctions (time x components)}
#'   \item{variance_explained}{Proportion of variance explained by each PC}
#'   \item{cumulative_variance}{Cumulative variance explained}
#'   \item{mean_function}{Mean waveform across observations}
#'
#' @details
#' fPCA decomposes waveform variability into orthogonal modes. In gait analysis,
#' PC1 often represents overall amplitude, PC2 timing/phase shifts, and
#' subsequent PCs capture more subtle shape variations.
#'
#' @references
#' Ramsay JO, Silverman BW (2005). "Functional Data Analysis." 2nd ed. Springer.
#'
#' @seealso [reconstructFPCA()] for waveform reconstruction from fPCA results,
#'   plotFPCA() (in PhysioMoCap) for visualization of fPCA results,
#'   [registerCurves()] for separating phase and amplitude variation.
#'
#' @export
#' @examples
#' # Simulate gait angle data (100 time points x 30 subjects)
#' set.seed(123)
#' t <- seq(0, 100, length.out = 100)
#' base_curve <- sin(2 * pi * t / 100) * 30
#'
#' # Add subject variability
#' data <- sapply(1:30, function(i) {
#'   amplitude <- rnorm(1, 1, 0.2)
#'   phase <- rnorm(1, 0, 5)
#'   base_curve * amplitude + rnorm(100, 0, 2)
#' })
#'
#' pe <- PhysioExperiment(assays = list(values = data), samplingRate = 100)
#' fpca_result <- fPCA(pe, n_components = 4)
fPCA <- function(x, n_components = 5, smooth = FALSE, smooth_param = 10) {

  # Extract data
  if (inherits(x, "PhysioExperiment")) {
    assay_name <- defaultAssay(x)
    data <- SummarizedExperiment::assay(x, assay_name)
  } else if (is.matrix(x)) {
    data <- x
  } else {
    stop("Input must be a PhysioExperiment or matrix", call. = FALSE)
  }

  n_time <- nrow(data)
  n_obs <- ncol(data)

  # Optional smoothing
  if (smooth) {
    data <- apply(data, 2, function(col) {
      .smoothCurve(col, smooth_param)
    })
  }

  # Center the data (subtract mean function)
  mean_function <- rowMeans(data, na.rm = TRUE)
  data_centered <- data - mean_function

  # Compute covariance matrix
  cov_matrix <- tcrossprod(data_centered) / (n_obs - 1)

  # Eigendecomposition
  eig <- eigen(cov_matrix, symmetric = TRUE)

  # Retain requested number of components
  n_components <- min(n_components, n_obs - 1, n_time)

  eigenvalues <- eig$values[seq_len(n_components)]
  eigenfunctions <- eig$vectors[, seq_len(n_components), drop = FALSE]

  # Variance explained
  total_variance <- sum(eig$values[eig$values > 0])
  variance_explained <- eigenvalues / total_variance
  cumulative_variance <- cumsum(variance_explained)

  # Compute scores (project data onto eigenfunctions)
  scores <- crossprod(data_centered, eigenfunctions)

  # Normalize eigenfunctions for interpretability
  # Scale so that variance of scores equals eigenvalue
  for (i in seq_len(n_components)) {
    eigenfunctions[, i] <- eigenfunctions[, i] * sqrt(eigenvalues[i])
  }

  result <- list(
    scores = scores,
    loadings = eigenfunctions,
    eigenvalues = eigenvalues,
    variance_explained = variance_explained,
    cumulative_variance = cumulative_variance,
    mean_function = mean_function,
    n_components = n_components,
    n_time = n_time,
    n_obs = n_obs
  )

  class(result) <- c("fpca_result", "list")
  result
}

#' Reconstruct waveforms from fPCA
#'
#' Reconstructs individual waveforms using a subset of principal components.
#'
#' @param fpca_result An fpca_result object from fPCA().
#' @param n_components Number of components to use for reconstruction.
#' @param observation Indices of observations to reconstruct. If NULL, all.
#'
#' @return Matrix of reconstructed waveforms (time x observations).
#'
#' @references
#' Ramsay JO, Silverman BW (2005). "Functional Data Analysis." 2nd ed. Springer.
#'
#' @seealso [fPCA()] for performing the decomposition,
#'   plotFPCA() (in PhysioMoCap) for visualizing fPCA results.
#'
#' @export
#' @examples
#' # Create sample data and run fPCA first
#' set.seed(123)
#' t <- seq(0, 100, length.out = 100)
#' base_curve <- sin(2 * pi * t / 100) * 30
#' data <- sapply(1:20, function(i) base_curve * rnorm(1, 1, 0.2) + rnorm(100, 0, 2))
#' fpca_result <- fPCA(data, n_components = 4)
#'
#' # Reconstruct using only first 2 PCs
#' reconstructed <- reconstructFPCA(fpca_result, n_components = 2)
reconstructFPCA <- function(fpca_result, n_components = NULL, observation = NULL) {

  if (!inherits(fpca_result, "fpca_result")) {
    stop("Input must be an fpca_result object", call. = FALSE)
  }

  if (is.null(n_components)) {
    n_components <- fpca_result$n_components
  }
  n_components <- min(n_components, fpca_result$n_components)

  if (is.null(observation)) {
    observation <- seq_len(fpca_result$n_obs)
  }

  # Reconstruct: mean + sum(score * loading)
  loadings <- fpca_result$loadings[, seq_len(n_components), drop = FALSE]
  scores <- fpca_result$scores[observation, seq_len(n_components), drop = FALSE]

  # Normalize loadings back (undo the scaling in fPCA)
  for (i in seq_len(n_components)) {
    loadings[, i] <- loadings[, i] / sqrt(fpca_result$eigenvalues[i])
  }

  reconstructed <- fpca_result$mean_function + tcrossprod(loadings, scores)

  reconstructed
}

#' Curve registration (time warping)
#'
#' Aligns waveforms by estimating and removing phase variation.
#' Uses landmark registration or continuous registration.
#'
#' @param x A PhysioExperiment object or matrix (time x observations).
#' @param method Registration method: "landmark" or "continuous".
#' @param landmarks For landmark method, matrix of landmark times (landmarks x obs).
#' @param template Template curve to align to. If NULL, uses mean.
#'
#' @return A list containing:
#'   \item{registered}{Registered waveforms}
#'   \item{warping}{Warping functions}
#'   \item{template}{Template used for registration}
#'
#' @details
#' Phase variation (timing differences) can obscure amplitude differences
#' in biomechanical data. Registration separates phase and amplitude variation.
#'
#' @references
#' Ramsay JO, Silverman BW (2005). "Functional Data Analysis." 2nd ed. Springer.
#'
#' @seealso [fPCA()] for functional PCA after registration,
#'   plotGaitCycle() (in PhysioMoCap) for plotting registered gait waveforms.
#'
#' @export
#' @examples
#' # Simulate data with phase variation
#' set.seed(123)
#' t <- seq(0, 100, length.out = 100)
#'
#' data <- sapply(1:20, function(i) {
#'   phase_shift <- rnorm(1, 0, 10)
#'   t_shifted <- t + phase_shift
#'   sin(2 * pi * t_shifted / 100) * 30 + rnorm(100, 0, 2)
#' })
#'
#' pe <- PhysioExperiment(assays = list(values = data), samplingRate = 100)
#' reg_result <- registerCurves(pe, method = "continuous")
registerCurves <- function(x, method = c("continuous", "landmark"),
                           landmarks = NULL, template = NULL) {

  method <- match.arg(method)

  # Extract data
  if (inherits(x, "PhysioExperiment")) {
    assay_name <- defaultAssay(x)
    data <- SummarizedExperiment::assay(x, assay_name)
  } else if (is.matrix(x)) {
    data <- x
  } else {
    stop("Input must be a PhysioExperiment or matrix", call. = FALSE)
  }

  n_time <- nrow(data)
  n_obs <- ncol(data)

  # Determine template
  if (is.null(template)) {
    template <- rowMeans(data, na.rm = TRUE)
  }

  if (method == "landmark") {
    if (is.null(landmarks)) {
      stop("Landmarks required for landmark registration", call. = FALSE)
    }
    result <- .landmarkRegister(data, landmarks, template)
  } else {
    result <- .continuousRegister(data, template)
  }

  result$method <- method
  result$n_time <- n_time
  result$n_obs <- n_obs

  class(result) <- c("registration_result", "list")
  result
}

#' Landmark-based registration
#' @noRd
.landmarkRegister <- function(data, landmarks, template) {
  n_time <- nrow(data)
  n_obs <- ncol(data)
  n_landmarks <- nrow(landmarks)

  # Target landmark positions (from template or evenly spaced)
  target_landmarks <- seq(1, n_time, length.out = n_landmarks)

  registered <- matrix(NA_real_, nrow = n_time, ncol = n_obs)
  warping <- matrix(NA_real_, nrow = n_time, ncol = n_obs)

  for (i in seq_len(n_obs)) {
    # Create piecewise linear warping function
    source_landmarks <- landmarks[, i]

    # Interpolate warping function
    warp_func <- stats::approx(
      x = source_landmarks,
      y = target_landmarks,
      xout = seq_len(n_time),
      rule = 2
    )$y

    warping[, i] <- warp_func

    # Apply warping
    registered[, i] <- stats::approx(
      x = seq_len(n_time),
      y = data[, i],
      xout = warp_func,
      rule = 2
    )$y
  }

  list(
    registered = registered,
    warping = warping,
    template = template
  )
}

#' Continuous registration (simplified)
#' @noRd
.continuousRegister <- function(data, template, max_iter = 20, tol = 1e-4) {
  n_time <- nrow(data)
  n_obs <- ncol(data)

  registered <- data
  warping <- matrix(rep(seq_len(n_time), n_obs), nrow = n_time, ncol = n_obs)

  # Iterative registration using cross-correlation
  for (iter in seq_len(max_iter)) {
    old_registered <- registered

    for (i in seq_len(n_obs)) {
      # Find optimal shift using cross-correlation
      ccf_result <- stats::ccf(template, registered[, i], plot = FALSE, lag.max = n_time / 4)
      best_lag <- ccf_result$lag[which.max(ccf_result$acf)]

      if (abs(best_lag) > 0) {
        # Apply shift
        if (best_lag > 0) {
          registered[, i] <- c(rep(NA, best_lag), data[1:(n_time - best_lag), i])
        } else {
          registered[, i] <- c(data[(-best_lag + 1):n_time, i], rep(NA, -best_lag))
        }

        # Update warping
        warping[, i] <- warping[, i] - best_lag
      }
    }

    # Fill NA with linear interpolation
    for (i in seq_len(n_obs)) {
      if (any(is.na(registered[, i]))) {
        valid_idx <- which(!is.na(registered[, i]))
        if (length(valid_idx) > 1) {
          registered[, i] <- stats::approx(
            x = valid_idx,
            y = registered[valid_idx, i],
            xout = seq_len(n_time),
            rule = 2
          )$y
        }
      }
    }

    # Check convergence
    change <- mean((registered - old_registered)^2, na.rm = TRUE)
    if (change < tol) break
  }

  list(
    registered = registered,
    warping = warping,
    template = template
  )
}

#' Smooth a curve using moving average
#' @noRd
.smoothCurve <- function(x, param) {
  n <- length(x)
  window <- min(param, n %/% 2)

  if (window < 2) return(x)

  kernel <- rep(1 / window, window)
  smoothed <- stats::filter(x, kernel, sides = 2)

  # Handle edges
  smoothed[is.na(smoothed)] <- x[is.na(smoothed)]

  as.numeric(smoothed)
}

#' Print fPCA result
#' @param x An fpca_result object
#' @param ... Additional arguments (unused)
#'
#' @references
#' Ramsay JO, Silverman BW (2005). "Functional Data Analysis." 2nd ed. Springer.
#'
#' @seealso [fPCA()] for performing functional principal component analysis,
#'   plotFPCA() (in PhysioMoCap) for visualization of fPCA results,
#'   [reconstructFPCA()] for waveform reconstruction from fPCA scores.
#'
#' @export
print.fpca_result <- function(x, ...) {
  cat("Functional PCA Result\n")
  cat("====================\n")
  cat(sprintf("Observations: %d\n", x$n_obs))
  cat(sprintf("Time points: %d\n", x$n_time))
  cat(sprintf("Components retained: %d\n", x$n_components))
  cat("\nVariance explained:\n")

  for (i in seq_len(min(5, x$n_components))) {
    cat(sprintf("  PC%d: %.1f%% (cumulative: %.1f%%)\n",
                i, x$variance_explained[i] * 100, x$cumulative_variance[i] * 100))
  }

  invisible(x)
}
