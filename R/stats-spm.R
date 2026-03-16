#' Statistical Parametric Mapping (SPM1D) for Biomechanics
#'
#' Functions for statistical analysis of continuous waveform data using
#' Statistical Parametric Mapping (SPM) methodology adapted from neuroimaging.
#' These methods test hypotheses over entire waveforms rather than discrete points.

#' SPM t-test for waveform comparison
#'
#' Performs a t-test at each time point and computes SPM{t} statistic
#' with Random Field Theory (RFT) correction for multiple comparisons.
#'
#' @param x A PhysioExperiment object or matrix (time x observations).
#' @param group1 Indices for first group (for two-sample test).
#' @param group2 Indices for second group. If NULL, performs one-sample test.
#' @param alpha Significance level (default: 0.05).
#' @param two_tailed Logical; if TRUE, performs two-tailed test.
#'
#' @return A list of class "spm_result" containing:
#'   \item{t}{T-statistic at each time point}
#'   \item{threshold}{Critical threshold from RFT}
#'   \item{clusters}{Significant clusters (start, end, extent, p-value)}
#'   \item{p_values}{Pointwise p-values}
#'   \item{alpha}{Significance level used}
#'
#' @details
#' SPM analyzes continuous biomechanical waveforms (e.g., joint angles, moments)
#' by computing t-statistics at each time point and using Random Field Theory
#' to control family-wise error rate across the entire waveform.
#'
#' @references
#' Pataky TC (2012). One-dimensional statistical parametric mapping in Python.
#' Computer Methods in Biomechanics and Biomedical Engineering.
#'
#' @export
#' @examples
#' # Create example gait data (100 time points x 20 subjects)
#' set.seed(123)
#' # Group 1: normal gait
#' g1 <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' # Group 2: altered gait (effect at 40-60% of cycle)
#' g2 <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' g2[40:60, ] <- g2[40:60, ] + 1.5
#'
#' data <- cbind(g1, g2)
#' pe <- PhysioExperiment(
#'   assays = list(values = data),
#'   samplingRate = 100
#' )
#'
#' # Two-sample SPM t-test
#' result <- spmTTest(pe, group1 = 1:10, group2 = 11:20)
#' print(result)
spmTTest <- function(x, group1 = NULL, group2 = NULL,
                     alpha = 0.05, two_tailed = TRUE) {

  # Extract data matrix
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

  # Determine test type
  if (is.null(group1)) {
    group1 <- seq_len(n_obs)
  }

  two_sample <- !is.null(group2)

  if (two_sample) {
    # Two-sample t-test
    data1 <- data[, group1, drop = FALSE]
    data2 <- data[, group2, drop = FALSE]
    n1 <- ncol(data1)
    n2 <- ncol(data2)

    # Compute t-statistic at each time point
    m1 <- rowMeans(data1, na.rm = TRUE)
    m2 <- rowMeans(data2, na.rm = TRUE)
    v1 <- apply(data1, 1, var, na.rm = TRUE)
    v2 <- apply(data2, 1, var, na.rm = TRUE)

    # Pooled variance (equal variance assumption for SPM)
    sp <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
    se <- sp * sqrt(1/n1 + 1/n2)

    t_stat <- (m1 - m2) / se
    df <- n1 + n2 - 2

  } else {
    # One-sample t-test against zero
    data1 <- data[, group1, drop = FALSE]
    n1 <- ncol(data1)

    m <- rowMeans(data1, na.rm = TRUE)
    s <- apply(data1, 1, sd, na.rm = TRUE)
    se <- s / sqrt(n1)

    t_stat <- m / se
    df <- n1 - 1
  }

  # Handle NA/Inf
  t_stat[!is.finite(t_stat)] <- 0

  # Estimate smoothness (FWHM) for RFT
  fwhm <- .estimateFWHM(t_stat)

  # Compute RFT threshold
  threshold <- .rftThreshold(n_time, fwhm, alpha, df, two_tailed)

  # Find significant clusters
  clusters <- .findSPMClusters(t_stat, threshold, two_tailed)

  # Compute cluster p-values using RFT
  if (length(clusters) > 0) {
    for (i in seq_along(clusters)) {
      clusters[[i]]$p_cluster <- .rftClusterPValue(
        clusters[[i]]$extent, n_time, fwhm, threshold, df
      )
    }
  }

  # Pointwise p-values (uncorrected)
  if (two_tailed) {
    p_values <- 2 * stats::pt(-abs(t_stat), df)
  } else {
    p_values <- stats::pt(-t_stat, df)
  }

  result <- list(
    t = t_stat,
    threshold = threshold,
    clusters = clusters,
    p_values = p_values,
    df = df,
    fwhm = fwhm,
    alpha = alpha,
    two_tailed = two_tailed,
    n_time = n_time,
    test_type = if (two_sample) "two-sample" else "one-sample"
  )

  class(result) <- c("spm_result", "list")
  result
}

#' SPM paired t-test
#'
#' Performs SPM analysis for paired/repeated measures data.
#'
#' @param x A PhysioExperiment object or matrix (time x observations).
#' @param condition1 Indices for first condition.
#' @param condition2 Indices for second condition.
#' @param alpha Significance level.
#' @param two_tailed Logical; if TRUE, performs two-tailed test.
#'
#' @return A list of class "spm_result".
#' @export
#' @examples
#' # Pre-post intervention comparison
#' set.seed(123)
#' pre <- matrix(rnorm(100 * 15), nrow = 100)
#' post <- pre + 0.8  # Effect across all time points
#' post[30:50, ] <- post[30:50, ] + 0.5  # Additional effect
#'
#' data <- cbind(pre, post)
#' pe <- PhysioExperiment(assays = list(values = data), samplingRate = 100)
#'
#' result <- spmPairedTTest(pe, condition1 = 1:15, condition2 = 16:30)
spmPairedTTest <- function(x, condition1, condition2,
                            alpha = 0.05, two_tailed = TRUE) {

  # Extract data
  if (inherits(x, "PhysioExperiment")) {
    assay_name <- defaultAssay(x)
    data <- SummarizedExperiment::assay(x, assay_name)
  } else if (is.matrix(x)) {
    data <- x
  } else {
    stop("Input must be a PhysioExperiment or matrix", call. = FALSE)
  }

  data1 <- data[, condition1, drop = FALSE]
  data2 <- data[, condition2, drop = FALSE]

  if (ncol(data1) != ncol(data2)) {
    stop("Paired test requires equal number of observations in each condition",
         call. = FALSE)
  }

  # Compute differences
  diffs <- data1 - data2

  # One-sample t-test on differences
  result <- spmTTest(diffs, alpha = alpha, two_tailed = two_tailed)
  result$test_type <- "paired"

  result
}

#' SPM ANOVA (F-test)
#'
#' Performs SPM F-test for comparing multiple groups.
#'
#' @param x A PhysioExperiment object or matrix (time x observations).
#' @param groups Factor or list indicating group membership.
#' @param alpha Significance level.
#'
#' @return A list of class "spm_result" containing F-statistics and clusters.
#' @export
#' @examples
#' # Three-group comparison
#' set.seed(123)
#' g1 <- matrix(rnorm(100 * 8), nrow = 100)
#' g2 <- matrix(rnorm(100 * 8), nrow = 100) + 0.5
#' g3 <- matrix(rnorm(100 * 8), nrow = 100) + 1.0
#'
#' data <- cbind(g1, g2, g3)
#' groups <- factor(rep(c("A", "B", "C"), each = 8))
#'
#' pe <- PhysioExperiment(assays = list(values = data), samplingRate = 100)
#' result <- spmAnova(pe, groups = groups)
spmAnova <- function(x, groups, alpha = 0.05) {

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

  groups <- as.factor(groups)
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  if (n_groups < 2) {
    stop("At least 2 groups required for ANOVA", call. = FALSE)
  }

  # Compute F-statistic at each time point
  f_stat <- numeric(n_time)
  df1 <- n_groups - 1
  df2 <- n_obs - n_groups

  for (t_idx in seq_len(n_time)) {
    vals <- data[t_idx, ]

    # Between-group variance
    grand_mean <- mean(vals, na.rm = TRUE)
    ss_between <- 0
    ss_within <- 0

    for (g in group_levels) {
      group_vals <- vals[groups == g]
      n_g <- sum(!is.na(group_vals))
      group_mean <- mean(group_vals, na.rm = TRUE)

      ss_between <- ss_between + n_g * (group_mean - grand_mean)^2
      ss_within <- ss_within + sum((group_vals - group_mean)^2, na.rm = TRUE)
    }

    ms_between <- ss_between / df1
    ms_within <- ss_within / df2

    if (ms_within > 0) {
      f_stat[t_idx] <- ms_between / ms_within
    }
  }

  # Estimate smoothness
  fwhm <- .estimateFWHM(f_stat)

  # RFT threshold for F-distribution
  threshold <- .rftThresholdF(n_time, fwhm, alpha, df1, df2)

  # Find clusters
  clusters <- .findSPMClustersF(f_stat, threshold)

  # Pointwise p-values
  p_values <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)

  result <- list(
    f = f_stat,
    threshold = threshold,
    clusters = clusters,
    p_values = p_values,
    df1 = df1,
    df2 = df2,
    fwhm = fwhm,
    alpha = alpha,
    n_time = n_time,
    groups = group_levels,
    test_type = "anova"
  )

  class(result) <- c("spm_result", "list")
  result
}

#' Estimate FWHM (Full Width at Half Maximum) for RFT
#' @noRd
.estimateFWHM <- function(x) {
  # Estimate smoothness from residuals using differences
  n <- length(x)
  if (n < 3) return(1)

  # First derivative
  dx <- diff(x)

  # Variance of derivative
  var_dx <- var(dx, na.rm = TRUE)

  # Variance of signal
  var_x <- var(x, na.rm = TRUE)

  if (var_x == 0 || !is.finite(var_dx)) return(n)

  # FWHM estimation (simplified)
  # FWHM = sqrt(4 * log(2) * var_x / var_dx)
  fwhm <- sqrt(4 * log(2) * var_x / var_dx)

  # Bound FWHM
  fwhm <- max(1, min(fwhm, n))

  fwhm
}

#' RFT threshold for t-distribution
#' @noRd
.rftThreshold <- function(n_time, fwhm, alpha, df, two_tailed) {
  # Simplified RFT threshold using Bonferroni-like correction
 # based on number of resels (resolution elements)
  resels <- n_time / fwhm

  # Expected Euler characteristic
  # For high thresholds, use Bonferroni approximation
  if (two_tailed) {
    alpha_adj <- alpha / 2
  } else {
    alpha_adj <- alpha
  }

  # Use t-distribution quantile with resel correction
  # This is a simplified version; full RFT uses expected Euler characteristic
  p_corrected <- alpha_adj / max(1, resels)
  threshold <- stats::qt(1 - p_corrected, df)

  threshold
}

#' RFT threshold for F-distribution
#' @noRd
.rftThresholdF <- function(n_time, fwhm, alpha, df1, df2) {
  resels <- n_time / fwhm
  p_corrected <- alpha / max(1, resels)
  threshold <- stats::qf(1 - p_corrected, df1, df2)
  threshold
}

#' Find SPM clusters for t-test
#' @noRd
.findSPMClusters <- function(t_stat, threshold, two_tailed) {
  n <- length(t_stat)
  clusters <- list()

  if (two_tailed) {
    # Positive clusters
    pos_sig <- t_stat > threshold
    pos_clusters <- .extractClusters(pos_sig, t_stat, "positive")
    clusters <- c(clusters, pos_clusters)

    # Negative clusters
    neg_sig <- t_stat < -threshold
    neg_clusters <- .extractClusters(neg_sig, t_stat, "negative")
    clusters <- c(clusters, neg_clusters)
  } else {
    sig <- t_stat > threshold
    clusters <- .extractClusters(sig, t_stat, "positive")
  }

  clusters
}

#' Find SPM clusters for F-test
#' @noRd
.findSPMClustersF <- function(f_stat, threshold) {
  sig <- f_stat > threshold
  .extractClusters(sig, f_stat, "positive")
}

#' Extract clusters from significance mask
#' @noRd
.extractClusters <- function(sig_mask, stat_values, direction) {
  if (!any(sig_mask)) return(list())

  rle_sig <- rle(sig_mask)
  clusters <- list()

  pos <- 1
  for (i in seq_along(rle_sig$lengths)) {
    if (rle_sig$values[i]) {
      start <- pos
      end <- pos + rle_sig$lengths[i] - 1
      extent <- end - start + 1

      cluster_stat <- sum(abs(stat_values[start:end]))
      peak_stat <- if (direction == "positive") {
        max(stat_values[start:end])
      } else {
        min(stat_values[start:end])
      }

      clusters <- c(clusters, list(list(
        start = start,
        end = end,
        extent = extent,
        cluster_stat = cluster_stat,
        peak_stat = peak_stat,
        direction = direction
      )))
    }
    pos <- pos + rle_sig$lengths[i]
  }

  clusters
}

#' RFT cluster p-value
#' @noRd
.rftClusterPValue <- function(extent, n_time, fwhm, threshold, df) {
  # Simplified cluster p-value using extent
  resels <- n_time / fwhm
  expected_extent <- fwhm  # Expected cluster extent under null

  # Approximate p-value based on cluster extent
  # Using exponential distribution approximation
  lambda <- 1 / expected_extent
  p <- exp(-lambda * extent)

  # Correct for multiple resels
  p_corrected <- 1 - (1 - p)^max(1, resels)

  min(1, p_corrected)
}

#' Plot SPM result
#'
#' Visualizes SPM analysis results with significance thresholds.
#'
#' @param x An spm_result object.
#' @param time_axis Optional time axis values.
#' @param show_threshold Logical; if TRUE, shows significance threshold.
#' @param show_clusters Logical; if TRUE, highlights significant clusters.
#' @param title Plot title.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' # Create and plot SPM result
#' set.seed(123)
#' g1 <- matrix(rnorm(100 * 10), nrow = 100)
#' g2 <- matrix(rnorm(100 * 10), nrow = 100)
#' g2[40:60, ] <- g2[40:60, ] + 1.5
#'
#' pe <- PhysioExperiment(
#'   assays = list(values = cbind(g1, g2)),
#'   samplingRate = 100
#' )
#' result <- spmTTest(pe, group1 = 1:10, group2 = 11:20)
#' plotSPM(result)
plotSPM <- function(x, time_axis = NULL, show_threshold = TRUE,
                    show_clusters = TRUE, title = NULL) {

  if (!inherits(x, "spm_result")) {
    stop("Input must be an spm_result object", call. = FALSE)
  }

  n_time <- x$n_time

  if (is.null(time_axis)) {
    time_axis <- seq(0, 100, length.out = n_time)
  }

  # Determine statistic type
  if (!is.null(x$t)) {
    stat <- x$t
    stat_label <- "SPM{t}"
  } else if (!is.null(x$f)) {
    stat <- x$f
    stat_label <- "SPM{F}"
  } else {
    stop("No statistic found in result", call. = FALSE)
  }

  # Create data frame
  df <- data.frame(
    time = time_axis,
    stat = stat
  )

  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$stat)) +
    ggplot2::geom_line(linewidth = 1, color = "black") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(x = "Time (%)", y = stat_label) +
    ggplot2::theme_minimal()

  # Add threshold lines
  if (show_threshold) {
    p <- p +
      ggplot2::geom_hline(yintercept = x$threshold,
                          linetype = "dashed", color = "red") +
      ggplot2::annotate("text", x = max(time_axis) * 0.95, y = x$threshold,
                        label = sprintf("p = %.3f", x$alpha),
                        color = "red", vjust = -0.5, size = 3)

    if (!is.null(x$t) && x$two_tailed) {
      p <- p +
        ggplot2::geom_hline(yintercept = -x$threshold,
                            linetype = "dashed", color = "red")
    }
  }

  # Highlight significant clusters
  if (show_clusters && length(x$clusters) > 0) {
    for (cl in x$clusters) {
      start_time <- time_axis[cl$start]
      end_time <- time_axis[cl$end]

      p <- p +
        ggplot2::annotate("rect",
                          xmin = start_time, xmax = end_time,
                          ymin = -Inf, ymax = Inf,
                          fill = "red", alpha = 0.2)
    }
  }

  # Add title
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  } else {
    test_desc <- switch(x$test_type,
      "two-sample" = "Two-sample t-test",
      "one-sample" = "One-sample t-test",
      "paired" = "Paired t-test",
      "anova" = sprintf("ANOVA (%d groups)", length(x$groups)),
      "SPM Analysis"
    )
    p <- p + ggplot2::ggtitle(test_desc)
  }

  p
}

#' Print SPM result
#' @export
print.spm_result <- function(x, ...) {
  cat("SPM Analysis Result\n")
  cat("==================\n")
  cat(sprintf("Test type: %s\n", x$test_type))
  cat(sprintf("Time points: %d\n", x$n_time))
  cat(sprintf("Alpha: %.3f\n", x$alpha))
  cat(sprintf("Threshold: %.3f\n", x$threshold))
  cat(sprintf("FWHM (smoothness): %.2f\n", x$fwhm))

  if (length(x$clusters) > 0) {
    cat(sprintf("\nSignificant clusters: %d\n", length(x$clusters)))
    for (i in seq_along(x$clusters)) {
      cl <- x$clusters[[i]]
      cat(sprintf("  Cluster %d: [%d-%d] extent=%d, p=%.4f\n",
                  i, cl$start, cl$end, cl$extent,
                  if (!is.null(cl$p_cluster)) cl$p_cluster else NA))
    }
  } else {
    cat("\nNo significant clusters found\n")
  }

  invisible(x)
}
