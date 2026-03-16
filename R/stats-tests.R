#' Statistical Testing for PhysioExperiment
#'
#' Functions for statistical analysis of physiological signal data,
#' including t-tests, ANOVA, cluster-based permutation tests, and effect sizes.

#' Pointwise t-test across epochs
#'
#' Performs t-tests at each time point and channel, comparing epochs
#' against a baseline or between two conditions.
#'
#' @param x An epoched PhysioExperiment object (4D data).
#' @param condition1 Indices or logical vector for first condition epochs.
#' @param condition2 Indices or logical vector for second condition epochs.
#'   If NULL, performs one-sample t-test against mu.
#' @param mu Value to test against for one-sample t-test (default: 0).
#' @param paired Logical; if TRUE, performs paired t-test.
#' @param alternative Alternative hypothesis: "two.sided", "less", or "greater".
#' @param var.equal Logical; if TRUE, assumes equal variances.
#' @return A list containing:
#'   \item{t_values}{Matrix of t-statistics (time x channel)}
#'   \item{p_values}{Matrix of p-values (time x channel)}
#'   \item{df}{Degrees of freedom}
#'   \item{n1, n2}{Sample sizes for each condition}
#' @export
#' @examples
#' # Create example epoched data
#' set.seed(123)
#' epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))
#' pe <- PhysioExperiment(
#'   assays = list(epoched = epochs),
#'   samplingRate = 100
#' )
#' # One-sample t-test against zero
#' result <- tTestEpochs(pe)
#' # Two-sample t-test comparing conditions
#' result2 <- tTestEpochs(pe, condition1 = 1:10, condition2 = 11:20)
tTestEpochs <- function(x, condition1 = NULL, condition2 = NULL,
                        mu = 0, paired = FALSE,
                        alternative = c("two.sided", "less", "greater"),
                        var.equal = FALSE) {
  stopifnot(inherits(x, "PhysioExperiment"))
  alternative <- match.arg(alternative)

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for t-test. ",
         "Use epochData() first.", call. = FALSE)
  }

  n_time <- dims[1]
  n_channels <- dims[2]
  n_epochs <- dims[3]
  n_samples <- dims[4]

  # Collapse samples dimension by averaging
  if (n_samples > 1) {
    data <- apply(data, c(1, 2, 3), mean, na.rm = TRUE)
  } else {
    # Remove 4th dimension but keep as 3D array
    data <- data[, , , 1, drop = FALSE]
    dim(data) <- dims[1:3]
  }

  # Determine conditions
  if (is.null(condition1)) {
    condition1 <- seq_len(n_epochs)
  }
  if (is.logical(condition1)) {
    condition1 <- which(condition1)
  }

  # Initialize result matrices

t_values <- matrix(NA_real_, nrow = n_time, ncol = n_channels)
  p_values <- matrix(NA_real_, nrow = n_time, ncol = n_channels)

  if (is.null(condition2)) {
    # One-sample t-test (vectorized)
    data1 <- data[, , condition1, drop = FALSE]
    n1 <- length(condition1)
    n2 <- NA
    df <- n1 - 1

    # Vectorized computation across time x channel
    means <- apply(data1, c(1, 2), mean, na.rm = TRUE)
    sds <- apply(data1, c(1, 2), sd, na.rm = TRUE)
    ns <- apply(data1, c(1, 2), function(x) sum(!is.na(x)))

    t_values <- (means - mu) / (sds / sqrt(ns))
    t_values[!is.finite(t_values)] <- NA_real_

    # Vectorized p-value computation
    p_values <- .computePValueMatrix(t_values, ns - 1, alternative)

  } else {
    # Two-sample t-test (vectorized)
    if (is.logical(condition2)) {
      condition2 <- which(condition2)
    }

    data1 <- data[, , condition1, drop = FALSE]
    data2 <- data[, , condition2, drop = FALSE]
    n1 <- length(condition1)
    n2 <- length(condition2)

    if (paired) {
      if (n1 != n2) {
        stop("Paired t-test requires equal sample sizes", call. = FALSE)
      }
      df <- n1 - 1

      # Compute differences and do one-sample t-test on differences
      diffs <- data1 - data2
      means <- apply(diffs, c(1, 2), mean, na.rm = TRUE)
      sds <- apply(diffs, c(1, 2), sd, na.rm = TRUE)
      ns <- apply(diffs, c(1, 2), function(x) sum(!is.na(x)))

      t_values <- means / (sds / sqrt(ns))
      t_values[!is.finite(t_values)] <- NA_real_

      p_values <- .computePValueMatrix(t_values, ns - 1, alternative)
    } else {
      # Independent samples (vectorized Welch's t-test)
      m1 <- apply(data1, c(1, 2), mean, na.rm = TRUE)
      m2 <- apply(data2, c(1, 2), mean, na.rm = TRUE)
      s1 <- apply(data1, c(1, 2), sd, na.rm = TRUE)
      s2 <- apply(data2, c(1, 2), sd, na.rm = TRUE)
      n1_mat <- apply(data1, c(1, 2), function(x) sum(!is.na(x)))
      n2_mat <- apply(data2, c(1, 2), function(x) sum(!is.na(x)))

      if (var.equal) {
        # Pooled variance
        sp <- sqrt(((n1_mat - 1) * s1^2 + (n2_mat - 1) * s2^2) /
                     (n1_mat + n2_mat - 2))
        se <- sp * sqrt(1 / n1_mat + 1 / n2_mat)
        df_mat <- n1_mat + n2_mat - 2
      } else {
        # Welch's t-test
        se <- sqrt(s1^2 / n1_mat + s2^2 / n2_mat)
        df_mat <- (s1^2 / n1_mat + s2^2 / n2_mat)^2 /
          ((s1^2 / n1_mat)^2 / (n1_mat - 1) +
             (s2^2 / n2_mat)^2 / (n2_mat - 1))
      }

      t_values <- (m1 - m2) / se
      t_values[!is.finite(t_values)] <- NA_real_

      p_values <- .computePValueMatrix(t_values, df_mat, alternative)

      df <- if (var.equal) n1 + n2 - 2 else NA
    }
  }

  # Get time vector if available
  times <- tryCatch(epochTimes(x), error = function(e) seq_len(n_time))

  list(
    t_values = t_values,
    p_values = p_values,
    df = df,
    n1 = if (is.null(condition1)) n_epochs else length(condition1),
    n2 = if (is.null(condition2)) NA else length(condition2),
    times = times,
    alternative = alternative,
    paired = paired
  )
}

#' Compute p-value from t-statistic
#' @noRd
.computePValue <- function(t_stat, df, alternative) {
  if (!is.finite(t_stat) || !is.finite(df) || df <= 0) {
    return(NA_real_)
  }

  switch(alternative,
    two.sided = 2 * stats::pt(-abs(t_stat), df),
    less = stats::pt(t_stat, df),
    greater = stats::pt(t_stat, df, lower.tail = FALSE)
  )
}

#' Compute p-value matrix from t-statistic matrix (vectorized)
#' @noRd
.computePValueMatrix <- function(t_matrix, df_matrix, alternative) {
  p_matrix <- matrix(NA_real_, nrow = nrow(t_matrix), ncol = ncol(t_matrix))

  valid <- is.finite(t_matrix) & is.finite(df_matrix) & df_matrix > 0

  if (alternative == "two.sided") {
    p_matrix[valid] <- 2 * stats::pt(-abs(t_matrix[valid]), df_matrix[valid])
  } else if (alternative == "less") {
    p_matrix[valid] <- stats::pt(t_matrix[valid], df_matrix[valid])
  } else {
    p_matrix[valid] <- stats::pt(t_matrix[valid], df_matrix[valid], lower.tail = FALSE)
  }

  p_matrix
}

#' ANOVA across conditions
#'
#' Performs one-way ANOVA at each time point and channel across multiple conditions.
#'
#' @param x An epoched PhysioExperiment object (4D data).
#' @param groups Factor or character vector indicating group membership for each epoch.
#'   Can also be a column name from epoch_info metadata.
#' @return A list containing:
#'   \item{f_values}{Matrix of F-statistics (time x channel)}
#'   \item{p_values}{Matrix of p-values (time x channel)}
#'   \item{df_between}{Between-group degrees of freedom}
#'   \item{df_within}{Within-group degrees of freedom}
#'   \item{group_means}{Array of group means (time x channel x group)}
#' @export
#' @examples
#' # Create example epoched data with conditions
#' set.seed(123)
#' epochs <- array(rnorm(100 * 4 * 30 * 1), dim = c(100, 4, 30, 1))
#' pe <- PhysioExperiment(
#'   assays = list(epoched = epochs),
#'   samplingRate = 100,
#'   metadata = list(
#'     epoch_info = S4Vectors::DataFrame(
#'       condition = rep(c("A", "B", "C"), each = 10)
#'     )
#'   )
#' )
#' # ANOVA across conditions
#' result <- anovaEpochs(pe, groups = "condition")
anovaEpochs <- function(x, groups) {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for ANOVA. ",
         "Use epochData() first.", call. = FALSE)
  }

  n_time <- dims[1]
  n_channels <- dims[2]
  n_epochs <- dims[3]
  n_samples <- dims[4]

  # Collapse samples dimension
  if (n_samples > 1) {
    data <- apply(data, c(1, 2, 3), mean, na.rm = TRUE)
  } else {
    # Remove 4th dimension but keep as 3D array
    data <- data[, , , 1, drop = FALSE]
    dim(data) <- dims[1:3]
  }

  # Get groups
  if (is.character(groups) && length(groups) == 1) {
    # Try to get from epoch_info
    epoch_info <- S4Vectors::metadata(x)$epoch_info
    if (is.null(epoch_info) || !groups %in% names(epoch_info)) {
      stop(sprintf("Column '%s' not found in epoch_info", groups), call. = FALSE)
    }
    groups <- epoch_info[[groups]]
  }

  if (length(groups) != n_epochs) {
    stop(sprintf("Length of groups (%d) must match number of epochs (%d)",
                 length(groups), n_epochs), call. = FALSE)
  }

  groups <- as.factor(groups)
  group_levels <- levels(groups)
  n_groups <- length(group_levels)

  if (n_groups < 2) {
    stop("At least 2 groups required for ANOVA", call. = FALSE)
  }

  # Initialize results
  f_values <- matrix(NA_real_, nrow = n_time, ncol = n_channels)
  p_values <- matrix(NA_real_, nrow = n_time, ncol = n_channels)
  group_means <- array(NA_real_, dim = c(n_time, n_channels, n_groups))

  df_between <- n_groups - 1
  df_within <- n_epochs - n_groups

  for (t_idx in seq_len(n_time)) {
    for (ch in seq_len(n_channels)) {
      vals <- data[t_idx, ch, ]

      # Compute group means
      for (g in seq_len(n_groups)) {
        group_vals <- vals[groups == group_levels[g]]
        group_means[t_idx, ch, g] <- mean(group_vals, na.rm = TRUE)
      }

      # Remove NA values
      valid_idx <- !is.na(vals)
      if (sum(valid_idx) < n_groups) next

      vals_valid <- vals[valid_idx]
      groups_valid <- groups[valid_idx]

      # Compute ANOVA
      grand_mean <- mean(vals_valid)

      # Between-group sum of squares
      ss_between <- 0
      for (g in seq_len(n_groups)) {
        group_vals <- vals_valid[groups_valid == group_levels[g]]
        n_g <- length(group_vals)
        if (n_g > 0) {
          ss_between <- ss_between + n_g * (mean(group_vals) - grand_mean)^2
        }
      }

      # Within-group sum of squares
      ss_within <- 0
      for (g in seq_len(n_groups)) {
        group_vals <- vals_valid[groups_valid == group_levels[g]]
        if (length(group_vals) > 0) {
          ss_within <- ss_within + sum((group_vals - mean(group_vals))^2)
        }
      }

      # F-statistic
      df_b <- n_groups - 1
      df_w <- length(vals_valid) - n_groups

      if (df_w > 0 && ss_within > 0) {
        ms_between <- ss_between / df_b
        ms_within <- ss_within / df_w
        f_stat <- ms_between / ms_within
        f_values[t_idx, ch] <- f_stat
        p_values[t_idx, ch] <- stats::pf(f_stat, df_b, df_w, lower.tail = FALSE)
      }
    }
  }

  # Get time vector
  times <- tryCatch(epochTimes(x), error = function(e) seq_len(n_time))

  dimnames(group_means)[[3]] <- group_levels

  list(
    f_values = f_values,
    p_values = p_values,
    df_between = df_between,
    df_within = df_within,
    group_means = group_means,
    groups = group_levels,
    times = times
  )
}

#' Cluster-based permutation test
#'
#' Performs cluster-based permutation testing for multiple comparison correction.
#' Identifies clusters of significant effects and computes cluster-level p-values.
#'
#' @param x An epoched PhysioExperiment object (4D data).
#' @param condition1 Indices for first condition.
#' @param condition2 Indices for second condition. If NULL, tests against zero.
#' @param n_permutations Number of permutations (default: 1000).
#' @param cluster_threshold Initial threshold for cluster formation (p-value).
#' @param tail Test type: 0 = two-tailed, 1 = right tail, -1 = left tail.
#' @param seed Random seed for reproducibility.
#' @param n_cores Number of cores for parallel processing. Default NULL uses
#'   sequential processing. Set to parallel::detectCores() - 1 for maximum speed.
#' @return A list containing:
#'   \item{clusters}{List of identified clusters with their statistics}
#'   \item{cluster_p}{P-values for each cluster}
#'   \item{t_obs}{Observed t-values matrix}
#'   \item{cluster_mask}{Logical matrix indicating cluster membership}
#' @export
#' @examples
#' # Create example epoched data
#' set.seed(123)
#' epochs <- array(rnorm(50 * 4 * 20 * 1), dim = c(50, 4, 20, 1))
#' # Add effect to condition 2
#' epochs[20:30, 1:2, 11:20, 1] <- epochs[20:30, 1:2, 11:20, 1] + 1
#' pe <- PhysioExperiment(
#'   assays = list(epoched = epochs),
#'   samplingRate = 100
#' )
#' # Cluster permutation test
#' result <- clusterPermutationTest(pe, 1:10, 11:20, n_permutations = 100)
#'
#' # With parallel processing (faster for large datasets)
#' # result <- clusterPermutationTest(pe, 1:10, 11:20, n_cores = 4)
clusterPermutationTest <- function(x, condition1 = NULL, condition2 = NULL,
                                    n_permutations = 1000L,
                                    cluster_threshold = 0.05,
                                    tail = 0L, seed = NULL, n_cores = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (!is.null(seed)) set.seed(seed)

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for cluster test. ",
         "Use epochData() first.", call. = FALSE)
  }

  n_time <- dims[1]
  n_channels <- dims[2]
  n_epochs <- dims[3]
  n_samples <- dims[4]

  # Collapse samples
  if (n_samples > 1) {
    data <- apply(data, c(1, 2, 3), mean, na.rm = TRUE)
  } else {
    # Remove 4th dimension but keep as 3D array
    data <- data[, , , 1, drop = FALSE]
    dim(data) <- dims[1:3]
  }

  # Set conditions
  if (is.null(condition1)) {
    condition1 <- seq_len(n_epochs)
  }

  two_sample <- !is.null(condition2)

  if (two_sample) {
    n1 <- length(condition1)
    n2 <- length(condition2)
    all_idx <- c(condition1, condition2)
    n_total <- n1 + n2
  } else {
    n1 <- length(condition1)
    n_total <- n1
  }

  # Compute observed t-statistics
  t_obs <- .computeTMatrix(data, condition1, condition2, n_time, n_channels)

  # Threshold for cluster formation
  if (two_sample) {
    df_init <- n1 + n2 - 2
  } else {
    df_init <- n1 - 1
  }

  # Use one-sided threshold for one-sided tests, two-sided for two-sided
  if (tail == 0) {
    # Two-sided: use alpha/2
    t_thresh <- stats::qt(1 - cluster_threshold / 2, df_init)
  } else {
    # One-sided: use alpha (more sensitive)
    t_thresh <- stats::qt(1 - cluster_threshold, df_init)
  }

  # Find clusters in observed data
  obs_clusters <- .findClusters(t_obs, t_thresh, tail)
  obs_cluster_stats <- sapply(obs_clusters, function(cl) sum(t_obs[cl]))

  if (length(obs_cluster_stats) == 0) {
    return(list(
      clusters = list(),
      cluster_p = numeric(0),
      t_obs = t_obs,
      cluster_mask = matrix(FALSE, nrow = n_time, ncol = n_channels),
      n_permutations = n_permutations
    ))
  }

  # Permutation distribution - with optional parallel processing
  .runPermutation <- function(perm_idx, data, condition1, condition2, all_idx,
                               n1, n_total, n_time, n_channels, t_thresh, tail,
                               two_sample) {
    if (two_sample) {
      shuffled <- sample(all_idx)
      perm_cond1 <- shuffled[seq_len(n1)]
      perm_cond2 <- shuffled[(n1 + 1):n_total]
      t_perm <- .computeTMatrix(data, perm_cond1, perm_cond2, n_time, n_channels)
    } else {
      signs <- sample(c(-1, 1), n1, replace = TRUE)
      perm_data <- data
      for (i in seq_along(condition1)) {
        perm_data[, , condition1[i]] <- data[, , condition1[i]] * signs[i]
      }
      t_perm <- .computeTMatrix(perm_data, condition1, NULL, n_time, n_channels)
    }

    perm_clusters <- .findClusters(t_perm, t_thresh, tail)
    perm_stats <- sapply(perm_clusters, function(cl) abs(sum(t_perm[cl])))

    if (length(perm_stats) > 0) max(perm_stats) else 0
  }

  if (!is.null(n_cores) && n_cores > 1) {
    # Parallel processing
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Export necessary functions and data
    parallel::clusterExport(cl, c(".computeTMatrix", ".findClusters"),
                            envir = environment())

    max_cluster_stats <- parallel::parSapply(cl, seq_len(n_permutations),
      .runPermutation,
      data = data, condition1 = condition1, condition2 = condition2,
      all_idx = all_idx, n1 = n1, n_total = n_total,
      n_time = n_time, n_channels = n_channels,
      t_thresh = t_thresh, tail = tail, two_sample = two_sample
    )
  } else {
    # Sequential processing
    max_cluster_stats <- sapply(seq_len(n_permutations), .runPermutation,
      data = data, condition1 = condition1, condition2 = condition2,
      all_idx = all_idx, n1 = n1, n_total = n_total,
      n_time = n_time, n_channels = n_channels,
      t_thresh = t_thresh, tail = tail, two_sample = two_sample
    )
  }

  # Compute cluster p-values
  cluster_p <- sapply(obs_cluster_stats, function(stat) {
    mean(max_cluster_stats >= abs(stat))
  })

  # Create cluster mask
  cluster_mask <- matrix(FALSE, nrow = n_time, ncol = n_channels)
  for (i in seq_along(obs_clusters)) {
    if (cluster_p[i] < 0.05) {
      cluster_mask[obs_clusters[[i]]] <- TRUE
    }
  }

  # Get times
  times <- tryCatch(epochTimes(x), error = function(e) seq_len(n_time))

  list(
    clusters = obs_clusters,
    cluster_stats = obs_cluster_stats,
    cluster_p = cluster_p,
    t_obs = t_obs,
    cluster_mask = cluster_mask,
    n_permutations = n_permutations,
    times = times,
    permutation_distribution = max_cluster_stats
  )
}

#' Compute t-statistic matrix (vectorized)
#' @noRd
.computeTMatrix <- function(data, cond1, cond2, n_time, n_channels) {
  data1 <- data[, , cond1, drop = FALSE]
  n1 <- dim(data1)[3]

  if (is.null(cond2)) {
    # One-sample t-test against zero (vectorized)
    # Compute means and sds across epochs dimension
    means <- apply(data1, c(1, 2), mean, na.rm = TRUE)
    sds <- apply(data1, c(1, 2), sd, na.rm = TRUE)
    ns <- apply(data1, c(1, 2), function(x) sum(!is.na(x)))

    # t = mean / (sd / sqrt(n))
    t_matrix <- means / (sds / sqrt(ns))
    t_matrix[!is.finite(t_matrix)] <- NA_real_
  } else {
    # Two-sample t-test (vectorized)
    data2 <- data[, , cond2, drop = FALSE]
    n2 <- dim(data2)[3]

    m1 <- apply(data1, c(1, 2), mean, na.rm = TRUE)
    m2 <- apply(data2, c(1, 2), mean, na.rm = TRUE)
    s1 <- apply(data1, c(1, 2), sd, na.rm = TRUE)
    s2 <- apply(data2, c(1, 2), sd, na.rm = TRUE)
    n1_mat <- apply(data1, c(1, 2), function(x) sum(!is.na(x)))
    n2_mat <- apply(data2, c(1, 2), function(x) sum(!is.na(x)))

    # Welch's t-test: t = (m1 - m2) / sqrt(s1^2/n1 + s2^2/n2)
    se <- sqrt(s1^2 / n1_mat + s2^2 / n2_mat)
    t_matrix <- (m1 - m2) / se
    t_matrix[!is.finite(t_matrix)] <- NA_real_
  }

  t_matrix
}

#' Find clusters of significant values
#' @noRd
.findClusters <- function(t_matrix, threshold, tail = 0) {
  n_time <- nrow(t_matrix)
  n_channels <- ncol(t_matrix)

  # Create significance mask
  if (tail == 0) {
    sig_mask <- abs(t_matrix) > threshold
  } else if (tail == 1) {
    sig_mask <- t_matrix > threshold
  } else {
    sig_mask <- t_matrix < -threshold
  }

  sig_mask[is.na(sig_mask)] <- FALSE

  if (!any(sig_mask)) {
    return(list())
  }

  # Find connected components (clusters)
  # Using simple 4-connectivity (time and channel neighbors)
  visited <- matrix(FALSE, nrow = n_time, ncol = n_channels)
  clusters <- list()

  for (t_idx in seq_len(n_time)) {
    for (ch in seq_len(n_channels)) {
      if (sig_mask[t_idx, ch] && !visited[t_idx, ch]) {
        # BFS to find cluster
        cluster <- matrix(FALSE, nrow = n_time, ncol = n_channels)
        queue <- list(c(t_idx, ch))

        while (length(queue) > 0) {
          pos <- queue[[1]]
          queue <- queue[-1]
          t_i <- pos[1]
          ch_i <- pos[2]

          if (t_i < 1 || t_i > n_time || ch_i < 1 || ch_i > n_channels) next
          if (visited[t_i, ch_i] || !sig_mask[t_i, ch_i]) next

          visited[t_i, ch_i] <- TRUE
          cluster[t_i, ch_i] <- TRUE

          # Add neighbors (4-connectivity: time and channel)
          queue <- c(queue, list(
            c(t_i - 1, ch_i),
            c(t_i + 1, ch_i),
            c(t_i, ch_i - 1),
            c(t_i, ch_i + 1)
          ))
        }

        if (any(cluster)) {
          clusters <- c(clusters, list(which(cluster, arr.ind = FALSE)))
        }
      }
    }
  }

  clusters
}

#' Compute effect size (Cohen's d)
#'
#' Calculates Cohen's d effect size at each time point and channel.
#'
#' @param x An epoched PhysioExperiment object (4D data).
#' @param condition1 Indices for first condition.
#' @param condition2 Indices for second condition. If NULL, computes d against zero.
#' @param pooled If TRUE (default for two-sample), uses pooled standard deviation.
#' @return A list containing:
#'   \item{d}{Matrix of Cohen's d values (time x channel)}
#'   \item{ci_lower}{Lower 95% CI for d}
#'   \item{ci_upper}{Upper 95% CI for d}
#' @export
#' @examples
#' # Create example epoched data
#' set.seed(123)
#' epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))
#' pe <- PhysioExperiment(
#'   assays = list(epoched = epochs),
#'   samplingRate = 100
#' )
#' # Effect size for one-sample
#' result <- effectSize(pe, condition1 = 1:10)
#' # Effect size between conditions
#' result2 <- effectSize(pe, condition1 = 1:10, condition2 = 11:20)
effectSize <- function(x, condition1 = NULL, condition2 = NULL,
                       pooled = TRUE) {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for effect size calculation. ",
         "Use epochData() first.", call. = FALSE)
  }

  n_time <- dims[1]
  n_channels <- dims[2]
  n_epochs <- dims[3]
  n_samples <- dims[4]

  # Collapse samples
  if (n_samples > 1) {
    data <- apply(data, c(1, 2, 3), mean, na.rm = TRUE)
  } else {
    # Remove 4th dimension but keep as 3D array
    data <- data[, , , 1, drop = FALSE]
    dim(data) <- dims[1:3]
  }

  if (is.null(condition1)) {
    condition1 <- seq_len(n_epochs)
  }

  d_matrix <- matrix(NA_real_, nrow = n_time, ncol = n_channels)
  ci_lower <- matrix(NA_real_, nrow = n_time, ncol = n_channels)
  ci_upper <- matrix(NA_real_, nrow = n_time, ncol = n_channels)

  data1 <- data[, , condition1, drop = FALSE]

  if (is.null(condition2)) {
    # One-sample effect size (d = mean / sd)
    n1 <- length(condition1)

    for (t_idx in seq_len(n_time)) {
      for (ch in seq_len(n_channels)) {
        vals <- data1[t_idx, ch, ]
        vals <- vals[!is.na(vals)]

        if (length(vals) < 2) next

        m <- mean(vals)
        s <- stats::sd(vals)

        if (s > 0) {
          d <- m / s
          d_matrix[t_idx, ch] <- d

          # Approximate CI using non-central t
          se_d <- sqrt(1 / length(vals) + d^2 / (2 * length(vals)))
          ci_lower[t_idx, ch] <- d - 1.96 * se_d
          ci_upper[t_idx, ch] <- d + 1.96 * se_d
        }
      }
    }
  } else {
    # Two-sample effect size
    data2 <- data[, , condition2, drop = FALSE]
    n1 <- length(condition1)
    n2 <- length(condition2)

    for (t_idx in seq_len(n_time)) {
      for (ch in seq_len(n_channels)) {
        vals1 <- data1[t_idx, ch, ]
        vals2 <- data2[t_idx, ch, ]
        vals1 <- vals1[!is.na(vals1)]
        vals2 <- vals2[!is.na(vals2)]

        if (length(vals1) < 2 || length(vals2) < 2) next

        m1 <- mean(vals1)
        m2 <- mean(vals2)
        s1 <- stats::sd(vals1)
        s2 <- stats::sd(vals2)
        n1_actual <- length(vals1)
        n2_actual <- length(vals2)

        if (pooled) {
          # Pooled standard deviation
          sp <- sqrt(((n1_actual - 1) * s1^2 + (n2_actual - 1) * s2^2) /
                       (n1_actual + n2_actual - 2))
          d <- (m1 - m2) / sp
        } else {
          # Use control group SD (Glass's delta)
          d <- (m1 - m2) / s2
        }

        d_matrix[t_idx, ch] <- d

        # Approximate CI
        se_d <- sqrt((n1_actual + n2_actual) / (n1_actual * n2_actual) +
                       d^2 / (2 * (n1_actual + n2_actual)))
        ci_lower[t_idx, ch] <- d - 1.96 * se_d
        ci_upper[t_idx, ch] <- d + 1.96 * se_d
      }
    }
  }

  # Get times
  times <- tryCatch(epochTimes(x), error = function(e) seq_len(n_time))

  list(
    d = d_matrix,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    times = times
  )
}

#' Bootstrap confidence interval for ERP
#'
#' Computes bootstrap confidence intervals for averaged epochs.
#'
#' @param x An epoched PhysioExperiment object (4D data).
#' @param n_bootstrap Number of bootstrap iterations (default: 1000).
#' @param ci_level Confidence interval level (default: 0.95).
#' @param condition Epoch indices to include. If NULL, uses all epochs.
#' @param seed Random seed for reproducibility.
#' @return A list containing:
#'   \item{mean}{Mean across epochs (time x channel)}
#'   \item{ci_lower}{Lower CI bound (time x channel)}
#'   \item{ci_upper}{Upper CI bound (time x channel)}
#'   \item{se}{Standard error (time x channel)}
#' @export
#' @examples
#' # Create example epoched data
#' set.seed(123)
#' epochs <- array(rnorm(100 * 4 * 20 * 1), dim = c(100, 4, 20, 1))
#' pe <- PhysioExperiment(
#'   assays = list(epoched = epochs),
#'   samplingRate = 100
#' )
#' # Bootstrap CI
#' result <- bootstrapCI(pe, n_bootstrap = 500)
bootstrapCI <- function(x, n_bootstrap = 1000L, ci_level = 0.95,
                        condition = NULL, seed = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (!is.null(seed)) set.seed(seed)

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for bootstrap CI. ",
         "Use epochData() first.", call. = FALSE)
  }

  n_time <- dims[1]
  n_channels <- dims[2]
  n_epochs <- dims[3]
  n_samples <- dims[4]

  # Collapse samples
  if (n_samples > 1) {
    data <- apply(data, c(1, 2, 3), mean, na.rm = TRUE)
  } else {
    # Remove 4th dimension but keep as 3D array
    data <- data[, , , 1, drop = FALSE]
    dim(data) <- dims[1:3]
  }

  if (is.null(condition)) {
    condition <- seq_len(n_epochs)
  }

  data <- data[, , condition, drop = FALSE]
  n_epochs <- length(condition)

  # Compute observed mean
  mean_obs <- apply(data, c(1, 2), mean, na.rm = TRUE)

  # Bootstrap
  boot_means <- array(NA_real_, dim = c(n_time, n_channels, n_bootstrap))

  for (b in seq_len(n_bootstrap)) {
    boot_idx <- sample(seq_len(n_epochs), n_epochs, replace = TRUE)
    boot_data <- data[, , boot_idx, drop = FALSE]
    boot_means[, , b] <- apply(boot_data, c(1, 2), mean, na.rm = TRUE)
  }

  # Compute CI
  alpha <- 1 - ci_level
  ci_lower <- apply(boot_means, c(1, 2), stats::quantile,
                    probs = alpha / 2, na.rm = TRUE)
  ci_upper <- apply(boot_means, c(1, 2), stats::quantile,
                    probs = 1 - alpha / 2, na.rm = TRUE)
  se <- apply(boot_means, c(1, 2), stats::sd, na.rm = TRUE)

  # Get times
  times <- tryCatch(epochTimes(x), error = function(e) seq_len(n_time))

  list(
    mean = mean_obs,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    se = se,
    times = times,
    n_bootstrap = n_bootstrap,
    ci_level = ci_level
  )
}

#' Multiple comparison correction
#'
#' Applies multiple comparison correction to p-values.
#'
#' @param p_values Matrix or vector of p-values.
#' @param method Correction method: "bonferroni", "holm", "fdr" (Benjamini-Hochberg),
#'   "bh" (alias for fdr), or "none".
#' @return Corrected p-values in the same format as input.
#' @export
#' @examples
#' # Example p-values
#' p <- matrix(runif(100), nrow = 10)
#' # Apply FDR correction
#' p_corrected <- correctPValues(p, method = "fdr")
correctPValues <- function(p_values, method = c("fdr", "bonferroni", "holm", "bh", "none")) {
  method <- match.arg(method)

  if (method == "bh") method <- "fdr"
  if (method == "none") return(p_values)

  is_matrix <- is.matrix(p_values)
  original_dim <- dim(p_values)

  p_vec <- as.vector(p_values)
  valid_idx <- !is.na(p_vec)

  if (method == "bonferroni") {
    p_vec[valid_idx] <- pmin(1, p_vec[valid_idx] * sum(valid_idx))
  } else if (method == "holm") {
    p_vec[valid_idx] <- stats::p.adjust(p_vec[valid_idx], method = "holm")
  } else if (method == "fdr") {
    p_vec[valid_idx] <- stats::p.adjust(p_vec[valid_idx], method = "BH")
  }

  if (is_matrix) {
    dim(p_vec) <- original_dim
  }

  p_vec
}

#' Find significant time windows
#'
#' Identifies contiguous time periods with significant effects.
#'
#' @param p_values Vector of p-values across time.
#' @param times Vector of time points.
#' @param alpha Significance threshold (default: 0.05).
#' @param min_duration Minimum duration of significant window in time units.
#' @return A data.frame with columns: start, end, duration, min_p.
#' @export
#' @examples
#' # Example p-values
#' times <- seq(-0.2, 0.8, length.out = 100)
#' p <- c(rep(0.5, 30), rep(0.01, 20), rep(0.5, 50))
#' windows <- findSignificantWindows(p, times)
findSignificantWindows <- function(p_values, times = NULL,
                                   alpha = 0.05, min_duration = 0) {
  if (is.null(times)) {
    times <- seq_along(p_values)
  }

  if (length(p_values) != length(times)) {
    stop("Length of p_values must match length of times", call. = FALSE)
  }

  # Find significant points
  sig <- p_values < alpha
  sig[is.na(sig)] <- FALSE

  if (!any(sig)) {
    return(data.frame(
      start = numeric(0),
      end = numeric(0),
      duration = numeric(0),
      min_p = numeric(0)
    ))
  }

  # Find runs of significant values
  rle_sig <- rle(sig)

  windows <- data.frame(
    start = numeric(0),
    end = numeric(0),
    duration = numeric(0),
    min_p = numeric(0)
  )

  pos <- 1
  for (i in seq_along(rle_sig$lengths)) {
    if (rle_sig$values[i]) {
      start_idx <- pos
      end_idx <- pos + rle_sig$lengths[i] - 1

      start_time <- times[start_idx]
      end_time <- times[end_idx]
      duration <- end_time - start_time
      min_p <- min(p_values[start_idx:end_idx], na.rm = TRUE)

      if (duration >= min_duration) {
        windows <- rbind(windows, data.frame(
          start = start_time,
          end = end_time,
          duration = duration,
          min_p = min_p
        ))
      }
    }
    pos <- pos + rle_sig$lengths[i]
  }

  windows
}
