# Clinimetrics: clinical measurement statistics (single source of truth).
# Effect sizes, reliability indices (ICC), measurement error (SEM/MDC), and
# method agreement (Bland-Altman). Shared across the ecosystem from PhysioExperiment;
# re-exported by domain packages (e.g. PhysioMoCap) for back-compatibility.

#' Cohen's d Effect Size
#'
#' Computes Cohen's d effect size for comparing two groups or conditions,
#' with confidence intervals and qualitative interpretation.
#'
#' @param x Numeric vector for group 1 (or condition 1 if paired).
#' @param y Numeric vector for group 2 (or condition 2 if paired).
#' @param paired Logical; if TRUE, computes effect size for paired data
#'   using the SD of differences as the denominator.
#' @param pooled Logical; if TRUE (default), uses pooled SD as denominator.
#'   If FALSE, uses the SD of \code{y} (Glass's delta, treating y as control).
#'   Ignored when \code{paired = TRUE}.
#'
#' @return A list with components:
#' \describe{
#'   \item{d}{Cohen's d value}
#'   \item{ci_lower}{Lower bound of 95 percent confidence interval}
#'   \item{ci_upper}{Upper bound of 95 percent confidence interval}
#'   \item{interpretation}{Qualitative label: "negligible", "small", "medium", or "large"}
#' }
#'
#' @details
#' For independent groups with \code{pooled = TRUE}, the pooled SD is:
#' \deqn{SD_{pooled} = \sqrt{\frac{(n_1 - 1) s_1^2 + (n_2 - 1) s_2^2}{n_1 + n_2 - 2}}}
#'
#' For paired data, the denominator is the SD of the within-pair differences.
#'
#' Interpretation thresholds follow Cohen (1988):
#' \itemize{
#'   \item |d| < 0.2: negligible
#'   \item 0.2 <= |d| < 0.5: small
#'   \item 0.5 <= |d| < 0.8: medium
#'   \item |d| >= 0.8: large
#' }
#'
#' @references
#' Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences.
#' Lawrence Erlbaum Associates.
#'
#' @seealso [etaSquared()] for ANOVA-based effect sizes,
#'   `plotEffectSizeForest()` for forest plot visualization of effect sizes.
#'
#' @export
#' @examples
#' set.seed(42)
#' x <- rnorm(30, mean = 10, sd = 2)
#' y <- rnorm(30, mean = 8, sd = 2)
#' result <- cohensD(x, y)
#' result$d
#' result$interpretation
cohensD <- function(x, y, paired = FALSE, pooled = TRUE) {
  stopifnot(is.numeric(x), is.numeric(y))

if (paired) {
    stopifnot(length(x) == length(y))
    diffs <- x - y
    n <- length(diffs)
    d <- mean(diffs) / stats::sd(diffs)
  } else {
    n1 <- length(x)
    n2 <- length(y)
    mean_diff <- mean(x) - mean(y)

    if (pooled) {
      sd_denom <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) /
                         (n1 + n2 - 2))
    } else {
      # Glass's delta: use SD of y (control group)
      sd_denom <- stats::sd(y)
    }

    d <- mean_diff / sd_denom
  }

  # Confidence interval using non-central t approximation
  if (paired) {
    n <- length(x)
    se_d <- sqrt(1 / n + d^2 / (2 * n))
    df <- n - 1
  } else {
    n1 <- length(x)
    n2 <- length(y)
    se_d <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
    df <- n1 + n2 - 2
  }

  t_crit <- stats::qt(0.975, df)
  ci_lower <- d - t_crit * se_d
  ci_upper <- d + t_crit * se_d

  # Interpretation
  abs_d <- abs(d)
  interpretation <- if (abs_d < 0.2) {
    "negligible"
  } else if (abs_d < 0.5) {
    "small"
  } else if (abs_d < 0.8) {
    "medium"
  } else {
    "large"
  }

  list(
    d = d,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    interpretation = interpretation
  )
}

#' Eta-Squared Effect Size
#'
#' Computes eta-squared, partial eta-squared, and omega-squared from a
#' one-way between-subjects comparison.
#'
#' @param x Numeric vector of values.
#' @param groups Factor or character vector of group membership.
#'
#' @return A list with components:
#' \describe{
#'   \item{eta_sq}{Eta-squared (SS_between / SS_total)}
#'   \item{partial_eta_sq}{Partial eta-squared (SS_between / (SS_between + SS_within))}
#'   \item{omega_sq}{Omega-squared (bias-corrected effect size)}
#' }
#'
#' @details
#' Eta-squared is the proportion of total variance explained by group membership:
#' \deqn{\eta^2 = \frac{SS_{between}}{SS_{total}}}
#'
#' Omega-squared provides a less biased estimate:
#' \deqn{\omega^2 = \frac{SS_{between} - df_{between} \cdot MS_{within}}{SS_{total} + MS_{within}}}
#'
#' For one-way designs, partial eta-squared equals eta-squared.
#'
#' @references
#' Cohen J (1988). "Statistical Power Analysis for the Behavioral Sciences."
#' Lawrence Erlbaum Associates.
#'
#' @seealso [cohensD()] for pairwise effect sizes,
#'   `plotEffectSizeForest()` for forest plot visualization.
#'
#' @export
#' @examples
#' set.seed(42)
#' x <- c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 14, 2))
#' groups <- rep(c("A", "B", "C"), each = 20)
#' result <- etaSquared(x, groups)
#' result$eta_sq
etaSquared <- function(x, groups) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == length(groups))

  groups <- as.factor(groups)
  stopifnot(nlevels(groups) >= 2)

  N <- length(x)
  grand_mean <- mean(x)

  # Between-groups sum of squares
  group_means <- tapply(x, groups, mean)
  group_ns <- tapply(x, groups, length)
  ss_between <- sum(group_ns * (group_means - grand_mean)^2)

  # Total sum of squares
  ss_total <- sum((x - grand_mean)^2)

  # Within-groups sum of squares
  ss_within <- ss_total - ss_between

  # Degrees of freedom
  df_between <- nlevels(groups) - 1
  df_within <- N - nlevels(groups)

  # Mean squares
  ms_within <- ss_within / df_within

  # Eta-squared
  eta_sq <- ss_between / ss_total

  # Partial eta-squared (same as eta_sq for one-way design)
  partial_eta_sq <- ss_between / (ss_between + ss_within)

  # Omega-squared (bias-corrected)
  omega_sq <- (ss_between - df_between * ms_within) / (ss_total + ms_within)
  omega_sq <- max(omega_sq, 0)  # floor at 0

  list(
    eta_sq = eta_sq,
    partial_eta_sq = partial_eta_sq,
    omega_sq = omega_sq
  )
}

#' Intraclass Correlation Coefficient (ICC)
#'
#' Computes the ICC using ANOVA-based methods following Shrout & Fleiss (1979).
#' Supports one-way and two-way random/mixed models, agreement and consistency
#' types, and single or average unit measures.
#'
#' @param ratings Numeric matrix with subjects as rows and raters/sessions as columns.
#' @param model Character; ICC model type:
#'   \describe{
#'     \item{"oneway"}{One-way random effects (ICC(1,1) or ICC(1,k))}
#'     \item{"twoway"}{Two-way random/mixed effects (default)}
#'   }
#' @param type Character; agreement type:
#'   \describe{
#'     \item{"agreement"}{Absolute agreement (ICC(2,1) for twoway)}
#'     \item{"consistency"}{Consistency/relative agreement (ICC(3,1) for twoway)}
#'   }
#' @param unit Character; unit of measurement:
#'   \describe{
#'     \item{"single"}{Reliability for a single rater/measurement}
#'     \item{"average"}{Reliability for the mean of k raters/measurements}
#'   }
#'
#' @return A list with components:
#' \describe{
#'   \item{icc}{ICC value}
#'   \item{ci_lower}{Lower bound of 95 percent confidence interval}
#'   \item{ci_upper}{Upper bound of 95 percent confidence interval}
#'   \item{f_value}{F statistic from ANOVA}
#'   \item{p_value}{p-value for testing ICC = 0}
#'   \item{model}{Model used}
#'   \item{type}{Agreement type used}
#' }
#'
#' @details
#' The function implements the six ICC forms from Shrout & Fleiss (1979):
#' \itemize{
#'   \item ICC(1,1): One-way random, single measures
#'   \item ICC(1,k): One-way random, average measures
#'   \item ICC(2,1): Two-way random, absolute agreement, single measures
#'   \item ICC(2,k): Two-way random, absolute agreement, average measures
#'   \item ICC(3,1): Two-way mixed, consistency, single measures
#'   \item ICC(3,k): Two-way mixed, consistency, average measures
#' }
#'
#' @references
#' Shrout PE, Fleiss JL (1979). Intraclass correlations: Uses in assessing
#' rater reliability. Psychological Bulletin, 86(2), 420-428.
#'
#' @seealso [sem()] for standard error of measurement based on ICC,
#'   [mdc()] for minimal detectable change,
#'   [blandAltman()] for agreement analysis between methods.
#'
#' @export
#' @examples
#' # Test-retest reliability
#' ratings <- matrix(c(
#'   9, 2, 5, 8,
#'   6, 1, 3, 2,
#'   8, 4, 6, 8,
#'   7, 1, 2, 6,
#'   10, 5, 6, 9,
#'   6, 2, 4, 7
#' ), nrow = 6, ncol = 4, byrow = TRUE)
#'
#' result <- icc(ratings, model = "twoway", type = "agreement")
#' result$icc
icc <- function(ratings, model = c("twoway", "oneway"),
                type = c("agreement", "consistency"),
                unit = c("single", "average")) {

  model <- match.arg(model)
  type <- match.arg(type)
  unit <- match.arg(unit)

  stopifnot(is.matrix(ratings) || is.data.frame(ratings))
  ratings <- as.matrix(ratings)
  stopifnot(is.numeric(ratings))
  stopifnot(nrow(ratings) >= 2, ncol(ratings) >= 2)

  n <- nrow(ratings)  # number of subjects
  k <- ncol(ratings)  # number of raters/sessions

  # Compute ANOVA components
  # Subject means and rater means
  subject_means <- rowMeans(ratings)
  rater_means <- colMeans(ratings)
  grand_mean <- mean(ratings)

  # Sum of squares
  ss_total <- sum((ratings - grand_mean)^2)
  ss_rows <- k * sum((subject_means - grand_mean)^2)    # SS between subjects
  ss_cols <- n * sum((rater_means - grand_mean)^2)       # SS between raters
  ss_residual <- ss_total - ss_rows - ss_cols            # SS residual (interaction)
  ss_within <- ss_total - ss_rows                        # SS within (for one-way)

  # Degrees of freedom
  df_rows <- n - 1
  df_cols <- k - 1
  df_residual <- df_rows * df_cols
  df_within <- n * (k - 1)

  # Mean squares
  ms_rows <- ss_rows / df_rows           # BMS (between-subjects MS)
  ms_cols <- ss_cols / df_cols           # JMS (between-raters MS)
  ms_residual <- ss_residual / df_residual  # EMS (error/residual MS)
  ms_within <- ss_within / df_within     # WMS (within-subjects MS)

  # Compute ICC based on model and type
  if (model == "oneway") {
    # ICC(1,1) or ICC(1,k)
    icc_val <- (ms_rows - ms_within) / (ms_rows + (k - 1) * ms_within)

    # F test
    f_val <- ms_rows / ms_within
    p_val <- 1 - stats::pf(f_val, df_rows, df_within)

    # CI for ICC(1,1)
    fl <- f_val / stats::qf(0.975, df_rows, df_within)
    fu <- f_val / stats::qf(0.025, df_rows, df_within)
    ci_lower <- (fl - 1) / (fl + k - 1)
    ci_upper <- (fu - 1) / (fu + k - 1)

    if (unit == "average") {
      # ICC(1,k)
      icc_val <- (ms_rows - ms_within) / ms_rows
      ci_lower <- 1 - 1 / fl
      ci_upper <- 1 - 1 / fu
    }

  } else {
    # Two-way models
    if (type == "agreement") {
      # ICC(2,1) or ICC(2,k)
      icc_val <- (ms_rows - ms_residual) /
        (ms_rows + (k - 1) * ms_residual + k * (ms_cols - ms_residual) / n)

      # F test
      f_val <- ms_rows / ms_residual
      p_val <- 1 - stats::pf(f_val, df_rows, df_residual)

      # CI for ICC(2,1) - Shrout & Fleiss
      a <- k * icc_val / (n * (1 - icc_val))
      b <- 1 + k * icc_val * (n - 1) / (n * (1 - icc_val))
      v <- (a * ms_cols + b * ms_residual)^2 /
        ((a * ms_cols)^2 / df_cols + (b * ms_residual)^2 / df_residual)

      fl <- f_val / stats::qf(0.975, df_rows, v)
      fu <- f_val / stats::qf(0.025, df_rows, v)

      ci_lower <- n * (fl - 1) / (k + n * fl - k * fl)
      ci_upper <- n * (fu - 1) / (k + n * fu - k * fu)

      if (unit == "average") {
        # ICC(2,k)
        icc_val <- (ms_rows - ms_residual) /
          (ms_rows + (ms_cols - ms_residual) / n)
        ci_lower <- 1 - 1 / fl
        ci_upper <- 1 - 1 / fu
      }

    } else {
      # ICC(3,1) or ICC(3,k) - consistency
      icc_val <- (ms_rows - ms_residual) /
        (ms_rows + (k - 1) * ms_residual)

      # F test
      f_val <- ms_rows / ms_residual
      p_val <- 1 - stats::pf(f_val, df_rows, df_residual)

      # CI for ICC(3,1)
      fl <- f_val / stats::qf(0.975, df_rows, df_residual)
      fu <- f_val / stats::qf(0.025, df_rows, df_residual)
      ci_lower <- (fl - 1) / (fl + k - 1)
      ci_upper <- (fu - 1) / (fu + k - 1)

      if (unit == "average") {
        # ICC(3,k)
        icc_val <- (ms_rows - ms_residual) / ms_rows
        ci_lower <- 1 - 1 / fl
        ci_upper <- 1 - 1 / fu
      }
    }
  }

  list(
    icc = icc_val,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    f_value = f_val,
    p_value = p_val,
    model = model,
    type = type
  )
}

#' Standard Error of Measurement (SEM)
#'
#' Computes the standard error of measurement from the SD of scores and a
#' reliability coefficient (ICC or test-retest correlation).
#'
#' @param x Numeric vector of observed scores. Used to compute SD.
#' @param icc_value Numeric; ICC value to use as the reliability coefficient.
#' @param reliability Numeric; alternative reliability coefficient (e.g.,
#'   test-retest correlation). Exactly one of \code{icc_value} or
#'   \code{reliability} must be provided.
#'
#' @return Numeric SEM value.
#'
#' @details
#' SEM is computed as:
#' \deqn{SEM = SD \times \sqrt{1 - r}}
#' where \eqn{r} is the reliability coefficient (ICC or correlation).
#'
#' @references
#' Shrout PE, Fleiss JL (1979). "Intraclass Correlations: Uses in Assessing
#' Rater Reliability." Psychological Bulletin, 86(2), 420-428.
#'
#' @seealso [icc()] for computing intraclass correlation coefficients,
#'   [mdc()] for minimal detectable change based on SEM.
#'
#' @export
#' @examples
#' scores <- c(10, 12, 15, 11, 13, 14, 9, 16, 12, 11)
#' sem(scores, icc_value = 0.90)
sem <- function(x, icc_value = NULL, reliability = NULL) {
  stopifnot(is.numeric(x), length(x) >= 2)

  if (is.null(icc_value) && is.null(reliability)) {
    stop("One of 'icc_value' or 'reliability' must be provided.", call. = FALSE)
  }
  if (!is.null(icc_value) && !is.null(reliability)) {
    stop("Provide only one of 'icc_value' or 'reliability', not both.", call. = FALSE)
  }

  r <- if (!is.null(icc_value)) icc_value else reliability
  stopifnot(is.numeric(r), length(r) == 1, r >= 0, r <= 1)

  sd_x <- stats::sd(x)
  sd_x * sqrt(1 - r)
}

#' Minimal Detectable Change (MDC)
#'
#' Computes the minimal detectable change from the standard error of measurement.
#'
#' @param sem_value Numeric; the standard error of measurement.
#' @param confidence Numeric; confidence level (default 0.95).
#'
#' @return Numeric MDC value.
#'
#' @details
#' MDC is computed as:
#' \deqn{MDC = SEM \times z \times \sqrt{2}}
#' where \eqn{z = \Phi^{-1}(1 - (1 - confidence)/2)}.
#'
#' At 95\% confidence, \eqn{z = 1.96}, giving \eqn{MDC_{95} = SEM \times 1.96 \times \sqrt{2}}.
#'
#' @references
#' Shrout PE, Fleiss JL (1979). "Intraclass Correlations: Uses in Assessing
#' Rater Reliability." Psychological Bulletin, 86(2), 420-428.
#'
#' @seealso [sem()] for computing standard error of measurement,
#'   [icc()] for computing intraclass correlation coefficients.
#'
#' @export
#' @examples
#' sem_val <- 2.5
#' mdc(sem_val, confidence = 0.95)
mdc <- function(sem_value, confidence = 0.95) {
  stopifnot(is.numeric(sem_value), length(sem_value) == 1, sem_value >= 0)
  stopifnot(is.numeric(confidence), length(confidence) == 1,
            confidence > 0, confidence < 1)

  z <- stats::qnorm(1 - (1 - confidence) / 2)
  sem_value * z * sqrt(2)
}

#' Bland-Altman Analysis for Method Agreement
#'
#' Performs a Bland-Altman analysis comparing two measurement methods or
#' two time points. Computes the bias (mean difference), limits of agreement,
#' and confidence interval for the bias.
#'
#' @param x Numeric vector of measurements from method/time 1.
#' @param y Numeric vector of measurements from method/time 2.
#' @param confidence Numeric; confidence level for limits of agreement (default 0.95).
#'
#' @return A list with components:
#' \describe{
#'   \item{bias}{Mean difference (x - y)}
#'   \item{lower_loa}{Lower limit of agreement}
#'   \item{upper_loa}{Upper limit of agreement}
#'   \item{sd_diff}{Standard deviation of differences}
#'   \item{ci_bias}{Two-element vector with lower and upper CI for the bias}
#' }
#'
#' @details
#' The Bland-Altman method assesses agreement between two measurements by
#' plotting their difference against their mean. The limits of agreement are:
#' \deqn{LoA = \bar{d} \pm z \times SD_d}
#' where \eqn{\bar{d}} is the mean difference (bias) and \eqn{SD_d} is the
#' SD of differences.
#'
#' @references
#' Bland JM, Altman DG (1986). Statistical methods for assessing agreement
#' between two methods of clinical measurement. Lancet, 327(8476), 307-310.
#'
#' @seealso [icc()] for intraclass correlation reliability analysis,
#'   `benchmarkAgreement()` for benchmark validation agreement metrics, and
#'   `plotBlandAltman()` in the PhysioMoCap package to visualize the agreement.
#'
#' @export
#' @examples
#' set.seed(42)
#' method1 <- rnorm(30, mean = 50, sd = 10)
#' method2 <- method1 + rnorm(30, mean = 0, sd = 3)
#' result <- blandAltman(method1, method2)
#' result$bias
#' result$lower_loa
#' result$upper_loa
blandAltman <- function(x, y, confidence = 0.95) {
  stopifnot(is.numeric(x), is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) >= 2)
  stopifnot(is.numeric(confidence), length(confidence) == 1,
            confidence > 0, confidence < 1)

  diffs <- x - y
  n <- length(diffs)
  bias <- mean(diffs)
  sd_diff <- stats::sd(diffs)

  z <- stats::qnorm(1 - (1 - confidence) / 2)
  lower_loa <- bias - z * sd_diff
  upper_loa <- bias + z * sd_diff

  # CI for bias
  se_bias <- sd_diff / sqrt(n)
  t_crit <- stats::qt(1 - (1 - confidence) / 2, df = n - 1)
  ci_bias <- c(bias - t_crit * se_bias, bias + t_crit * se_bias)

  list(
    bias = bias,
    lower_loa = lower_loa,
    upper_loa = upper_loa,
    sd_diff = sd_diff,
    ci_bias = ci_bias
  )
}
