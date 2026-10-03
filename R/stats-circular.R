# Inferential circular statistics for movement phase / coordination angles.
#
# Coordination analysis (continuousRelativePhase(), vectorCoding()) produces
# ANGLES -- relative phases, coupling angles -- that live on the circle, where
# ordinary means and t-tests are wrong (359 deg and 1 deg average to 0, not 180).
# This module adds the standard circular tools (Fisher 1993; Zar 1999; Batschelet
# 1981): the mean direction and its spread, the Rayleigh test for a preferred
# direction, the Watson-Williams test for equal mean directions across groups,
# and Mardia's circular-linear correlation. Dependency-free base R; angles in
# degrees by default (what the coordination functions emit).

.circ_to_rad <- function(a, units) if (match.arg(units, c("degrees", "radians")) == "degrees") a * pi / 180 else a
.circ_from_rad <- function(a, units) if (units == "degrees") a * 180 / pi else a

# Cartesian resultant of unit vectors at angles `a` (radians): C, S, R, mean dir.
.circ_resultant <- function(a) {
  n <- length(a); C <- sum(cos(a)); S <- sum(sin(a))
  R <- sqrt(C^2 + S^2)
  list(n = n, C = C, S = S, R = R, rbar = R / n, mean = atan2(S, C))
}

# Best-Fisher (1993) MLE of the von Mises concentration kappa from mean length.
.circ_kappa <- function(rbar) {
  if (rbar < 0.53) 2 * rbar + rbar^3 + 5 * rbar^5 / 6
  else if (rbar < 0.85) -0.4 + 1.39 * rbar + 0.43 / (1 - rbar)
  else 1 / (rbar^3 - 4 * rbar^2 + 3 * rbar)
}

#' Circular mean direction and spread
#'
#' The mean direction of a set of angles and its circular spread, computed via
#' the resultant vector (so the wrap-around at 0/360 is handled correctly).
#'
#' @param angles Numeric vector of angles.
#' @param units `"degrees"` (default) or `"radians"`.
#' @return a `circular_summary` list: `mean` (mean direction), `R` (resultant
#'   length), `rbar` (mean resultant length in \[0,1]), `variance`
#'   (`1 - rbar`), `sd` (circular SD), `kappa` (von Mises concentration), all in
#'   the input units where applicable.
#' @references Zar JH (1999) Biostatistical Analysis, ch. 26-27.
#' @seealso [rayleighTest()], [circularLinearCorrelation()]
#' @export
#' @examples
#' circularSummary(c(10, 20, 350, 355))$mean        # ~ 0, not ~180
circularSummary <- function(angles, units = c("degrees", "radians")) {
  units <- match.arg(units)
  a <- .circ_to_rad(as.numeric(angles), units)
  r <- .circ_resultant(a)
  sd_rad <- sqrt(-2 * log(max(r$rbar, .Machine$double.eps)))
  structure(list(
    mean = .circ_from_rad(r$mean %% (2 * pi), units),
    R = r$R, rbar = r$rbar, variance = 1 - r$rbar,
    sd = if (units == "degrees") sd_rad * 180 / pi else sd_rad,
    kappa = .circ_kappa(r$rbar), n = r$n, units = units),
    class = "circular_summary")
}

#' @export
print.circular_summary <- function(x, ...) {
  cat(sprintf("Circular summary (n = %d, %s)\n", x$n, x$units))
  cat(sprintf("  mean direction = %.2f   mean resultant rbar = %.3f\n", x$mean, x$rbar))
  cat(sprintf("  circular SD = %.2f   kappa = %.3f\n", x$sd, x$kappa))
  invisible(x)
}

#' Rayleigh test for a preferred direction
#'
#' Tests the null hypothesis that the angles are uniformly distributed on the
#' circle (no preferred direction) against a unimodal concentration. A small
#' p-value means the angles cluster around a mean direction -- e.g. that a
#' coupling angle is consistently in-phase rather than random.
#'
#' @inheritParams circularSummary
#' @return an `htest`: Rayleigh `Z = n * rbar^2`, `p.value` (Zar 1999
#'   approximation), and the estimated mean direction.
#' @references Zar JH (1999) eq. 27.2-27.4; Fisher NI (1993).
#' @seealso [circularSummary()], [watsonWilliamsTest()]
#' @export
#' @examples
#' set.seed(1)
#' rayleighTest(rnorm(40, mean = 30, sd = 12))$p.value   # clustered -> small
rayleighTest <- function(angles, units = c("degrees", "radians")) {
  units <- match.arg(units)
  a <- .circ_to_rad(as.numeric(angles), units)
  r <- .circ_resultant(a); n <- r$n; Z <- n * r$rbar^2
  p <- exp(-Z) * (1 + (2 * Z - Z^2) / (4 * n) -
                    (24 * Z - 132 * Z^2 + 76 * Z^3 - 9 * Z^4) / (288 * n^2))
  p <- min(max(p, 0), 1)
  structure(list(
    statistic = c(Z = Z), p.value = p,
    estimate = c("mean direction" = .circ_from_rad(r$mean %% (2 * pi), units)),
    parameter = c(n = n), method = "Rayleigh test for circular uniformity",
    data.name = deparse(substitute(angles))), class = "htest")
}

#' Watson-Williams test for equal mean directions
#'
#' The circular analogue of a one-way ANOVA: tests whether two or more groups of
#' angles share a common mean direction (assuming von Mises data of similar
#' concentration). Use it to compare, e.g., coupling angles between limbs, speeds
#' or groups.
#'
#' @param angles Numeric vector of angles, or a list of per-group angle vectors.
#' @param group Grouping factor (length `length(angles)`); required when `angles`
#'   is a single vector, ignored when `angles` is a list.
#' @param units `"degrees"` (default) or `"radians"`.
#' @return an `htest`: `F` statistic (concentration-corrected), df and `p.value`.
#'   Warns when the pooled concentration is low (`rbar < 0.45`), where the test
#'   is unreliable.
#' @references Watson GS, Williams EJ (1956); Zar JH (1999) eq. 27.9.
#' @seealso [rayleighTest()]
#' @export
#' @examples
#' set.seed(2)
#' g1 <- rnorm(25, 20, 10); g2 <- rnorm(25, 60, 10)
#' watsonWilliamsTest(list(g1, g2))$p.value              # differ -> small
watsonWilliamsTest <- function(angles, group = NULL, units = c("degrees", "radians")) {
  units <- match.arg(units)
  if (is.list(angles)) {
    grps <- lapply(angles, function(v) .circ_to_rad(as.numeric(v), units))
  } else {
    if (is.null(group)) stop("`group` is required when `angles` is a vector.", call. = FALSE)
    a <- .circ_to_rad(as.numeric(angles), units)
    grps <- split(a, as.factor(group))
  }
  k <- length(grps)
  if (k < 2L) stop("need >= 2 groups.", call. = FALSE)
  res <- lapply(grps, .circ_resultant)
  N <- sum(vapply(res, `[[`, numeric(1), "n"))
  sumRj <- sum(vapply(res, `[[`, numeric(1), "R"))
  pooled <- .circ_resultant(unlist(grps))
  rbar_p <- sumRj / N                                    # weighted mean length
  if (rbar_p < 0.45)
    warning("pooled concentration low (rbar < 0.45); Watson-Williams unreliable.",
            call. = FALSE)
  kappa <- .circ_kappa(rbar_p)
  Kcorr <- 1 + 3 / (8 * kappa)                           # Zar correction factor
  Fstat <- Kcorr * (N - k) * (sumRj - pooled$R) / ((k - 1) * (N - sumRj))
  df1 <- k - 1; df2 <- N - k
  p <- stats::pf(Fstat, df1, df2, lower.tail = FALSE)
  structure(list(
    statistic = c(F = Fstat), parameter = c(df1 = df1, df2 = df2), p.value = p,
    estimate = vapply(res, function(r) .circ_from_rad(r$mean %% (2 * pi), units), numeric(1)),
    method = "Watson-Williams test for equal mean directions",
    data.name = deparse(substitute(angles))), class = "htest")
}

#' Mardia's circular-linear correlation
#'
#' The association between an angular variable (e.g. a coupling angle or relative
#' phase) and a linear one (e.g. speed, force, a clinical score). Symmetric under
#' rotation of the angle.
#'
#' @param theta Numeric angular variable.
#' @param x Numeric linear variable (same length as `theta`).
#' @param units `"degrees"` (default) or `"radians"` for `theta`.
#' @return an `htest`: the circular-linear correlation `r` (`estimate`), the test
#'   statistic `n * r^2 ~ chi-square(2)` and its `p.value`.
#' @references Mardia KV (1976); Zar JH (1999) eq. 27.19.
#' @seealso [circularSummary()]
#' @export
#' @examples
#' set.seed(3); th <- runif(60, 0, 360); y <- cos(th * pi / 180) + rnorm(60, 0, 0.3)
#' circularLinearCorrelation(th, y)$estimate            # strong association
circularLinearCorrelation <- function(theta, x, units = c("degrees", "radians")) {
  units <- match.arg(units)
  a <- .circ_to_rad(as.numeric(theta), units); x <- as.numeric(x)
  if (length(a) != length(x)) stop("`theta` and `x` must have equal length.", call. = FALSE)
  n <- length(a); C <- cos(a); S <- sin(a)
  rxc <- stats::cor(x, C); rxs <- stats::cor(x, S); rcs <- stats::cor(C, S)
  r2 <- (rxc^2 + rxs^2 - 2 * rxc * rxs * rcs) / (1 - rcs^2)
  r2 <- min(max(r2, 0), 1)
  stat <- n * r2
  p <- stats::pchisq(stat, df = 2, lower.tail = FALSE)
  structure(list(
    statistic = c("n*r^2" = stat), parameter = c(df = 2), p.value = p,
    estimate = c(r = sqrt(r2)),
    method = "Mardia circular-linear correlation",
    data.name = paste(deparse(substitute(theta)), "and", deparse(substitute(x)))),
    class = "htest")
}
