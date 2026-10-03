# Shared multivariate autoregressive (MVAR) core for the PhysioExperiment
# ecosystem: model fitting, order selection, and spectral factorization. This is
# the single implementation that PhysioEEG and PhysioCrossModal (both depend on
# PhysioExperiment) build DTF / PDC / spectral Granger-causality on, so they do not
# diverge into separate copies.
#
# Model:  X(t) = sum_{k=1}^{p} A_k X(t-k) + E(t),   E(t) ~ N(0, Sigma),
# with X(t) an m-vector, A_k an m x m coefficient matrix.

# ---- fitting back-ends -------------------------------------------------------

# Ordinary least squares (multivariate regression on lagged predictors).
.mvar_ols <- function(X, p) {
  n <- nrow(X); m <- ncol(X)
  Y <- X[(p + 1):n, , drop = FALSE]
  Z <- matrix(0, n - p, m * p)
  for (k in seq_len(p)) {
    Z[, ((k - 1) * m + 1):(k * m)] <- X[(p + 1 - k):(n - k), , drop = FALSE]
  }
  B <- solve(crossprod(Z), crossprod(Z, Y))            # (m*p) x m
  A <- array(0, c(m, m, p))
  for (k in seq_len(p)) A[, , k] <- t(B[((k - 1) * m + 1):(k * m), , drop = FALSE])
  resid <- Y - Z %*% B
  list(A = A, Sigma = crossprod(resid) / (n - p))
}

# Sample autocovariance Gamma(k) = (1/n) sum_t X(t) X(t-k)^T.
.mvar_autocov <- function(X, k) {
  n <- nrow(X)
  if (k >= 0) {
    crossprod(X[(k + 1):n, , drop = FALSE], X[1:(n - k), , drop = FALSE]) / n
  } else {
    t(.mvar_autocov(X, -k))
  }
}

# Yule-Walker via the block-Toeplitz normal equations.
.mvar_yw <- function(X, p) {
  m <- ncol(X)
  gam <- function(k) if (k >= 0) .mvar_autocov(X, k) else t(.mvar_autocov(X, -k))
  M <- matrix(0, m * p, m * p)
  for (k in seq_len(p)) for (l in seq_len(p)) {
    M[((k - 1) * m + 1):(k * m), ((l - 1) * m + 1):(l * m)] <- gam(l - k)
  }
  C <- matrix(0, m, m * p)
  for (l in seq_len(p)) C[, ((l - 1) * m + 1):(l * m)] <- gam(l)
  A_flat <- C %*% solve(M)
  A <- array(0, c(m, m, p))
  for (k in seq_len(p)) A[, , k] <- A_flat[, ((k - 1) * m + 1):(k * m)]
  Sigma <- gam(0)
  for (k in seq_len(p)) Sigma <- Sigma - A[, , k] %*% t(gam(k))
  list(A = A, Sigma = Sigma)
}

# Symmetric positive-definite matrix square root / inverse square root.
.mvar_msqrt <- function(S, inverse = FALSE) {
  e <- eigen((S + t(S)) / 2, symmetric = TRUE)
  v <- pmax(e$values, 1e-12)
  d <- if (inverse) 1 / sqrt(v) else sqrt(v)
  e$vectors %*% diag(d, length(d)) %*% t(e$vectors)
}

# Nuttall-Strand / Vieira-Morf multichannel lattice (geometric-mean reflection).
.mvar_ns <- function(X, p) {
  n <- nrow(X); m <- ncol(X)
  ef <- X; eb <- X                                     # order-0 fwd/bwd errors
  A <- array(0, c(m, m, p)); B <- array(0, c(m, m, p))
  for (mm in seq_len(p)) {
    fc <- ef[(mm + 1):n, , drop = FALSE]               # forward error f(t)
    bc <- eb[mm:(n - 1), , drop = FALSE]               # backward error b(t-1)
    Pf <- crossprod(fc); Pb <- crossprod(bc); D <- crossprod(fc, bc)
    rho <- .mvar_msqrt(Pf, inverse = TRUE) %*% D %*% .mvar_msqrt(Pb, inverse = TRUE)
    Kf <- .mvar_msqrt(Pf) %*% rho %*% .mvar_msqrt(Pb, inverse = TRUE)
    Kb <- .mvar_msqrt(Pb) %*% t(rho) %*% .mvar_msqrt(Pf, inverse = TRUE)
    Ao <- A; Bo <- B
    if (mm > 1) for (k in seq_len(mm - 1)) {
      A[, , k] <- Ao[, , k] - Kf %*% Bo[, , mm - k]
      B[, , k] <- Bo[, , k] - Kb %*% Ao[, , mm - k]
    }
    A[, , mm] <- Kf; B[, , mm] <- Kb
    fnew <- fc - bc %*% t(Kf)
    bnew <- bc - fc %*% t(Kb)
    ef2 <- matrix(0, n, m); eb2 <- matrix(0, n, m)
    ef2[(mm + 1):n, ] <- fnew; eb2[(mm + 1):n, ] <- bnew
    ef <- ef2; eb <- eb2
  }
  list(A = A, Sigma = crossprod(ef[(p + 1):n, , drop = FALSE]) / (n - p))
}

# ---- exported core -----------------------------------------------------------

#' Fit a multivariate autoregressive (MVAR) model
#'
#' Fits \code{X(t) = sum_{k=1}^{p} A_k X(t-k) + E(t)}, with \code{E(t)} white
#' noise of covariance \code{Sigma}, to a multichannel time series. This is the
#' shared MVAR estimator used across the ecosystem for connectivity spectra
#' (DTF, PDC, spectral Granger causality). The series is mean-centred before
#' fitting.
#'
#' @param X A numeric matrix (\code{time x channels}).
#' @param order Integer model order \code{p}.
#' @param method Estimator: \code{"ols"} (ordinary least squares, the default),
#'   \code{"yulewalker"} (block-Toeplitz normal equations), or
#'   \code{"nuttall-strand"} (Nuttall-Strand / Vieira-Morf multichannel lattice).
#' @return A list with the coefficient array \code{A} (\code{channels x channels
#'   x order}), the residual covariance \code{Sigma}, the \code{order}, and the
#'   \code{method}.
#' @references
#' Barnett, L., & Seth, A. K. (2014). The MVGC multivariate Granger causality
#' toolbox. \emph{Journal of Neuroscience Methods}, 223, 50-68.
#' \doi{10.1016/j.jneumeth.2013.10.018}
#' @seealso \code{\link{mvarFit}}, \code{\link{mvarOrderSelect}},
#'   \code{\link{.mvarSpectral}}
#' @keywords internal
#' @export
.fitMVAR <- function(X, order, method = c("ols", "yulewalker", "nuttall-strand")) {
  method <- match.arg(method)
  X <- as.matrix(X)
  stopifnot(is.numeric(X), is.numeric(order), order >= 1)
  order <- as.integer(order)
  n <- nrow(X); m <- ncol(X)
  if (n <= order * m + m) {
    stop("Too few samples for the requested order and channel count.",
         call. = FALSE)
  }
  X <- sweep(X, 2, colMeans(X), "-")                   # mean-centre
  fit <- switch(method,
    ols = .mvar_ols(X, order),
    yulewalker = .mvar_yw(X, order),
    "nuttall-strand" = .mvar_ns(X, order))
  list(A = fit$A, Sigma = fit$Sigma, order = order, method = method)
}

#' MVAR model-order selection by AIC / BIC
#'
#' Fits MVAR models over a range of orders and returns the order minimising the
#' Akaike (AIC) or Bayesian (BIC) information criterion. For an m-channel model
#' of order p with n effective samples the criteria are
#' \code{n log det(Sigma_p) + 2 m^2 p} (AIC) and
#' \code{n log det(Sigma_p) + log(n) m^2 p} (BIC).
#'
#' @param X A numeric matrix (\code{time x channels}).
#' @param max_order Maximum order to consider (default: 20).
#' @param criterion \code{"bic"} (default) or \code{"aic"}.
#' @param method Estimator passed to \code{\link{.fitMVAR}} (default "ols").
#' @return A list with the selected \code{order}, the chosen \code{criterion},
#'   and a \code{data.frame} \code{criteria} of \code{order}, \code{aic},
#'   \code{bic}.
#' @seealso \code{\link{.fitMVAR}}, \code{\link{mvarFit}}
#' @export
mvarOrderSelect <- function(X, max_order = 20L, criterion = c("bic", "aic"),
                            method = c("ols", "yulewalker", "nuttall-strand")) {
  criterion <- match.arg(criterion)
  method <- match.arg(method)
  X <- as.matrix(X)
  n <- nrow(X); m <- ncol(X)
  max_order <- min(as.integer(max_order), floor((n - m) / (m + 1)))
  orders <- seq_len(max_order)
  crit <- lapply(orders, function(q) {
    fit <- .fitMVAR(X, q, method = method)
    n_eff <- n - q
    ld <- determinant(fit$Sigma, logarithm = TRUE)$modulus
    ld <- as.numeric(ld)
    k <- m * m * q
    c(aic = n_eff * ld + 2 * k, bic = n_eff * ld + log(n_eff) * k)
  })
  crit <- do.call(rbind, crit)
  df <- data.frame(order = orders, aic = crit[, "aic"], bic = crit[, "bic"])
  best <- orders[which.min(df[[criterion]])]
  list(order = best, criterion = criterion, criteria = df)
}

#' MVAR spectral factorization (transfer function and spectral density)
#'
#' Turns fitted MVAR coefficients into the frequency-domain quantities used by
#' DTF, PDC and spectral Granger causality. At frequency \eqn{f},
#' \eqn{\bar{A}(f) = I - \sum_k A_k e^{-2\pi i f k / sr}}, the transfer function
#' is \eqn{H(f) = \bar{A}(f)^{-1}}, and the spectral density is
#' \eqn{S(f) = H(f)\,\Sigma\,H(f)^{*}}.
#'
#' @param A Coefficient array (\code{channels x channels x order}).
#' @param Sigma Residual covariance (\code{channels x channels}).
#' @param freqs Numeric vector of frequencies in Hz.
#' @param sr Sampling rate in Hz.
#' @return A list of complex arrays (\code{channels x channels x length(freqs)}):
#'   \code{H} (transfer function), \code{S} (spectral density), \code{A} (the
#'   \eqn{\bar{A}(f)} matrices), plus \code{frequencies}.
#' @seealso \code{\link{mvarTransfer}}, \code{\link{.fitMVAR}}
#' @keywords internal
#' @export
.mvarSpectral <- function(A, Sigma, freqs, sr) {
  m <- dim(A)[1]; p <- dim(A)[3]; nf <- length(freqs)
  H <- array(0i, c(m, m, nf)); S <- array(0i, c(m, m, nf)); Af <- array(0i, c(m, m, nf))
  I <- diag(m)
  for (fi in seq_len(nf)) {
    Abar <- I + 0i
    for (k in seq_len(p)) Abar <- Abar - A[, , k] * exp(-2i * pi * freqs[fi] * k / sr)
    Hf <- solve(Abar)
    Af[, , fi] <- Abar
    H[, , fi] <- Hf
    S[, , fi] <- Hf %*% Sigma %*% Conj(t(Hf))
  }
  list(H = H, S = S, A = Af, frequencies = freqs)
}

# ---- user-facing wrappers ----------------------------------------------------

#' Fit an MVAR model (user-facing)
#'
#' Convenience wrapper around \code{\link{.fitMVAR}} that optionally selects the
#' model order automatically via \code{\link{mvarOrderSelect}}.
#'
#' @param X A numeric matrix (\code{time x channels}).
#' @param order Integer order, or \code{NULL} (default) to select automatically.
#' @param method Estimator (see \code{\link{.fitMVAR}}).
#' @param max_order Maximum order for automatic selection (default: 20).
#' @param criterion Selection criterion when \code{order} is \code{NULL}
#'   (default "bic").
#' @return An object of class \code{"mvar"}: a list with \code{A}, \code{Sigma},
#'   \code{order}, \code{method}, and \code{n_channels}.
#' @seealso \code{\link{mvarTransfer}}, \code{\link{mvarOrderSelect}}
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(600), ncol = 3)
#' fit <- mvarFit(X, order = 2)
#' dim(fit$A)
mvarFit <- function(X, order = NULL, method = c("ols", "yulewalker", "nuttall-strand"),
                    max_order = 20L, criterion = c("bic", "aic")) {
  method <- match.arg(method)
  criterion <- match.arg(criterion)
  X <- as.matrix(X)
  if (is.null(order)) {
    order <- mvarOrderSelect(X, max_order = max_order, criterion = criterion,
                             method = method)$order
  }
  fit <- .fitMVAR(X, order, method = method)
  structure(list(A = fit$A, Sigma = fit$Sigma, order = fit$order,
                 method = fit$method, n_channels = ncol(X)), class = "mvar")
}

#' MVAR transfer function and spectra (user-facing)
#'
#' Convenience wrapper around \code{\link{.mvarSpectral}} for a fitted
#' \code{"mvar"} model.
#'
#' @param fit An \code{"mvar"} object from \code{\link{mvarFit}}.
#' @param freqs Numeric vector of frequencies in Hz.
#' @param sr Sampling rate in Hz.
#' @return A list as returned by \code{\link{.mvarSpectral}} (\code{H}, \code{S},
#'   \code{A}, \code{frequencies}).
#' @seealso \code{\link{mvarFit}}
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(600), ncol = 3)
#' fit <- mvarFit(X, order = 2)
#' sp <- mvarTransfer(fit, freqs = seq(1, 40, by = 1), sr = 100)
#' dim(sp$H)
mvarTransfer <- function(fit, freqs, sr) {
  stopifnot(inherits(fit, "mvar"))
  .mvarSpectral(fit$A, fit$Sigma, freqs, sr)
}
