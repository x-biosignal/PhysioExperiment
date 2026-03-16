#' Advanced signal filtering functions
#'
#' This file provides advanced filtering operations including Butterworth,
#' FIR, and notch filters for physiological signal processing.

# --- Internal SOS (Second-Order Sections) helper functions ---
# SOS form avoids numerical instability that occurs with transfer function
# (ba) form for higher-order Butterworth filters (order > 8).

#' Convert transfer function to second-order sections
#'
#' Converts filter coefficients from transfer function (b, a) form
#' to cascaded second-order sections (SOS) form. SOS form is numerically
#' stable for higher filter orders.
#'
#' @param b Numerator polynomial coefficients.
#' @param a Denominator polynomial coefficients.
#' @return A matrix with one row per section and 6 columns: b0, b1, b2, a0, a1, a2.
#' @keywords internal
.tf2sos <- function(b, a) {
  # Normalize coefficients
  b <- b / a[1]
  a <- a / a[1]

  # Find zeros and poles
  zeros <- if (length(b) > 1) polyroot(rev(b)) else complex(0)
  poles <- if (length(a) > 1) polyroot(rev(a)) else complex(0)

  # Snap nearly-real roots to real (polyroot adds small imaginary artifacts)
  real_tol <- 1e-4
  zeros <- ifelse(abs(Im(zeros)) < real_tol, Re(zeros) + 0i, zeros)
  poles <- ifelse(abs(Im(poles)) < real_tol, Re(poles) + 0i, poles)

  # Pair conjugate roots
  pair_conjugates <- function(roots) {
    n <- length(roots)
    if (n == 0) return(list())
    used <- rep(FALSE, n)
    pairs <- list()

    # First pass: pair complex conjugates
    for (i in seq_len(n)) {
      if (used[i] || Im(roots[i]) == 0) next
      best_j <- NA
      best_d <- Inf
      for (j in seq_len(n)) {
        if (j != i && !used[j]) {
          d <- abs(roots[j] - Conj(roots[i]))
          if (d < best_d) { best_d <- d; best_j <- j }
        }
      }
      if (!is.na(best_j) && best_d < 0.01) {
        pairs[[length(pairs) + 1]] <- c(roots[i], roots[best_j])
        used[i] <- TRUE; used[best_j] <- TRUE
      }
    }

    # Second pass: pair remaining real roots (sort to alternate +/-)
    real_idx <- which(!used)
    if (length(real_idx) > 0) {
      real_roots <- Re(roots[real_idx])
      real_roots <- real_roots[order(real_roots)]  # sort ascending
      for (k in seq(1, length(real_roots), by = 2)) {
        if (k + 1 <= length(real_roots)) {
          pairs[[length(pairs) + 1]] <- c(real_roots[k], real_roots[k + 1])
        } else {
          pairs[[length(pairs) + 1]] <- real_roots[k]
        }
      }
    }
    pairs
  }

  zero_pairs <- pair_conjugates(zeros)
  pole_pairs <- pair_conjugates(poles)
  n_sections <- max(length(pole_pairs), 1)

  # Pad zero_pairs if fewer than pole_pairs
  while (length(zero_pairs) < n_sections) {
    zero_pairs <- c(zero_pairs, list(numeric(0)))
  }

  sos <- matrix(0, nrow = n_sections, ncol = 6)
  for (i in seq_len(n_sections)) {
    zp <- if (i <= length(zero_pairs)) zero_pairs[[i]] else numeric(0)
    pp <- if (i <= length(pole_pairs)) pole_pairs[[i]] else numeric(0)

    # Build section polynomials from roots: (z - r1)(z - r2) = z^2 - (r1+r2)z + r1*r2
    if (length(zp) == 2) {
      b_sec <- Re(c(1, -(zp[1] + zp[2]), zp[1] * zp[2]))
    } else if (length(zp) == 1) {
      b_sec <- c(1, -Re(zp[1]), 0)
    } else {
      b_sec <- c(1, 0, 0)
    }

    if (length(pp) == 2) {
      a_sec <- Re(c(1, -(pp[1] + pp[2]), pp[1] * pp[2]))
    } else if (length(pp) == 1) {
      a_sec <- c(1, -Re(pp[1]), 0)
    } else {
      a_sec <- c(1, 0, 0)
    }

    sos[i, ] <- c(b_sec, a_sec)
  }

  # Distribute overall gain: ratio of b[1]/product_of_section_b0
  section_gain <- prod(sos[, 1])
  overall_gain <- b[1] / section_gain
  sos[1, 1:3] <- sos[1, 1:3] * overall_gain
  sos
}

#' Design Butterworth filter directly in SOS form
#'
#' Avoids polynomial root-finding by computing SOS sections directly from
#' the Butterworth analog prototype poles via bilinear transform.
#' This is numerically stable for all filter orders.
#'
#' @param n Filter order.
#' @param W Normalized frequency (0-1, Nyquist = 1). Scalar for low/high,
#'   length-2 vector for pass/stop.
#' @param type Filter type: "low", "high", "pass", or "stop".
#' @return SOS matrix (n_sections x 6).
#' @keywords internal
.buttersos <- function(n, W, type = c("low", "high", "pass", "stop")) {
  type <- match.arg(type)

  # Analog Butterworth prototype poles on unit circle (left half plane)
  k <- seq(0, n - 1)
  analog_poles <- exp(1i * pi * (2 * k + n + 1) / (2 * n))

  # Pair conjugate analog poles
  .pair_analog <- function(poles) {
    np <- length(poles)
    used <- rep(FALSE, np)
    pairs <- list()
    singles <- c()
    for (i in seq_len(np)) {
      if (used[i]) next
      if (abs(Im(poles[i])) < 1e-10) {
        singles <- c(singles, poles[i])
        used[i] <- TRUE
      } else {
        for (j in seq_len(np)) {
          if (j != i && !used[j] && abs(poles[j] - Conj(poles[i])) < 1e-10) {
            pairs[[length(pairs) + 1]] <- c(poles[i], poles[j])
            used[i] <- TRUE; used[j] <- TRUE
            break
          }
        }
      }
    }
    list(pairs = pairs, singles = singles)
  }

  # Bilinear transform: s -> z
  .bilinear <- function(s) (1 + s / 2) / (1 - s / 2)

  # Build SOS section from a pair of z-domain poles and zeros
  .make_section <- function(z_poles, z_zeros, eval_freq) {
    a_sec <- Re(c(1, -(z_poles[1] + z_poles[2]), z_poles[1] * z_poles[2]))
    b_sec <- Re(c(1, -(z_zeros[1] + z_zeros[2]), z_zeros[1] * z_zeros[2]))
    # Normalize gain at eval_freq
    ejw <- exp(1i * eval_freq)
    H_num <- b_sec[1] * ejw^2 + b_sec[2] * ejw + b_sec[3]
    H_den <- a_sec[1] * ejw^2 + a_sec[2] * ejw + a_sec[3]
    sg <- abs(H_den / H_num)
    c(b_sec * sg, a_sec)
  }

  Wn <- W * pi  # Convert to radians/sample

  if (type == "low") {
    wc <- 2 * tan(Wn / 2)  # Pre-warp
    s_poles <- wc * analog_poles
    grouped <- .pair_analog(s_poles)
    sos_list <- list()
    for (pair in grouped$pairs) {
      z1 <- .bilinear(pair[1]); z2 <- .bilinear(pair[2])
      a_sec <- Re(c(1, -(z1 + z2), z1 * z2))
      b_sec <- c(1, 2, 1)  # zeros at z = -1
      sg <- sum(a_sec) / sum(b_sec)
      sos_list[[length(sos_list) + 1]] <- c(b_sec * sg, a_sec)
    }
    for (s in grouped$singles) {
      z1 <- Re(.bilinear(s))
      a_sec <- c(1, -z1, 0)
      b_sec <- c(1, 1, 0)
      sg <- sum(a_sec[1:2]) / sum(b_sec[1:2])
      sos_list[[length(sos_list) + 1]] <- c(b_sec * sg, a_sec)
    }
    sos <- do.call(rbind, sos_list)

  } else if (type == "high") {
    wc <- 2 * tan(Wn / 2)
    # Highpass transform: s_hp = wc / s_lp
    s_poles <- wc / analog_poles
    grouped <- .pair_analog(s_poles)
    sos_list <- list()
    for (pair in grouped$pairs) {
      z1 <- .bilinear(pair[1]); z2 <- .bilinear(pair[2])
      a_sec <- Re(c(1, -(z1 + z2), z1 * z2))
      b_sec <- c(1, -2, 1)  # zeros at z = +1
      sg <- abs((a_sec[1]*(-1)^2 + a_sec[2]*(-1) + a_sec[3]) /
                (b_sec[1]*(-1)^2 + b_sec[2]*(-1) + b_sec[3]))
      sos_list[[length(sos_list) + 1]] <- c(b_sec * sg, a_sec)
    }
    for (s in grouped$singles) {
      z1 <- Re(.bilinear(s))
      a_sec <- c(1, -z1, 0)
      b_sec <- c(1, -1, 0)
      sg <- abs((a_sec[1]*(-1) + a_sec[2]) / (b_sec[1]*(-1) + b_sec[2]))
      sos_list[[length(sos_list) + 1]] <- c(b_sec * sg, a_sec)
    }
    sos <- do.call(rbind, sos_list)

  } else if (type == "pass") {
    w1 <- 2 * tan(Wn[1] / 2); w2 <- 2 * tan(Wn[2] / 2)
    bw <- w2 - w1; w0 <- sqrt(w1 * w2)
    # LP -> BP: each pole maps to two poles
    bp_poles <- c()
    for (i in seq_len(n)) {
      p <- analog_poles[i]
      disc <- (bw * p)^2 - 4 * w0^2
      bp_poles <- c(bp_poles,
                    (bw * p + sqrt(disc + 0i)) / 2,
                    (bw * p - sqrt(disc + 0i)) / 2)
    }
    # Pair conjugates in z-domain
    z_poles <- .bilinear(bp_poles)
    np <- length(z_poles)
    used <- rep(FALSE, np)
    sos_list <- list()
    w_center <- (Wn[1] + Wn[2]) / 2
    for (i in seq_len(np)) {
      if (used[i]) next
      best_j <- NA; best_d <- Inf
      for (j in seq_len(np)) {
        if (j != i && !used[j]) {
          d <- abs(z_poles[j] - Conj(z_poles[i]))
          if (d < best_d) { best_d <- d; best_j <- j }
        }
      }
      if (!is.na(best_j) && best_d < 0.01) {
        sos_list[[length(sos_list) + 1]] <- .make_section(
          c(z_poles[i], z_poles[best_j]), c(1, -1), w_center)
        used[i] <- TRUE; used[best_j] <- TRUE
      }
    }
    sos <- do.call(rbind, sos_list)
    # Final gain correction at center frequency
    ejw <- exp(1i * w_center)
    H <- 1
    for (r in seq_len(nrow(sos))) {
      H <- H * (sos[r,1]*ejw^2 + sos[r,2]*ejw + sos[r,3]) /
               (sos[r,4]*ejw^2 + sos[r,5]*ejw + sos[r,6])
    }
    sos[1, 1:3] <- sos[1, 1:3] / abs(H)

  } else { # stop
    w1 <- 2 * tan(Wn[1] / 2); w2 <- 2 * tan(Wn[2] / 2)
    bw <- w2 - w1; w0 <- sqrt(w1 * w2)
    # LP -> BS: each pole maps to two poles
    bs_poles <- c()
    for (i in seq_len(n)) {
      p <- analog_poles[i]
      disc <- (bw / p)^2 - 4 * w0^2
      bs_poles <- c(bs_poles,
                    (bw / p + sqrt(disc + 0i)) / 2,
                    (bw / p - sqrt(disc + 0i)) / 2)
    }
    z_poles <- .bilinear(bs_poles)
    np <- length(z_poles)
    used <- rep(FALSE, np)
    sos_list <- list()
    # Bandstop zeros are at the center frequency: z = exp(+/-j*w_center)
    w_center <- (Wn[1] + Wn[2]) / 2
    z_zero <- exp(1i * w_center)
    for (i in seq_len(np)) {
      if (used[i]) next
      best_j <- NA; best_d <- Inf
      for (j in seq_len(np)) {
        if (j != i && !used[j]) {
          d <- abs(z_poles[j] - Conj(z_poles[i]))
          if (d < best_d) { best_d <- d; best_j <- j }
        }
      }
      if (!is.na(best_j) && best_d < 0.01) {
        a_sec <- Re(c(1, -(z_poles[i] + z_poles[best_j]),
                      z_poles[i] * z_poles[best_j]))
        b_sec <- Re(c(1, -(z_zero + Conj(z_zero)), z_zero * Conj(z_zero)))
        sg <- sum(a_sec) / sum(b_sec)
        sos_list[[length(sos_list) + 1]] <- c(b_sec * sg, a_sec)
        used[i] <- TRUE; used[best_j] <- TRUE
      }
    }
    sos <- do.call(rbind, sos_list)
  }
  sos
}

#' Apply SOS filter to a signal (single pass)
#'
#' Cascades second-order sections to filter a signal. Each section is applied
#' sequentially using \code{signal::filter}.
#'
#' @param sos SOS matrix (n_sections x 6).
#' @param x Numeric vector to filter.
#' @return Filtered numeric vector.
#' @keywords internal
.sosfilt <- function(sos, x) {
  y <- x
  for (i in seq_len(nrow(sos))) {
    b <- sos[i, 1:3]
    a <- sos[i, 4:6]
    y <- as.numeric(signal::filter(signal::Arma(b = b, a = a), y))
  }
  y
}

#' Zero-phase SOS filtering (forward-backward)
#'
#' Applies the SOS filter forward, then reverses the result and applies
#' the filter again to achieve zero-phase distortion. This doubles the
#' effective filter order.
#'
#' @param sos SOS matrix (n_sections x 6).
#' @param x Numeric vector to filter.
#' @return Filtered numeric vector with zero phase distortion.
#' @keywords internal
.sosfiltfilt <- function(sos, x) {
  n <- length(x)

  # Reflect-pad the signal to reduce edge transients
  # Use 3 * max(section order) samples for padding
  nfact <- min(3 * nrow(sos) * 2, n - 1)

  # Edge extension: reflect signal at both ends
  if (nfact > 0) {
    x_start <- 2 * x[1] - x[(nfact + 1):2]
    x_end <- 2 * x[n] - x[(n - 1):(n - nfact)]
    x_ext <- c(x_start, x, x_end)
  } else {
    x_ext <- x
  }

  # Forward pass
  y <- .sosfilt(sos, x_ext)
  # Backward pass
  y <- rev(.sosfilt(sos, rev(y)))

  # Remove padding
  if (nfact > 0) {
    y <- y[(nfact + 1):(nfact + n)]
  }
  y
}

#' Butterworth filter
#'
#' Applies a Butterworth filter (lowpass, highpass, bandpass, or bandstop)
#' along the time axis of the specified assay.
#'
#' By default, second-order sections (SOS) form is used for filtering, which
#' provides numerical stability for higher filter orders (> 8). The traditional
#' transfer function (ba) form can exhibit coefficient quantization errors at
#' high orders, leading to unstable filters. Set \code{use_sos = FALSE} to
#' revert to the legacy ba-form behavior.
#'
#' @param x A `PhysioExperiment` object.
#' @param low Lower cutoff frequency in Hz. Required for highpass and bandpass.
#' @param high Upper cutoff frequency in Hz. Required for lowpass and bandpass.
#' @param order Filter order. Default is 4.
#' @param type Filter type: "low", "high", "pass" (bandpass), or "stop" (bandstop).
#' @param use_sos Logical. If TRUE (default), use second-order sections (SOS)
#'   form for improved numerical stability, especially for high filter orders.
#'   If FALSE, use the traditional transfer function (ba) form.
#' @param output_assay Name for the output assay. Default is "filtered".
#' @return The input object with a new assay containing filtered data.
#' @export
#' @examples
#' # Create example EEG data
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000)),
#'   samplingRate = 250
#' )
#'
#' # Bandpass filter (1-40 Hz) - common for EEG
#' pe <- butterworthFilter(pe, low = 1, high = 40, type = "pass")
#'
#' # Lowpass filter (30 Hz)
#' pe <- butterworthFilter(pe, high = 30, type = "low",
#'                         output_assay = "lowpass")
#'
#' # Highpass filter (0.5 Hz) to remove DC drift
#' pe <- butterworthFilter(pe, low = 0.5, type = "high",
#'                         output_assay = "highpass")
#'
#' # High-order filter with SOS (numerically stable)
#' pe <- butterworthFilter(pe, low = 1, high = 40, type = "pass",
#'                         order = 10)
#'
#' # Legacy ba-form filtering
#' pe <- butterworthFilter(pe, low = 1, high = 40, type = "pass",
#'                         use_sos = FALSE, output_assay = "filtered_ba")
butterworthFilter <- function(x, low = NULL, high = NULL, order = 4L,
                               type = c("pass", "low", "high", "stop"),
                               use_sos = TRUE,
                               output_assay = "filtered") {
  stopifnot(inherits(x, "PhysioExperiment"))
  type <- match.arg(type)

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate is required for Butterworth filter", call. = FALSE)
  }

  nyquist <- sr / 2

  # Determine filter type and frequencies
  if (type == "low") {
    if (is.null(high)) stop("'high' frequency required for lowpass filter", call. = FALSE)
    W <- high / nyquist
    ftype <- "low"
  } else if (type == "high") {
    if (is.null(low)) stop("'low' frequency required for highpass filter", call. = FALSE)
    W <- low / nyquist
    ftype <- "high"
  } else if (type == "pass") {
    if (is.null(low) || is.null(high)) {
      stop("Both 'low' and 'high' frequencies required for bandpass filter", call. = FALSE)
    }
    W <- c(low / nyquist, high / nyquist)
    ftype <- "pass"
  } else if (type == "stop") {
    if (is.null(low) || is.null(high)) {
      stop("Both 'low' and 'high' frequencies required for bandstop filter", call. = FALSE)
    }
    W <- c(low / nyquist, high / nyquist)
    ftype <- "stop"
  }

  # Validate frequency range
  if (any(W <= 0) || any(W >= 1)) {
    stop("Filter frequencies must be between 0 and Nyquist frequency", call. = FALSE)
  }

  # Design filter
  bf <- signal::butter(n = order, W = W, type = ftype)

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available to filter", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # Build the per-channel filter function
  if (use_sos) {
    # Design SOS directly from analog prototype (avoids polynomial root-finding)
    sos <- tryCatch(
      .buttersos(n = order, W = W, type = ftype),
      error = function(e) NULL
    )
    if (is.null(sos)) {
      warning("SOS design failed; falling back to ba-form filtering",
              call. = FALSE)
      apply_butter <- function(vec) {
        signal::filtfilt(bf, vec)
      }
    } else {
      apply_butter <- function(vec) {
        .sosfiltfilt(sos, vec)
      }
    }
  } else {
    apply_butter <- function(vec) {
      signal::filtfilt(bf, vec)
    }
  }

  filtered <- data
  if (length(dims) == 1) {
    filtered[] <- apply_butter(data)
  } else {
    filtered[] <- apply(data, seq_along(dims)[-1], apply_butter)
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- filtered
  SummarizedExperiment::assays(x) <- assays
  x
}

#' FIR filter
#'
#' Applies a Finite Impulse Response (FIR) filter along the time axis.
#'
#' @param x A `PhysioExperiment` object.
#' @param low Lower cutoff frequency in Hz.
#' @param high Upper cutoff frequency in Hz.
#' @param order Filter order (number of taps - 1). Default is 100.
#' @param type Filter type: "low", "high", "pass" (bandpass), or "stop" (bandstop).
#' @param window Window function for FIR design. Default is "hamming".
#' @param output_assay Name for the output assay. Default is "filtered".
#' @return The input object with a new assay containing filtered data.
#' @export
firFilter <- function(x, low = NULL, high = NULL, order = 100L,
                      type = c("pass", "low", "high", "stop"),
                      window = "hamming", output_assay = "filtered") {
  stopifnot(inherits(x, "PhysioExperiment"))
  type <- match.arg(type)

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate is required for FIR filter", call. = FALSE)
  }

  nyquist <- sr / 2

  # Determine filter type and frequencies
  if (type == "low") {
    if (is.null(high)) stop("'high' frequency required for lowpass filter", call. = FALSE)
    W <- high / nyquist
    ftype <- "low"
  } else if (type == "high") {
    if (is.null(low)) stop("'low' frequency required for highpass filter", call. = FALSE)
    W <- low / nyquist
    ftype <- "high"
  } else if (type == "pass") {
    if (is.null(low) || is.null(high)) {
      stop("Both 'low' and 'high' frequencies required for bandpass filter", call. = FALSE)
    }
    W <- c(low / nyquist, high / nyquist)
    ftype <- "pass"
  } else if (type == "stop") {
    if (is.null(low) || is.null(high)) {
      stop("Both 'low' and 'high' frequencies required for bandstop filter", call. = FALSE)
    }
    W <- c(low / nyquist, high / nyquist)
    ftype <- "stop"
  }

  # Validate frequency range
  if (any(W <= 0) || any(W >= 1)) {
    stop("Filter frequencies must be between 0 and Nyquist frequency", call. = FALSE)
  }

  # Design FIR filter
  fir_coef <- signal::fir1(n = order, w = W, type = ftype)

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available to filter", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  apply_fir <- function(vec) {
    signal::filtfilt(fir_coef, 1, vec)
  }

  filtered <- data
  if (length(dims) == 1) {
    filtered[] <- apply_fir(data)
  } else {
    filtered[] <- apply(data, seq_along(dims)[-1], apply_fir)
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- filtered
  SummarizedExperiment::assays(x) <- assays
  x
}

#' Notch filter (power line noise removal)
#'
#' Applies a notch filter to remove power line noise (50 Hz or 60 Hz) and
#' optionally its harmonics.
#'
#' @param x A `PhysioExperiment` object.
#' @param freq Center frequency to remove in Hz. Default is 50 (European power line).
#' @param bandwidth Bandwidth of the notch in Hz. Default is 2.
#' @param harmonics Number of harmonics to remove. Default is 1 (only fundamental).
#' @param output_assay Name for the output assay. Default is "filtered".
#' @return The input object with a new assay containing filtered data.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000)),
#'   samplingRate = 250
#' )
#'
#' # Remove 50 Hz power line noise (Europe/Asia)
#' pe <- notchFilter(pe, freq = 50)
#'
#' # Remove 60 Hz and harmonics (Americas)
#' pe <- notchFilter(pe, freq = 60, harmonics = 2)
notchFilter <- function(x, freq = 50, bandwidth = 2, harmonics = 1L,
                        output_assay = "filtered") {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate is required for notch filter", call. = FALSE)
  }

  nyquist <- sr / 2

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available to filter", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  filtered <- data

  # Apply notch filter for each harmonic
  for (h in seq_len(harmonics)) {
    center_freq <- freq * h

    if (center_freq >= nyquist) {
      warning(sprintf("Harmonic %d (%.1f Hz) exceeds Nyquist frequency; skipping",
                      h, center_freq), call. = FALSE)
      next
    }

    low <- (center_freq - bandwidth / 2) / nyquist
    high <- (center_freq + bandwidth / 2) / nyquist

    if (low <= 0) low <- 0.001
    if (high >= 1) high <- 0.999

    bf <- signal::butter(n = 4, W = c(low, high), type = "stop")

    apply_notch <- function(vec) {
      signal::filtfilt(bf, vec)
    }

    if (length(dims) == 1) {
      filtered[] <- apply_notch(filtered)
    } else {
      filtered[] <- apply(filtered, seq_along(dims)[-1], apply_notch)
    }
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- filtered
  SummarizedExperiment::assays(x) <- assays
  x
}

#' Detrend signal
#'
#' Removes linear or polynomial trends from the signal.
#'
#' @param x A `PhysioExperiment` object.
#' @param type Type of detrending: "linear" or "constant" (mean removal).
#' @param output_assay Name for the output assay. Default is "detrended".
#' @return The input object with a new assay containing detrended data.
#' @export
detrendSignal <- function(x, type = c("linear", "constant"),
                          output_assay = "detrended") {
  stopifnot(inherits(x, "PhysioExperiment"))
  type <- match.arg(type)

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    stop("No assays available to detrend", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  n <- dims[1]
  t <- seq_len(n)

  detrend_vec <- function(vec) {
    if (type == "constant") {
      vec - mean(vec, na.rm = TRUE)
    } else {
      fit <- stats::lm.fit(cbind(1, t), vec)
      vec - fit$fitted.values
    }
  }

  detrended <- data
  if (length(dims) == 1) {
    detrended[] <- detrend_vec(data)
  } else {
    detrended[] <- apply(data, seq_along(dims)[-1], detrend_vec)
  }

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- detrended
  SummarizedExperiment::assays(x) <- assays
  x
}
