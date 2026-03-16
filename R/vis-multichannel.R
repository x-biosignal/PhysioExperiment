#' Multi-channel visualization functions
#'
#' Functions for visualizing multiple channels simultaneously.

#' Plot multiple channels (butterfly or stacked)
#'
#' Generates a plot showing multiple channels from the signal data.
#'
#' @param x A PhysioExperiment object.
#' @param channels Integer vector of channel indices to plot. If NULL, plots all.
#' @param sample Integer index for the sample (for 3D data).
#' @param style Plot style: "butterfly" (overlaid) or "stacked" (offset).
#' @param offset Numeric offset between channels for stacked plot.
#' @param assay_name Optional assay name. If NULL, uses the default assay.
#' @param colors Optional color vector for channels.
#' @return A ggplot object.
#' @export
#' @examples
#' # Create example data
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500)),
#'   rowData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Butterfly plot (all channels overlaid)
#' plotMultiChannel(pe, style = "butterfly")
#'
#' # Stacked plot (channels offset vertically)
#' plotMultiChannel(pe, style = "stacked")
#'
#' # Plot specific channels
#' plotMultiChannel(pe, channels = c(1, 3), style = "butterfly")
plotMultiChannel <- function(x, channels = NULL, sample = 1L,
                             style = c("butterfly", "stacked"),
                             offset = NULL, assay_name = NULL, colors = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))
  style <- match.arg(style)

  if (is.null(assay_name)) {
    assay_name <- defaultAssay(x)
  }

  if (is.na(assay_name)) {
    stop("No assays available for plotting", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  ndim <- length(dims)

  # Get channel labels
  row_data <- SummarizedExperiment::rowData(x)
  if ("label" %in% names(row_data)) {
    channel_labels <- as.character(row_data$label)
  } else {
    channel_labels <- paste0("Ch", seq_len(dims[2]))
  }

  # Determine channels to plot
  if (is.null(channels)) {
    channels <- seq_len(dims[2])
  }

  if (any(channels < 1) || any(channels > dims[2])) {
    stop("Channel indices out of range", call. = FALSE)
  }

  # Extract data based on dimensionality
  if (ndim == 2) {
    plot_data <- data[, channels, drop = FALSE]
  } else if (ndim == 3) {
    sample <- as.integer(sample)
    if (sample < 1 || sample > dims[3]) {
      stop("Sample index out of range", call. = FALSE)
    }
    plot_data <- data[, channels, sample, drop = TRUE]
    if (length(channels) == 1) {
      plot_data <- matrix(plot_data, ncol = 1)
    }
  } else {
    stop("Data must be 2D or 3D", call. = FALSE)
  }

  t <- timeIndex(x)
  n_channels <- length(channels)

  # Calculate offset for stacked plot
  if (style == "stacked" && is.null(offset)) {
    offset <- max(abs(plot_data), na.rm = TRUE) * 1.5
  }

  # Build data frame for plotting
  plot_df <- data.frame(
    time = rep(t, n_channels),
    amplitude = numeric(length(t) * n_channels),
    channel = factor(rep(channel_labels[channels], each = length(t)),
                     levels = channel_labels[channels])
  )

  for (i in seq_len(n_channels)) {
    idx <- ((i - 1) * length(t) + 1):(i * length(t))
    if (style == "stacked") {
      plot_df$amplitude[idx] <- plot_data[, i] + (n_channels - i) * offset
    } else {
      plot_df$amplitude[idx] <- plot_data[, i]
    }
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = amplitude,
                                              color = channel)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time (s)", y = "Amplitude",
                  title = paste("Multi-channel signal -", style))

  if (!is.null(colors) && length(colors) >= n_channels) {
    p <- p + ggplot2::scale_color_manual(values = colors[seq_len(n_channels)])
  }

  if (style == "stacked") {
    p <- p + ggplot2::theme(legend.position = "right")
  }

  p
}

#' Plot event-related potential (ERP) waveform
#'
#' Generates an ERP plot from epoched data.
#'
#' @param x An epoched PhysioExperiment object.
#' @param channel Integer index or character name of the channel.
#' @param ci Confidence interval level (0-1). NULL for no CI.
#' @param show_epochs Logical. If TRUE, shows individual epoch traces.
#' @param epoch_alpha Alpha value for individual epoch traces.
#' @return A ggplot object.
#' @export
plotERP <- function(x, channel = 1L, ci = 0.95, show_epochs = FALSE,
                    epoch_alpha = 0.2) {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) != 4) {
    stop("Data must be 4D (epoched) for ERP plot", call. = FALSE)
  }

  # Get time vector
  times <- epochTimes(x)

  # Get channel data
  if (is.character(channel)) {
    row_data <- SummarizedExperiment::rowData(x)
    if ("label" %in% names(row_data)) {
      channel <- which(row_data$label == channel)
      if (length(channel) == 0) {
        stop("Channel not found", call. = FALSE)
      }
      channel <- channel[1]
    } else {
      stop("Channel labels not available", call. = FALSE)
    }
  }

  channel <- as.integer(channel)
  if (channel < 1 || channel > dims[2]) {
    stop("Channel index out of range", call. = FALSE)
  }

  # Extract channel data across epochs (assuming single sample)
  channel_data <- data[, channel, , 1]  # time x epochs

  # Calculate mean and CI
  erp_mean <- rowMeans(channel_data, na.rm = TRUE)

  plot_df <- data.frame(
    time = times,
    amplitude = erp_mean
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = amplitude))

  # Add individual epochs if requested
  if (show_epochs) {
    n_epochs <- ncol(channel_data)
    epoch_df <- data.frame(
      time = rep(times, n_epochs),
      amplitude = as.vector(channel_data),
      epoch = rep(seq_len(n_epochs), each = length(times))
    )
    p <- p + ggplot2::geom_line(
      data = epoch_df,
      ggplot2::aes(group = epoch),
      alpha = epoch_alpha,
      color = "gray50"
    )
  }

  # Add confidence interval
  if (!is.null(ci) && ci > 0 && ci < 1) {
    n_epochs <- ncol(channel_data)
    erp_se <- apply(channel_data, 1, stats::sd, na.rm = TRUE) / sqrt(n_epochs)
    z <- stats::qnorm((1 + ci) / 2)
    plot_df$lower <- erp_mean - z * erp_se
    plot_df$upper <- erp_mean + z * erp_se

    p <- p + ggplot2::geom_ribbon(
      data = plot_df,
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.3,
      fill = "blue"
    )
  }

  # Add mean ERP line
  p <- p +
    ggplot2::geom_line(size = 1, color = "blue") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
    ggplot2::labs(x = "Time (s)", y = "Amplitude", title = "Event-Related Potential")

  p
}

#' Plot power spectral density
#'
#' Generates a PSD plot for specified channels.
#'
#' @param x A PhysioExperiment object.
#' @param channels Integer vector of channel indices. If NULL, plots all.
#' @param sample Integer index for the sample (for 3D data).
#' @param log_scale Logical. If TRUE, uses log scale for power.
#' @param freq_range Numeric vector of length 2 specifying frequency range.
#' @return A ggplot object.
#' @export
plotPSD <- function(x, channels = NULL, sample = 1L, log_scale = TRUE,
                    freq_range = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate required for PSD plot", call. = FALSE)
  }

  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)
  ndim <- length(dims)

  # Get channel labels
  row_data <- SummarizedExperiment::rowData(x)
  if ("label" %in% names(row_data)) {
    channel_labels <- as.character(row_data$label)
  } else {
    channel_labels <- paste0("Ch", seq_len(dims[2]))
  }

  # Determine channels
  if (is.null(channels)) {
    channels <- seq_len(min(dims[2], 5))  # Limit to 5 by default
  }

  # Extract data
  if (ndim == 2) {
    signal_data <- data[, channels, drop = FALSE]
  } else if (ndim == 3) {
    signal_data <- data[, channels, sample, drop = TRUE]
    if (length(channels) == 1) {
      signal_data <- matrix(signal_data, ncol = 1)
    }
  } else {
    stop("Data must be 2D or 3D", call. = FALSE)
  }

  n <- nrow(signal_data)
  freqs <- seq(0, sr / 2, length.out = floor(n / 2) + 1)

  # Compute PSD for each channel
  plot_list <- list()

  for (i in seq_along(channels)) {
    sig <- signal_data[, i]
    fft_result <- stats::fft(sig)
    psd <- (Mod(fft_result[1:(floor(n / 2) + 1)])^2) / n

    plot_list[[i]] <- data.frame(
      freq = freqs,
      power = psd,
      channel = channel_labels[channels[i]]
    )
  }

  plot_df <- do.call(rbind, plot_list)
  plot_df$channel <- factor(plot_df$channel, levels = channel_labels[channels])

  # Apply frequency range filter
  if (!is.null(freq_range)) {
    plot_df <- plot_df[plot_df$freq >= freq_range[1] &
                         plot_df$freq <= freq_range[2], ]
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = freq, y = power,
                                              color = channel)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Frequency (Hz)", y = "Power", title = "Power Spectral Density")

  if (log_scale) {
    p <- p + ggplot2::scale_y_log10()
  }

  p
}

