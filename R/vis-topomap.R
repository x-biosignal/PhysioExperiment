#' Topographic Map Visualization
#'
#' Functions for plotting scalp topography maps showing the spatial
#' distribution of signal values across electrode positions.

#' Plot topographic map (scalp topography)
#'
#' Creates a 2D topographic map showing the spatial distribution of values
#' across electrode positions on the scalp.
#'
#' @param x A PhysioExperiment object with electrode positions.
#' @param values Optional numeric vector of values to plot. If NULL, uses
#'   values from the specified time point.
#' @param time Time point in seconds to extract values (if values is NULL).
#' @param channel_values Named vector of channel values (alternative to values).
#' @param assay_name Optional assay name. If NULL, uses the default assay.
#' @param resolution Grid resolution for interpolation. Default is 100.
#' @param contours Logical. If TRUE, adds contour lines. Default is TRUE
#' @param head_shape Logical. If TRUE, draws head outline. Default is TRUE.
#' @param electrodes Logical. If TRUE, shows electrode positions. Default is TRUE.
#' @param palette Color palette name or vector of colors.
#' @param limits Numeric vector of length 2 for color scale limits.
#' @param title Plot title. If NULL, auto-generated.
#' @return A ggplot object.
#' @export
#' @examples
#' # Create example with 10-20 electrode positions
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   rowData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Apply 10-20 montage to get electrode positions
#' pe <- applyMontage(pe, "10-20")
#'
#' # Plot topographic map at time = 0.5s
#' plotTopomap(pe, time = 0.5)
#'
#' # Plot with custom values
#' plotTopomap(pe, values = c(1, 0.5, -0.5, -1))
plotTopomap <- function(x, values = NULL, time = NULL, channel_values = NULL,
                        assay_name = NULL, resolution = 100L,
                        contours = TRUE, head_shape = TRUE, electrodes = TRUE,
                        palette = "RdBu", limits = NULL, title = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  # Get electrode positions
  positions <- getElectrodePositions(x)

  if (is.null(positions)) {
    stop("Electrode positions not set. Use setElectrodePositions() or applyMontage() first.",
         call. = FALSE)
  }

  # Remove electrodes without positions
  valid_idx <- !is.na(positions$x) & !is.na(positions$y)
  if (sum(valid_idx) < 3) {
    stop("At least 3 electrodes with valid positions are required", call. = FALSE)
  }

  positions <- positions[valid_idx, ]
  n_electrodes <- nrow(positions)

  # Get values to plot
  if (!is.null(values)) {
    if (length(values) != n_electrodes) {
      stop("Length of values must match number of electrodes with positions", call. = FALSE)
    }
    plot_values <- values
  } else if (!is.null(channel_values)) {
    ch_names <- positions$channel
    if (!all(ch_names %in% names(channel_values))) {
      stop("channel_values must contain values for all electrodes with positions", call. = FALSE)
    }
    plot_values <- channel_values[ch_names]
  } else {
    # Extract values from data at specified time
    if (is.null(assay_name)) {
      assay_name <- defaultAssay(x)
    }
    data <- SummarizedExperiment::assay(x, assay_name)
    dims <- dim(data)

    # Get time index
    if (is.null(time)) {
      time_idx <- 1L
    } else {
      sr <- samplingRate(x)
      if (is.na(sr) || sr <= 0) {
        stop("Valid sampling rate required", call. = FALSE)
      }
      time_idx <- max(1L, min(dims[1], as.integer(round(time * sr)) + 1L))
    }

    # Extract values for the valid channels
    ch_indices <- which(valid_idx)
    if (length(dims) == 2) {
      plot_values <- data[time_idx, ch_indices]
    } else if (length(dims) >= 3) {
      plot_values <- data[time_idx, ch_indices, 1]
    }
  }

  # Convert 3D to 2D projection (simple azimuthal equidistant)
  # Using x, y coordinates directly (assuming already in 2D head space)
  pos_x <- positions$x
  pos_y <- positions$y

  # Create interpolation grid
  x_range <- range(pos_x, na.rm = TRUE)
  y_range <- range(pos_y, na.rm = TRUE)
  margin <- 0.2 * max(diff(x_range), diff(y_range))

  grid_x <- seq(x_range[1] - margin, x_range[2] + margin, length.out = resolution)
  grid_y <- seq(y_range[1] - margin, y_range[2] + margin, length.out = resolution)
  grid <- expand.grid(x = grid_x, y = grid_y)

  # Interpolate values using inverse distance weighting
  grid$value <- .interpolateIDW(pos_x, pos_y, plot_values, grid$x, grid$y)

  # Create circular mask for head
  head_center <- c(mean(x_range), mean(y_range))
  head_radius <- max(diff(x_range), diff(y_range)) / 2 + margin * 0.5
  grid$distance <- sqrt((grid$x - head_center[1])^2 + (grid$y - head_center[2])^2)
  grid$value[grid$distance > head_radius] <- NA

  # Build plot
  p <- ggplot2::ggplot(grid, ggplot2::aes(x = x, y = y, fill = value))

  # Add interpolated surface
  p <- p + ggplot2::geom_raster(interpolate = TRUE)

  # Add contour lines
  if (contours) {
    p <- p + ggplot2::geom_contour(ggplot2::aes(z = value), color = "black",
                                    alpha = 0.5, bins = 10)
  }

  # Add head shape
  if (head_shape) {
    # Create head outline
    theta <- seq(0, 2 * pi, length.out = 100)
    head_outline <- data.frame(
      x = head_center[1] + head_radius * cos(theta),
      y = head_center[2] + head_radius * sin(theta)
    )

    # Nose
    nose_tip <- head_center[2] + head_radius * 1.1
    nose <- data.frame(
      x = c(head_center[1] - 0.1 * head_radius, head_center[1],
            head_center[1] + 0.1 * head_radius),
      y = c(head_center[2] + head_radius, nose_tip,
            head_center[2] + head_radius)
    )

    # Ears
    ear_left <- data.frame(
      x = c(head_center[1] - head_radius, head_center[1] - head_radius * 1.1,
            head_center[1] - head_radius),
      y = c(head_center[2] + 0.1 * head_radius, head_center[2],
            head_center[2] - 0.1 * head_radius)
    )
    ear_right <- data.frame(
      x = c(head_center[1] + head_radius, head_center[1] + head_radius * 1.1,
            head_center[1] + head_radius),
      y = c(head_center[2] + 0.1 * head_radius, head_center[2],
            head_center[2] - 0.1 * head_radius)
    )

    p <- p +
      ggplot2::geom_path(data = head_outline, ggplot2::aes(x = x, y = y),
                         inherit.aes = FALSE, linewidth = 1) +
      ggplot2::geom_path(data = nose, ggplot2::aes(x = x, y = y),
                         inherit.aes = FALSE, linewidth = 1) +
      ggplot2::geom_path(data = ear_left, ggplot2::aes(x = x, y = y),
                         inherit.aes = FALSE, linewidth = 1) +
      ggplot2::geom_path(data = ear_right, ggplot2::aes(x = x, y = y),
                         inherit.aes = FALSE, linewidth = 1)
  }

  # Add electrode positions
  if (electrodes) {
    electrode_df <- data.frame(
      x = pos_x,
      y = pos_y,
      label = positions$channel
    )

    p <- p +
      ggplot2::geom_point(data = electrode_df, ggplot2::aes(x = x, y = y),
                          inherit.aes = FALSE, size = 2, shape = 21, fill = "white") +
      ggplot2::geom_text(data = electrode_df, ggplot2::aes(x = x, y = y, label = label),
                         inherit.aes = FALSE, size = 2.5, vjust = -1)
  }

  # Set color scale
  if (!is.null(limits)) {
    p <- p + ggplot2::scale_fill_distiller(
      palette = palette, direction = -1,
      limits = limits, na.value = "transparent"
    )
  } else {
    # Symmetric limits for diverging scale
    max_abs <- max(abs(plot_values), na.rm = TRUE)
    p <- p + ggplot2::scale_fill_distiller(
      palette = palette, direction = -1,
      limits = c(-max_abs, max_abs), na.value = "transparent"
    )
  }

  # Final styling
  plot_title <- if (!is.null(title)) {
    title
  } else if (!is.null(time)) {
    sprintf("Topographic Map (t = %.3f s)", time)
  } else {
    "Topographic Map"
  }

  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = plot_title, fill = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  p
}

#' Inverse distance weighting interpolation
#' @noRd
.interpolateIDW <- function(x, y, values, xi, yi, power = 2) {
  n <- length(xi)
  result <- numeric(n)

  for (i in seq_len(n)) {
    distances <- sqrt((x - xi[i])^2 + (y - yi[i])^2)

    # Handle exact matches
    if (any(distances < 1e-10)) {
      result[i] <- values[which.min(distances)]
    } else {
      weights <- 1 / (distances^power)
      result[i] <- sum(weights * values) / sum(weights)
    }
  }

  result
}

#' Plot topographic map animation
#'
#' Creates a series of topographic maps across time.
#'
#' @param x A PhysioExperiment object with electrode positions.
#' @param times Numeric vector of time points to plot.
#' @param ... Additional arguments passed to plotTopomap.
#' @return A list of ggplot objects.
#' @export
#' @examples
#' \dontrun{
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   rowData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#' pe <- applyMontage(pe, "10-20")
#'
#' # Create topomaps at multiple time points
#' plots <- plotTopomapSeries(pe, times = c(0.1, 0.2, 0.3, 0.4))
#' }
plotTopomapSeries <- function(x, times, ...) {
  stopifnot(inherits(x, "PhysioExperiment"))

  plots <- lapply(times, function(t) {
    plotTopomap(x, time = t, title = sprintf("t = %.3f s", t), ...)
  })

  plots
}
