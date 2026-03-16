#' Network Visualization for PhysioExperiment
#'
#' Functions for visualizing functional connectivity networks including
#' network graphs, adjacency matrices, and network metrics.

#' Plot network graph
#'
#' Creates a network graph visualization with nodes and edges.
#'
#' @param adjacency An adjacency matrix or connectivity result.
#' @param node_names Optional vector of node names.
#' @param node_size Node size. Can be "degree", "betweenness", "eigenvector", or numeric.
#' @param edge_threshold Minimum edge weight to display.
#' @param layout Layout algorithm: "circle", "spring", or "grid".
#' @param node_color Node color or vector of colors.
#' @param edge_color Edge color.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(123)
#' adj <- matrix(runif(16), 4, 4)
#' adj <- (adj + t(adj)) / 2
#' diag(adj) <- 0
#' plotNetwork(adj, node_names = c("Fz", "Cz", "Pz", "Oz"))
plotNetwork <- function(adjacency, node_names = NULL,
                         node_size = "degree", edge_threshold = 0,
                         layout = c("circle", "spring", "grid"),
                         node_color = "#3498db", edge_color = "#7f8c8d",
                         title = "Network Graph") {
  layout <- match.arg(layout)

  # Handle list input
  if (is.list(adjacency)) {
    adjacency <- adjacencyMatrix(adjacency)
  }

  n <- nrow(adjacency)
  diag(adjacency) <- 0

  # Node names
  if (is.null(node_names)) {
    node_names <- paste0("N", seq_len(n))
  }

  # Calculate node positions based on layout
  if (layout == "circle") {
    angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    x <- cos(angles)
    y <- sin(angles)
  } else if (layout == "spring") {
    # Force-directed layout (simplified Fruchterman-Reingold)
    coords <- .springLayout(adjacency)
    x <- coords[, 1]
    y <- coords[, 2]
  } else if (layout == "grid") {
    ncol_grid <- ceiling(sqrt(n))
    x <- ((seq_len(n) - 1) %% ncol_grid) - ncol_grid / 2
    y <- -((seq_len(n) - 1) %/% ncol_grid) + ncol_grid / 2
  }

  # Calculate node sizes
  if (is.character(node_size)) {
    if (node_size == "degree") {
      sizes <- nodeDegree(adjacency, weighted = TRUE)
    } else if (node_size == "betweenness") {
      sizes <- betweennessCentrality(adjacency)
    } else if (node_size == "eigenvector") {
      sizes <- eigenvectorCentrality(adjacency)
    } else {
      sizes <- rep(1, n)
    }
    # Normalize sizes
    sizes <- 3 + 7 * (sizes - min(sizes)) / (max(sizes) - min(sizes) + 1e-10)
  } else {
    sizes <- rep(node_size, n)
  }

  # Create edge data
  edges <- which(abs(adjacency) > edge_threshold & row(adjacency) < col(adjacency),
                 arr.ind = TRUE)

  if (nrow(edges) > 0) {
    edge_df <- data.frame(
      x = x[edges[, 1]],
      y = y[edges[, 1]],
      xend = x[edges[, 2]],
      yend = y[edges[, 2]],
      weight = adjacency[edges]
    )
  } else {
    edge_df <- data.frame(x = numeric(0), y = numeric(0),
                          xend = numeric(0), yend = numeric(0),
                          weight = numeric(0))
  }

  # Create node data
  node_df <- data.frame(
    x = x,
    y = y,
    name = node_names,
    size = sizes
  )

  # Build plot
  p <- ggplot2::ggplot()

  # Add edges
  if (nrow(edge_df) > 0) {
    max_weight <- max(abs(edge_df$weight))
    p <- p + ggplot2::geom_segment(
      data = edge_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                   alpha = abs(weight) / max_weight),
      color = edge_color, linewidth = 0.5
    )
  }

  # Add nodes
  p <- p + ggplot2::geom_point(
    data = node_df,
    ggplot2::aes(x = x, y = y, size = size),
    color = node_color
  )

  # Add labels
  p <- p + ggplot2::geom_text(
    data = node_df,
    ggplot2::aes(x = x, y = y, label = name),
    vjust = -1.5, size = 3
  )

  # Styling
  p <- p +
    ggplot2::scale_size_identity() +
    ggplot2::scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    )

  p
}

#' Spring layout algorithm
#' @noRd
.springLayout <- function(adjacency, iterations = 50) {
  n <- nrow(adjacency)

  # Initialize positions randomly
  set.seed(42)
  pos <- matrix(runif(n * 2, -1, 1), ncol = 2)

  k <- sqrt(4 / n)  # Optimal distance

  for (iter in seq_len(iterations)) {
    disp <- matrix(0, n, 2)

    # Repulsive forces between all pairs
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i != j) {
          delta <- pos[i, ] - pos[j, ]
          dist <- sqrt(sum(delta^2)) + 0.01
          disp[i, ] <- disp[i, ] + (delta / dist) * (k^2 / dist)
        }
      }
    }

    # Attractive forces for connected nodes
    edges <- which(adjacency != 0, arr.ind = TRUE)
    for (e in seq_len(nrow(edges))) {
      i <- edges[e, 1]
      j <- edges[e, 2]
      delta <- pos[i, ] - pos[j, ]
      dist <- sqrt(sum(delta^2)) + 0.01
      force <- (delta / dist) * (dist^2 / k)
      disp[i, ] <- disp[i, ] - force
      disp[j, ] <- disp[j, ] + force
    }

    # Apply displacement with cooling
    temp <- 1 - iter / iterations
    for (i in seq_len(n)) {
      disp_len <- sqrt(sum(disp[i, ]^2)) + 0.01
      pos[i, ] <- pos[i, ] + (disp[i, ] / disp_len) * min(disp_len, temp)
    }
  }

  # Normalize to [-1, 1]
  pos[, 1] <- (pos[, 1] - min(pos[, 1])) / (max(pos[, 1]) - min(pos[, 1]) + 1e-10) * 2 - 1
  pos[, 2] <- (pos[, 2] - min(pos[, 2])) / (max(pos[, 2]) - min(pos[, 2]) + 1e-10) * 2 - 1

  pos
}

#' Plot adjacency matrix heatmap
#'
#' Creates a heatmap visualization of an adjacency or connectivity matrix.
#'
#' @param adjacency An adjacency matrix or connectivity result.
#' @param node_names Optional vector of node names.
#' @param symmetric If TRUE, only shows lower triangle.
#' @param color_palette Color palette: "default", "viridis", "heat", or custom.
#' @param show_values If TRUE, displays values in cells.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(123)
#' adj <- matrix(runif(16), 4, 4)
#' adj <- (adj + t(adj)) / 2
#' diag(adj) <- 1
#' plotAdjacencyMatrix(adj, node_names = c("Fz", "Cz", "Pz", "Oz"))
plotAdjacencyMatrix <- function(adjacency, node_names = NULL,
                                  symmetric = TRUE, color_palette = "default",
                                  show_values = FALSE, title = "Connectivity Matrix") {
  # Handle list input
  if (is.list(adjacency)) {
    if ("matrix" %in% names(adjacency)) {
      adjacency <- adjacency$matrix
    } else if ("correlation" %in% names(adjacency)) {
      adjacency <- adjacency$correlation
    } else if ("coherence" %in% names(adjacency)) {
      adjacency <- apply(adjacency$coherence, c(2, 3), mean)
    }
  }

  n <- nrow(adjacency)

  if (is.null(node_names)) {
    node_names <- paste0("N", seq_len(n))
  }

  # Create data frame for plotting
  mat_df <- expand.grid(row = seq_len(n), col = seq_len(n))
  mat_df$value <- as.vector(adjacency)
  mat_df$row_name <- factor(node_names[mat_df$row], levels = rev(node_names))
  mat_df$col_name <- factor(node_names[mat_df$col], levels = node_names)

  # Mask upper triangle if symmetric
  if (symmetric) {
    mat_df$value[mat_df$row < mat_df$col] <- NA
  }

  # Build plot
  p <- ggplot2::ggplot(mat_df, ggplot2::aes(x = col_name, y = row_name, fill = value))

  p <- p + ggplot2::geom_tile(color = "white", linewidth = 0.5)

  # Color scale
  if (color_palette == "default") {
    p <- p + ggplot2::scale_fill_gradient2(
      low = "#3498db", mid = "white", high = "#e74c3c",
      midpoint = 0, na.value = "grey90",
      name = "Value"
    )
  } else if (color_palette == "viridis") {
    p <- p + ggplot2::scale_fill_viridis_c(na.value = "grey90", name = "Value")
  } else if (color_palette == "heat") {
    p <- p + ggplot2::scale_fill_gradient(
      low = "white", high = "#e74c3c",
      na.value = "grey90", name = "Value"
    )
  }

  # Add values if requested
  if (show_values) {
    mat_df_text <- mat_df[!is.na(mat_df$value), ]
    p <- p + ggplot2::geom_text(
      data = mat_df_text,
      ggplot2::aes(label = sprintf("%.2f", value)),
      size = 3
    )
  }

  # Styling
  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      panel.grid = ggplot2::element_blank()
    )

  p
}

#' Plot network metrics
#'
#' Creates a bar plot of network metrics for each node.
#'
#' @param adjacency An adjacency matrix.
#' @param metrics Vector of metrics to compute: "degree", "clustering",
#'   "betweenness", "eigenvector", "local_efficiency".
#' @param node_names Optional vector of node names.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0), 4, 4)
#' plotNetworkMetrics(adj, node_names = c("Fz", "Cz", "Pz", "Oz"))
plotNetworkMetrics <- function(adjacency,
                                 metrics = c("degree", "clustering", "betweenness"),
                                 node_names = NULL, title = "Network Metrics") {
  # Handle list input
  if (is.list(adjacency)) {
    adjacency <- adjacencyMatrix(adjacency)
  }

  n <- nrow(adjacency)
  diag(adjacency) <- 0

  if (is.null(node_names)) {
    node_names <- paste0("N", seq_len(n))
  }

  # Compute metrics
  metric_data <- data.frame(node = rep(node_names, length(metrics)))
  metric_data$metric <- rep(metrics, each = n)
  metric_data$value <- NA

  for (m in metrics) {
    if (m == "degree") {
      vals <- nodeDegree(adjacency, weighted = TRUE)
    } else if (m == "clustering") {
      vals <- clusteringCoefficient(adjacency)
    } else if (m == "betweenness") {
      vals <- betweennessCentrality(adjacency)
    } else if (m == "eigenvector") {
      vals <- eigenvectorCentrality(adjacency)
    } else if (m == "local_efficiency") {
      vals <- localEfficiency(adjacency)
    } else {
      vals <- rep(0, n)
    }

    metric_data$value[metric_data$metric == m] <- vals
  }

  metric_data$node <- factor(metric_data$node, levels = node_names)
  metric_data$metric <- factor(metric_data$metric, levels = metrics)

  # Build plot
  p <- ggplot2::ggplot(metric_data,
                        ggplot2::aes(x = node, y = value, fill = metric)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_brewer(palette = "Set2", name = "Metric") +
    ggplot2::labs(x = "Node", y = "Value", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    )

  p
}

#' Plot dynamic connectivity
#'
#' Creates a visualization of time-varying connectivity.
#'
#' @param dyn_conn Result from slidingWindowConnectivity().
#' @param node_pair Vector of two node indices to plot, or "mean" for average.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)),
#'   samplingRate = 100
#' )
#' dyn_conn <- slidingWindowConnectivity(pe, window_size = 200, step = 50)
#' plotDynamicConnectivity(dyn_conn, node_pair = c(1, 2))
plotDynamicConnectivity <- function(dyn_conn, node_pair = "mean",
                                      title = "Dynamic Connectivity") {
  conn <- dyn_conn$connectivity
  times <- dyn_conn$times
  n_windows <- dim(conn)[3]

  if (is.character(node_pair) && node_pair == "mean") {
    # Average connectivity over all pairs
    values <- numeric(n_windows)
    for (w in seq_len(n_windows)) {
      mat <- conn[, , w]
      values[w] <- mean(mat[upper.tri(mat)], na.rm = TRUE)
    }
    ylabel <- "Mean Connectivity"
  } else {
    # Specific node pair
    i <- node_pair[1]
    j <- node_pair[2]
    values <- conn[i, j, ]
    ylabel <- sprintf("Connectivity (%d-%d)", i, j)
  }

  plot_df <- data.frame(time = times, connectivity = values)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = connectivity)) +
    ggplot2::geom_line(color = "#3498db", linewidth = 1) +
    ggplot2::geom_point(color = "#2980b9", size = 2) +
    ggplot2::labs(x = "Time (s)", y = ylabel, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    )

  p
}

#' Plot network stability over time
#'
#' Visualizes the temporal stability of network topology.
#'
#' @param stability Result from temporalStability().
#' @param title Plot title.
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)),
#'   samplingRate = 100
#' )
#' dyn_conn <- slidingWindowConnectivity(pe, window_size = 200, step = 50)
#' stab <- temporalStability(dyn_conn)
#' plotNetworkStability(stab)
plotNetworkStability <- function(stability, title = "Network Stability") {
  if (is.na(stability$mean_stability)) {
    stop("Insufficient data for stability plot", call. = FALSE)
  }

  plot_df <- data.frame(
    time = stability$times,
    stability = stability$stability_over_time
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = stability)) +
    ggplot2::geom_line(color = "#27ae60", linewidth = 1) +
    ggplot2::geom_point(color = "#229954", size = 2) +
    ggplot2::geom_hline(yintercept = stability$mean_stability,
                         linetype = "dashed", color = "#e74c3c") +
    ggplot2::annotate("text",
                       x = max(plot_df$time),
                       y = stability$mean_stability,
                       label = sprintf("Mean: %.3f", stability$mean_stability),
                       hjust = 1, vjust = -0.5, color = "#e74c3c") +
    ggplot2::labs(x = "Time (s)", y = "Stability", title = title) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    )

  p
}
