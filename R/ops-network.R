#' Network Analysis for PhysioExperiment
#'
#' Functions for graph-theoretic analysis of functional connectivity networks,
#' including network construction, topology measures, and spectral analysis.

#' Create adjacency matrix from connectivity
#'
#' Converts a connectivity matrix into an adjacency matrix for network analysis.
#'
#' @param connectivity A connectivity matrix or result from connectivityMatrix().
#' @param threshold Threshold value for binarization. If NULL, keeps weighted edges.
#' @param absolute If TRUE, uses absolute values before thresholding.
#' @return A square adjacency matrix.
#' @export
#' @examples
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500, ncol = 4)),
#'   samplingRate = 100
#' )
#' conn <- correlationMatrix(pe)
#' adj <- adjacencyMatrix(conn$correlation, threshold = 0.3)
adjacencyMatrix <- function(connectivity, threshold = NULL, absolute = FALSE) {
  # Handle list input from connectivity functions

if (is.list(connectivity)) {
    if ("matrix" %in% names(connectivity)) {
      connectivity <- connectivity$matrix
    } else if ("correlation" %in% names(connectivity)) {
      connectivity <- connectivity$correlation
    } else if ("coherence" %in% names(connectivity)) {
      # Average across frequencies for coherence
      connectivity <- apply(connectivity$coherence, c(2, 3), mean)
    } else {
      stop("Unknown connectivity list format", call. = FALSE)
    }
  }

  if (!is.matrix(connectivity)) {
    stop("'connectivity' must be a matrix or connectivity result list", call. = FALSE)
  }

  adj <- connectivity

  # Use absolute values if requested
  if (absolute) {
    adj <- abs(adj)
  }

  # Remove self-connections
  diag(adj) <- 0

  # Apply threshold
  if (!is.null(threshold)) {
    adj[adj < threshold] <- 0
  }

  adj
}

#' Threshold network by density
#'
#' Thresholds a connectivity matrix to achieve a target network density.
#'
#' @param connectivity A connectivity matrix.
#' @param density Target density (proportion of edges to keep, 0-1).
#' @param absolute If TRUE, uses absolute values for ranking.
#' @return A thresholded adjacency matrix.
#' @export
#' @examples
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(500 * 4), nrow = 500, ncol = 4)),
#'   samplingRate = 100
#' )
#' conn <- correlationMatrix(pe)
#' # Keep top 30% of connections
#' adj <- thresholdNetwork(conn$correlation, density = 0.3)
thresholdNetwork <- function(connectivity, density = 0.2, absolute = TRUE) {
  if (is.list(connectivity)) {
    connectivity <- adjacencyMatrix(connectivity)
  }

  n <- nrow(connectivity)
  adj <- connectivity
  diag(adj) <- 0

  if (absolute) {
    vals <- abs(adj[upper.tri(adj)])
  } else {
    vals <- adj[upper.tri(adj)]
  }

  # Calculate threshold for target density
  n_edges <- length(vals)
  n_keep <- ceiling(n_edges * density)
  threshold <- sort(abs(vals), decreasing = TRUE)[min(n_keep, n_edges)]

  # Apply threshold
  if (absolute) {
    adj[abs(adj) < threshold] <- 0
  } else {
    adj[adj < threshold] <- 0
  }

  adj
}

#' Binarize network
#'
#' Converts a weighted adjacency matrix to a binary network.
#'
#' @param adjacency A weighted adjacency matrix.
#' @param threshold Threshold for binarization. Default 0 (any non-zero edge).
#' @return A binary adjacency matrix (0s and 1s).
#' @export
#' @examples
#' set.seed(123)
#' adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
#' bin <- binarizeNetwork(adj)
binarizeNetwork <- function(adjacency, threshold = 0) {
  binary <- adjacency
  binary[abs(adjacency) > threshold] <- 1
  binary[abs(adjacency) <= threshold] <- 0
  diag(binary) <- 0
  binary
}

#' Compute node degree
#'
#' Calculates the degree (number of connections) for each node.
#'
#' @param adjacency An adjacency matrix (binary or weighted).
#' @param weighted If TRUE, returns weighted degree (strength).
#' @return A numeric vector of node degrees.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3)
#' nodeDegree(adj)
nodeDegree <- function(adjacency, weighted = FALSE, use_cpp = TRUE) {
  diag(adjacency) <- 0

  # Use C++ implementation if available
  if (use_cpp) {
    result <- tryCatch(
      as.vector(node_degree_cpp(adjacency, weighted)),
      error = function(e) {
        warning("C++ implementation unavailable, using R fallback", call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # R implementation (fallback)
  if (weighted) {
    rowSums(abs(adjacency))
  } else {
    rowSums(adjacency != 0)
  }
}

#' Compute clustering coefficient
#'
#' Calculates the local clustering coefficient for each node.
#'
#' @param adjacency An adjacency matrix.
#' @param weighted If TRUE, uses weighted clustering coefficient.
#' @return A numeric vector of clustering coefficients.
#' @export
#' @examples
#' # Fully connected triangle
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' clusteringCoefficient(adj)  # Should be 1 for all nodes
clusteringCoefficient <- function(adjacency, weighted = FALSE, use_cpp = TRUE, n_cores = 1L) {
  n <- nrow(adjacency)
  diag(adjacency) <- 0

  # Use C++ implementation if available
  if (use_cpp) {
    result <- tryCatch(
      as.vector(clustering_coefficient_cpp(adjacency, weighted, n_cores)),
      error = function(e) {
        warning("C++ implementation unavailable, using R fallback (slower)", call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # R implementation (fallback)
  cc <- numeric(n)

  for (i in seq_len(n)) {
    neighbors <- which(adjacency[i, ] != 0)
    k <- length(neighbors)

    if (k < 2) {
      cc[i] <- 0
      next
    }

    subgraph <- adjacency[neighbors, neighbors, drop = FALSE]

    if (weighted) {
      weights <- adjacency[i, neighbors]
      max_weight <- max(abs(adjacency))
      if (max_weight > 0) {
        weights <- (abs(weights) / max_weight)^(1/3)
      }

      tri_sum <- 0
      for (j in seq_along(neighbors)) {
        for (l in seq_along(neighbors)) {
          if (j != l && subgraph[j, l] != 0) {
            w_jl <- (abs(subgraph[j, l]) / max_weight)^(1/3)
            tri_sum <- tri_sum + weights[j] * weights[l] * w_jl
          }
        }
      }
      cc[i] <- tri_sum / (k * (k - 1))
    } else {
      n_triangles <- sum(subgraph != 0) / 2
      cc[i] <- (2 * n_triangles) / (k * (k - 1))
    }
  }

  cc
}

#' Compute shortest path length
#'
#' Calculates the shortest path length between all node pairs using Floyd-Warshall.
#'
#' @param adjacency An adjacency matrix.
#' @param weighted If TRUE, uses edge weights as distances.
#' @return A matrix of shortest path lengths.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' pathLength(adj)
pathLength <- function(adjacency, weighted = FALSE, use_cpp = TRUE) {
  n <- nrow(adjacency)
  diag(adjacency) <- 0

  # Use C++ implementation if available
  if (use_cpp) {
    result <- tryCatch(
      floyd_warshall_cpp(adjacency, weighted),
      error = function(e) {
        warning("C++ implementation unavailable, using R fallback (slower)", call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # R implementation (fallback)
  if (weighted) {
    max_weight <- max(abs(adjacency[adjacency != 0]))
    dist_matrix <- matrix(Inf, n, n)
    dist_matrix[adjacency != 0] <- max_weight / abs(adjacency[adjacency != 0])
  } else {
    dist_matrix <- matrix(Inf, n, n)
    dist_matrix[adjacency != 0] <- 1
  }
  diag(dist_matrix) <- 0

  for (k in seq_len(n)) {
    dist_via_k <- outer(dist_matrix[, k], dist_matrix[k, ], `+`)
    dist_matrix <- pmin(dist_matrix, dist_via_k)
  }

  dist_matrix
}

#' Compute betweenness centrality
#'
#' Calculates betweenness centrality for each node.
#'
#' @param adjacency An adjacency matrix.
#' @param normalized If TRUE, normalizes by (n-1)(n-2)/2.
#' @return A numeric vector of betweenness centrality values.
#' @export
#' @examples
#' # Star network - center node should have highest betweenness
#' adj <- matrix(0, 4, 4)
#' adj[1, 2:4] <- 1
#' adj[2:4, 1] <- 1
#' betweennessCentrality(adj)
betweennessCentrality <- function(adjacency, normalized = TRUE, use_cpp = TRUE, n_cores = 1L) {
  n <- nrow(adjacency)
  diag(adjacency) <- 0

  # Use C++ implementation if available (Brandes algorithm with OpenMP)
  if (use_cpp) {
    result <- tryCatch(
      as.vector(betweenness_centrality_cpp(adjacency, normalized, n_cores)),
      error = function(e) {
        warning("C++ Brandes algorithm unavailable, using R fallback (slower)", call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # R implementation (fallback)
  bc <- numeric(n)

  for (s in seq_len(n)) {
    dist <- rep(Inf, n)
    dist[s] <- 0
    sigma <- rep(0, n)
    sigma[s] <- 1
    pred <- vector("list", n)

    queue <- s
    order <- integer(0)

    while (length(queue) > 0) {
      v <- queue[1]
      queue <- queue[-1]
      order <- c(order, v)

      neighbors <- which(adjacency[v, ] != 0)
      for (w in neighbors) {
        if (dist[w] == Inf) {
          dist[w] <- dist[v] + 1
          queue <- c(queue, w)
        }
        if (dist[w] == dist[v] + 1) {
          sigma[w] <- sigma[w] + sigma[v]
          pred[[w]] <- c(pred[[w]], v)
        }
      }
    }

    delta <- rep(0, n)
    for (w in rev(order)) {
      for (v in pred[[w]]) {
        delta[v] <- delta[v] + (sigma[v] / sigma[w]) * (1 + delta[w])
      }
      if (w != s) {
        bc[w] <- bc[w] + delta[w]
      }
    }
  }

  if (normalized && n > 2) {
    bc <- bc / ((n - 1) * (n - 2))
  }

  bc
}

#' Compute eigenvector centrality
#'
#' Calculates eigenvector centrality for each node.
#'
#' @param adjacency An adjacency matrix.
#' @param max_iter Maximum iterations for power method.
#' @param tol Convergence tolerance.
#' @return A numeric vector of eigenvector centrality values.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' eigenvectorCentrality(adj)
eigenvectorCentrality <- function(adjacency, max_iter = 100, tol = 1e-6, use_cpp = TRUE) {
  n <- nrow(adjacency)
  diag(adjacency) <- 0

  # Use C++ implementation if available
  if (use_cpp) {
    result <- tryCatch(
      as.vector(eigenvector_centrality_cpp(adjacency, max_iter, tol)),
      error = function(e) {
        warning("C++ implementation unavailable, using R fallback", call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # R implementation (fallback)
  x <- rep(1, n)
  x <- x / sqrt(sum(x^2))

  for (iter in seq_len(max_iter)) {
    x_new <- adjacency %*% x
    x_new <- as.vector(x_new)

    norm_val <- sqrt(sum(x_new^2))
    if (norm_val > 0) {
      x_new <- x_new / norm_val
    }

    if (max(abs(x_new - x)) < tol) {
      break
    }
    x <- x_new
  }

  if (sum(x) < 0) {
    x <- -x
  }

  x
}

#' Compute graph Laplacian
#'
#' Calculates the graph Laplacian matrix.
#'
#' @param adjacency An adjacency matrix.
#' @param normalized If TRUE, returns normalized Laplacian.
#' @return The Laplacian matrix.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' L <- graphLaplacian(adj)
graphLaplacian <- function(adjacency, normalized = FALSE) {
  diag(adjacency) <- 0
  degree <- rowSums(abs(adjacency))
  D <- diag(degree)
  L <- D - adjacency

  if (normalized) {
    # Normalized Laplacian: D^(-1/2) * L * D^(-1/2)
    d_inv_sqrt <- 1 / sqrt(degree)
    d_inv_sqrt[!is.finite(d_inv_sqrt)] <- 0
    D_inv_sqrt <- diag(d_inv_sqrt)
    L <- D_inv_sqrt %*% L %*% D_inv_sqrt
  }

  L
}

#' Spectral decomposition of graph Laplacian
#'
#' Computes eigenvalues and eigenvectors of the graph Laplacian.
#'
#' @param adjacency An adjacency matrix.
#' @param normalized If TRUE, uses normalized Laplacian.
#' @param n_components Number of components to return. If NULL, returns all.
#' @return A list with eigenvalues and eigenvectors.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' spec <- spectralDecomposition(adj)
spectralDecomposition <- function(adjacency, normalized = TRUE, n_components = NULL) {
  L <- graphLaplacian(adjacency, normalized = normalized)
  eig <- eigen(L, symmetric = TRUE)

  # Sort by eigenvalue (ascending)
  ord <- order(eig$values)
  values <- eig$values[ord]
  vectors <- eig$vectors[, ord, drop = FALSE]

  if (!is.null(n_components)) {
    n_components <- min(n_components, length(values))
    values <- values[seq_len(n_components)]
    vectors <- vectors[, seq_len(n_components), drop = FALSE]
  }

  list(
    eigenvalues = values,
    eigenvectors = vectors,
    laplacian = L
  )
}

#' Spectral clustering of network nodes
#'
#' Performs spectral clustering on the network.
#'
#' @param adjacency An adjacency matrix.
#' @param n_clusters Number of clusters.
#' @param normalized If TRUE, uses normalized Laplacian.
#' @return A list with cluster assignments and spectral embedding.
#' @export
#' @examples
#' # Create block-structured network
#' adj <- matrix(0, 6, 6)
#' adj[1:3, 1:3] <- 1
#' adj[4:6, 4:6] <- 1
#' adj[3, 4] <- adj[4, 3] <- 0.5
#' diag(adj) <- 0
#' clusters <- spectralClustering(adj, n_clusters = 2)
spectralClustering <- function(adjacency, n_clusters = 2, normalized = TRUE) {
  spec <- spectralDecomposition(adjacency, normalized = normalized,
                                 n_components = n_clusters)

  # Use eigenvectors for k-means clustering
  embedding <- spec$eigenvectors

  # Simple k-means implementation
  n <- nrow(embedding)
  k <- n_clusters

  # Initialize centroids randomly
  set.seed(123)
  centroids <- embedding[sample(n, k), , drop = FALSE]

  for (iter in 1:100) {
    # Assign points to nearest centroid
    dists <- matrix(0, n, k)
    for (j in seq_len(k)) {
      diff <- sweep(embedding, 2, centroids[j, ])
      dists[, j] <- rowSums(diff^2)
    }
    clusters <- apply(dists, 1, which.min)

    # Update centroids
    new_centroids <- matrix(0, k, ncol(embedding))
    for (j in seq_len(k)) {
      members <- which(clusters == j)
      if (length(members) > 0) {
        new_centroids[j, ] <- colMeans(embedding[members, , drop = FALSE])
      }
    }

    # Check convergence
    if (max(abs(new_centroids - centroids)) < 1e-6) {
      break
    }
    centroids <- new_centroids
  }

  list(
    clusters = clusters,
    embedding = embedding,
    eigenvalues = spec$eigenvalues
  )
}

#' Compute global efficiency
#'
#' Calculates the global efficiency of a network.
#'
#' @param adjacency An adjacency matrix.
#' @param weighted If TRUE, uses weighted paths.
#' @return Global efficiency value (0-1).
#' @export
#' @examples
#' # Fully connected - maximum efficiency
#' adj <- matrix(1, 4, 4)
#' diag(adj) <- 0
#' globalEfficiency(adj)
globalEfficiency <- function(adjacency, weighted = FALSE, use_cpp = TRUE) {
  n <- nrow(adjacency)
  if (n < 2) return(0)

  # Use C++ implementation if available
  if (use_cpp) {
    result <- tryCatch(
      global_efficiency_cpp(adjacency, weighted),
      error = function(e) {
        warning("C++ implementation unavailable, using R fallback (slower)", call. = FALSE)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # R implementation (fallback)
  dist_matrix <- pathLength(adjacency, weighted = weighted, use_cpp = use_cpp)
  diag(dist_matrix) <- Inf  # Exclude self-paths

  # Efficiency is inverse of path length
  eff_matrix <- 1 / dist_matrix
  eff_matrix[!is.finite(eff_matrix)] <- 0

  # Global efficiency: average of all pairwise efficiencies
  sum(eff_matrix) / (n * (n - 1))
}

#' Compute local efficiency
#'
#' Calculates the local efficiency for each node.
#'
#' @param adjacency An adjacency matrix.
#' @param weighted If TRUE, uses weighted paths.
#' @return A numeric vector of local efficiency values.
#' @export
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' localEfficiency(adj)
localEfficiency <- function(adjacency, weighted = FALSE) {
  n <- nrow(adjacency)
  diag(adjacency) <- 0
  local_eff <- numeric(n)

  for (i in seq_len(n)) {
    neighbors <- which(adjacency[i, ] != 0)
    k <- length(neighbors)

    if (k < 2) {
      local_eff[i] <- 0
      next
    }

    # Subgraph of neighbors
    subgraph <- adjacency[neighbors, neighbors, drop = FALSE]

    # Global efficiency of subgraph
    local_eff[i] <- globalEfficiency(subgraph, weighted = weighted)
  }

  local_eff
}

#' Compute small-worldness
#'
#' Calculates the small-world coefficient (sigma) of a network.
#'
#' @param adjacency An adjacency matrix.
#' @param n_rand Number of random networks for comparison.
#' @param n_cores Number of cores for parallel processing. Default NULL uses
#'   sequential processing.
#' @return A list with small-worldness metrics.
#' @export
#' @examples
#' # Create a small-world-like network
#' set.seed(123)
#' n <- 20
#' adj <- matrix(0, n, n)
#' for (i in 1:n) {
#'   adj[i, ((i) %% n) + 1] <- 1
#'   adj[i, ((i + 1) %% n) + 1] <- 1
#' }
#' adj <- adj + t(adj)
#' adj[adj > 0] <- 1
#' # Add some random long-range connections
#' adj[sample(which(adj == 0 & row(adj) < col(adj)), 5)] <- 1
#' adj <- adj + t(adj)
#' adj[adj > 0] <- 1
#' diag(adj) <- 0
#' sw <- smallWorldness(adj, n_rand = 10)
smallWorldness <- function(adjacency, n_rand = 100, n_cores = NULL) {
  diag(adjacency) <- 0
  binary <- binarizeNetwork(adjacency)

  # Network metrics
  cc_real <- mean(clusteringCoefficient(binary))
  dist_matrix <- pathLength(binary)
  dist_matrix[!is.finite(dist_matrix)] <- NA
  pl_real <- mean(dist_matrix[upper.tri(dist_matrix)], na.rm = TRUE)

  # Function to compute metrics for one random network
  .computeRandomMetrics <- function(r, binary) {
    rand_adj <- .randomNetworkDegreePreserving(binary)
    cc <- mean(clusteringCoefficient(rand_adj))
    dist_r <- pathLength(rand_adj)
    dist_r[!is.finite(dist_r)] <- NA
    pl <- mean(dist_r[upper.tri(dist_r)], na.rm = TRUE)
    c(cc = cc, pl = pl)
  }

  # Generate random networks (with optional parallel processing)
  if (!is.null(n_cores) && n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(cl, c(".randomNetworkDegreePreserving",
                                   "clusteringCoefficient", "pathLength",
                                   "binarizeNetwork", "nodeDegree"),
                            envir = environment())

    rand_metrics <- parallel::parSapply(cl, seq_len(n_rand),
                                         .computeRandomMetrics, binary = binary)
  } else {
    rand_metrics <- sapply(seq_len(n_rand), .computeRandomMetrics, binary = binary)
  }

  cc_rand <- rand_metrics["cc", ]
  pl_rand <- rand_metrics["pl", ]

  # Small-worldness metrics
  cc_ratio <- cc_real / mean(cc_rand)
  pl_ratio <- pl_real / mean(pl_rand)
  sigma <- cc_ratio / pl_ratio

  list(
    sigma = sigma,
    clustering_coefficient = cc_real,
    path_length = pl_real,
    cc_random = mean(cc_rand),
    pl_random = mean(pl_rand),
    cc_ratio = cc_ratio,
    pl_ratio = pl_ratio
  )
}

#' Generate random network preserving degree sequence
#' @noRd
.randomNetworkDegreePreserving <- function(adjacency) {
  n <- nrow(adjacency)
  rand_adj <- adjacency

  # Edge rewiring (Maslov-Sneppen algorithm)
  edges <- which(adjacency != 0 & row(adjacency) < col(adjacency), arr.ind = TRUE)
  n_edges <- nrow(edges)
  n_swaps <- n_edges * 10

  for (swap in seq_len(n_swaps)) {
    if (n_edges < 2) break

    # Select two random edges
    idx <- sample(n_edges, 2)
    e1 <- edges[idx[1], ]
    e2 <- edges[idx[2], ]

    # Try to swap endpoints
    # (a-b, c-d) -> (a-c, b-d) or (a-d, b-c)
    a <- e1[1]; b <- e1[2]
    c <- e2[1]; d <- e2[2]

    # Skip if nodes overlap
    if (length(unique(c(a, b, c, d))) < 4) next

    # Try first swap pattern
    if (rand_adj[a, c] == 0 && rand_adj[b, d] == 0) {
      rand_adj[a, b] <- rand_adj[b, a] <- 0
      rand_adj[c, d] <- rand_adj[d, c] <- 0
      rand_adj[a, c] <- rand_adj[c, a] <- 1
      rand_adj[b, d] <- rand_adj[d, b] <- 1

      # Update edge list
      edges[idx[1], ] <- c(min(a, c), max(a, c))
      edges[idx[2], ] <- c(min(b, d), max(b, d))
    }
  }

  rand_adj
}

#' Compute network modularity
#'
#' Calculates the modularity of a network given a community partition.
#'
#' @param adjacency An adjacency matrix.
#' @param communities A vector of community assignments for each node.
#' @return Modularity value (-0.5 to 1).
#' @export
#' @examples
#' # Two clear communities
#' adj <- matrix(0, 6, 6)
#' adj[1:3, 1:3] <- 1
#' adj[4:6, 4:6] <- 1
#' diag(adj) <- 0
#' communities <- c(1, 1, 1, 2, 2, 2)
#' modularity(adj, communities)
modularity <- function(adjacency, communities) {
  diag(adjacency) <- 0
  n <- nrow(adjacency)
  m <- sum(adjacency) / 2
  k <- rowSums(adjacency)

  Q <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (communities[i] == communities[j]) {
        Q <- Q + adjacency[i, j] - (k[i] * k[j]) / (2 * m)
      }
    }
  }

  Q / (2 * m)
}

#' Sliding window connectivity
#'
#' Computes connectivity matrices over sliding time windows.
#'
#' @param x A PhysioExperiment object.
#' @param window_size Window size in samples.
#' @param step Step size in samples.
#' @param method Connectivity method: "correlation", "coherence", or "plv".
#' @param ... Additional arguments passed to connectivity function.
#' @return A list with time-varying connectivity matrices.
#' @export
#' @examples
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)),
#'   samplingRate = 100
#' )
#' dyn_conn <- slidingWindowConnectivity(pe, window_size = 200, step = 50)
slidingWindowConnectivity <- function(x, window_size = 256L, step = 64L,
                                       method = c("correlation", "coherence", "plv"),
                                       ...) {
  stopifnot(inherits(x, "PhysioExperiment"))
  method <- match.arg(method)

  sr <- samplingRate(x)
  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)

  n_time <- nrow(data)
  n_channels <- ncol(data)
  n_windows <- floor((n_time - window_size) / step) + 1

  # Initialize output
  conn_matrices <- array(NA_real_, dim = c(n_channels, n_channels, n_windows))
  window_times <- numeric(n_windows)

  for (w in seq_len(n_windows)) {
    start <- (w - 1) * step + 1
    end <- start + window_size - 1

    # Extract window data
    window_data <- data[start:end, , drop = FALSE]

    # Create temporary PhysioExperiment for this window
    pe_window <- PhysioExperiment(
      assays = list(window = window_data),
      samplingRate = sr
    )

    # Compute connectivity
    if (method == "correlation") {
      conn <- correlationMatrix(pe_window, assay_name = "window", ...)
      conn_matrices[, , w] <- conn$correlation
    } else if (method == "coherence") {
      conn <- coherence(pe_window, assay_name = "window", ...)
      # Average across frequencies
      conn_matrices[, , w] <- apply(conn$coherence, c(2, 3), mean)
    } else if (method == "plv") {
      conn <- plv(pe_window, assay_name = "window", ...)
      conn_matrices[, , w] <- conn$plv
    }

    window_times[w] <- (start + end - 1) / 2 / sr
  }

  list(
    connectivity = conn_matrices,
    times = window_times,
    window_size = window_size,
    step = step,
    method = method,
    sampling_rate = sr
  )
}

#' Compute temporal stability of network
#'
#' Calculates how stable the network topology is over time.
#'
#' @param dyn_conn Result from slidingWindowConnectivity().
#' @param metric Stability metric: "correlation" or "distance".
#' @return A list with stability metrics.
#' @export
#' @examples
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)),
#'   samplingRate = 100
#' )
#' dyn_conn <- slidingWindowConnectivity(pe, window_size = 200, step = 50)
#' stability <- temporalStability(dyn_conn)
temporalStability <- function(dyn_conn, metric = c("correlation", "distance")) {
  metric <- match.arg(metric)

  conn <- dyn_conn$connectivity
  n_windows <- dim(conn)[3]

  if (n_windows < 2) {
    return(list(mean_stability = NA, stability_over_time = NA))
  }

  # Compute pairwise stability between consecutive windows
  stability <- numeric(n_windows - 1)

  for (w in seq_len(n_windows - 1)) {
    mat1 <- conn[, , w]
    mat2 <- conn[, , w + 1]

    # Vectorize upper triangle
    vec1 <- mat1[upper.tri(mat1)]
    vec2 <- mat2[upper.tri(mat2)]

    if (metric == "correlation") {
      stability[w] <- stats::cor(vec1, vec2, use = "complete.obs")
    } else {
      # Euclidean distance (normalized)
      stability[w] <- 1 - sqrt(sum((vec1 - vec2)^2)) / length(vec1)
    }
  }

  list(
    mean_stability = mean(stability, na.rm = TRUE),
    stability_over_time = stability,
    times = dyn_conn$times[-1]
  )
}
