library(testthat)
library(PhysioExperiment)

# --- Helper: create a known test adjacency matrix ---

# Fully connected triangle
make_triangle <- function() {
  matrix(c(0, 1, 1,
            1, 0, 1,
            1, 1, 0), 3, 3)
}

# Star network: node 1 connected to all others
make_star <- function(n = 5) {
  adj <- matrix(0, n, n)
  adj[1, 2:n] <- 1
  adj[2:n, 1] <- 1
  adj
}

# Two-community block structure
make_two_blocks <- function() {
  adj <- matrix(0, 6, 6)
  adj[1:3, 1:3] <- 1
  adj[4:6, 4:6] <- 1
  diag(adj) <- 0
  adj
}

# Path graph: 1-2-3-4
make_path <- function(n = 4) {
  adj <- matrix(0, n, n)
  for (i in seq_len(n - 1)) {
    adj[i, i + 1] <- 1
    adj[i + 1, i] <- 1
  }
  adj
}

# --- adjacencyMatrix ---

test_that("adjacencyMatrix creates matrix from numeric matrix input", {
  conn <- matrix(c(1, 0.5, 0.3,
                    0.5, 1, 0.7,
                    0.3, 0.7, 1), 3, 3)

  adj <- adjacencyMatrix(conn)

  expect_true(is.matrix(adj))
  expect_equal(nrow(adj), 3)
  expect_equal(diag(adj), c(0, 0, 0))  # self-connections removed
})

test_that("adjacencyMatrix applies threshold", {
  conn <- matrix(c(1, 0.5, 0.3,
                    0.5, 1, 0.7,
                    0.3, 0.7, 1), 3, 3)

  adj <- adjacencyMatrix(conn, threshold = 0.4)

  expect_equal(adj[1, 3], 0)  # 0.3 < 0.4, should be zero
  expect_equal(adj[1, 2], 0.5)  # 0.5 >= 0.4, should be kept
})

test_that("adjacencyMatrix applies absolute values", {
  conn <- matrix(c(0, -0.8, 0.3,
                    -0.8, 0, 0.5,
                    0.3, 0.5, 0), 3, 3)

  adj <- adjacencyMatrix(conn, absolute = TRUE)

  expect_true(all(adj >= 0))
})

test_that("adjacencyMatrix handles list input with 'correlation' element", {
  conn_list <- list(correlation = matrix(c(1, 0.5, 0.5, 1), 2, 2))
  adj <- adjacencyMatrix(conn_list)
  expect_true(is.matrix(adj))
  expect_equal(diag(adj), c(0, 0))
})

test_that("adjacencyMatrix errors on non-matrix/non-list input", {
  expect_error(adjacencyMatrix("not_a_matrix"), "must be a matrix")
})

# --- thresholdNetwork ---

test_that("thresholdNetwork keeps target density", {
  set.seed(42)
  conn <- matrix(runif(25), 5, 5)
  conn <- (conn + t(conn)) / 2
  diag(conn) <- 0

  adj <- thresholdNetwork(conn, density = 0.5)

  n_edges <- sum(adj[upper.tri(adj)] != 0)
  max_edges <- 5 * 4 / 2
  actual_density <- n_edges / max_edges

  # Should be approximately 50%
  expect_lte(abs(actual_density - 0.5), 0.2)
})

# --- binarizeNetwork ---

test_that("binarizeNetwork produces 0/1 matrix", {
  adj <- matrix(c(0, 0.5, 0.3,
                   0.5, 0, 0.8,
                   0.3, 0.8, 0), 3, 3)

  bin <- binarizeNetwork(adj)

  expect_true(all(bin %in% c(0, 1)))
  expect_equal(diag(bin), c(0, 0, 0))
})

test_that("binarizeNetwork respects threshold", {
  adj <- matrix(c(0, 0.5, 0.3,
                   0.5, 0, 0.8,
                   0.3, 0.8, 0), 3, 3)

  bin <- binarizeNetwork(adj, threshold = 0.4)

  expect_equal(bin[1, 3], 0)  # 0.3 <= 0.4
  expect_equal(bin[1, 2], 1)  # 0.5 > 0.4
})

# --- nodeDegree ---

test_that("nodeDegree returns correct values for triangle", {
  adj <- make_triangle()
  deg <- nodeDegree(adj)

  expect_equal(length(deg), 3)
  expect_true(all(deg == 2))  # each node connected to 2 others
})

test_that("nodeDegree returns correct values for star", {
  adj <- make_star(5)
  deg <- nodeDegree(adj)

  expect_equal(deg[1], 4)  # center node
  expect_true(all(deg[2:5] == 1))  # leaf nodes
})

test_that("nodeDegree weighted mode returns sums", {
  adj <- matrix(c(0, 2, 3,
                   2, 0, 1,
                   3, 1, 0), 3, 3)

  deg_w <- nodeDegree(adj, weighted = TRUE)

  expect_equal(deg_w[1], 5)  # 2 + 3
  expect_equal(deg_w[2], 3)  # 2 + 1
  expect_equal(deg_w[3], 4)  # 3 + 1
})

# --- clusteringCoefficient ---

test_that("clusteringCoefficient is 1 for fully connected triangle", {
  adj <- make_triangle()
  cc <- clusteringCoefficient(adj)

  expect_equal(length(cc), 3)
  expect_true(all(abs(cc - 1) < 1e-10))
})

test_that("clusteringCoefficient is 0 for star network leaves", {
  adj <- make_star(5)
  cc <- clusteringCoefficient(adj)

  # Leaf nodes have degree 1, so clustering coefficient is 0
  expect_true(all(cc[2:5] == 0))
})

# --- pathLength ---

test_that("pathLength returns correct values for path graph", {
  adj <- make_path(4)
  dist_mat <- pathLength(adj)

  expect_equal(dist_mat[1, 2], 1)  # direct neighbors
  expect_equal(dist_mat[1, 3], 2)  # two hops
  expect_equal(dist_mat[1, 4], 3)  # three hops
  expect_equal(diag(dist_mat), c(0, 0, 0, 0))
})

test_that("pathLength returns Inf for disconnected components", {
  adj <- matrix(0, 4, 4)
  adj[1, 2] <- adj[2, 1] <- 1
  adj[3, 4] <- adj[4, 3] <- 1

  dist_mat <- pathLength(adj)

  expect_equal(dist_mat[1, 2], 1)
  expect_equal(dist_mat[1, 3], Inf)
})

# --- betweennessCentrality ---

test_that("betweennessCentrality identifies hub in star network", {
  adj <- make_star(5)
  bc <- betweennessCentrality(adj, normalized = FALSE)

  # Center node should have highest betweenness
  expect_equal(which.max(bc), 1)
  expect_true(bc[1] > bc[2])
})

test_that("betweennessCentrality normalized values are between 0 and 1", {
  adj <- make_star(5)
  bc <- betweennessCentrality(adj, normalized = TRUE)

  expect_true(all(bc >= 0))
  expect_true(all(bc <= 1))
})

# --- eigenvectorCentrality ---

test_that("eigenvectorCentrality is uniform for fully connected graph", {
  adj <- make_triangle()
  ec <- eigenvectorCentrality(adj)

  expect_equal(length(ec), 3)
  # All nodes should have equal centrality in a fully connected graph
  expect_true(max(abs(ec - ec[1])) < 1e-4)
})

test_that("eigenvectorCentrality identifies hub node", {
  adj <- make_star(5)
  ec <- eigenvectorCentrality(adj)

  # Center node should have highest eigenvector centrality
  expect_equal(which.max(ec), 1)
})

# --- graphLaplacian ---

test_that("graphLaplacian row sums are zero", {
  adj <- make_triangle()
  L <- graphLaplacian(adj)

  row_sums <- rowSums(L)
  expect_true(all(abs(row_sums) < 1e-10))
})

test_that("graphLaplacian normalized has ones on diagonal", {
  adj <- make_triangle()
  L <- graphLaplacian(adj, normalized = TRUE)

  # Diagonal should be 1 for nodes with edges
  expect_true(all(abs(diag(L) - 1) < 1e-10))
})

# --- spectralDecomposition ---

test_that("spectralDecomposition returns eigenvalues and eigenvectors", {
  adj <- make_triangle()
  spec <- spectralDecomposition(adj)

  expect_true("eigenvalues" %in% names(spec))
  expect_true("eigenvectors" %in% names(spec))
  expect_true("laplacian" %in% names(spec))

  # First eigenvalue of Laplacian should be approximately 0
  expect_true(abs(spec$eigenvalues[1]) < 1e-10)
})

test_that("spectralDecomposition respects n_components", {
  adj <- make_triangle()
  spec <- spectralDecomposition(adj, n_components = 2)

  expect_equal(length(spec$eigenvalues), 2)
  expect_equal(ncol(spec$eigenvectors), 2)
})

# --- spectralClustering ---

test_that("spectralClustering separates two blocks", {
  adj <- make_two_blocks()
  result <- spectralClustering(adj, n_clusters = 2)

  expect_true("clusters" %in% names(result))
  expect_equal(length(result$clusters), 6)

  # Nodes in the same block should be in the same cluster
  expect_equal(result$clusters[1], result$clusters[2])
  expect_equal(result$clusters[1], result$clusters[3])
  expect_equal(result$clusters[4], result$clusters[5])
  expect_equal(result$clusters[4], result$clusters[6])

  # Two blocks should be in different clusters
  expect_true(result$clusters[1] != result$clusters[4])
})

# --- globalEfficiency ---

test_that("globalEfficiency is 1 for fully connected graph", {
  n <- 4
  adj <- matrix(1, n, n)
  diag(adj) <- 0

  eff <- globalEfficiency(adj)

  expect_equal(eff, 1, tolerance = 1e-10)
})

test_that("globalEfficiency is 0 for isolated nodes", {
  adj <- matrix(0, 3, 3)
  eff <- globalEfficiency(adj)

  expect_equal(eff, 0)
})

# --- localEfficiency ---

test_that("localEfficiency returns correct length", {
  adj <- make_triangle()
  le <- localEfficiency(adj)

  expect_equal(length(le), 3)
})

test_that("localEfficiency is 1 for fully connected triangle", {
  adj <- make_triangle()
  le <- localEfficiency(adj)

  # In a triangle, each node's neighbors are fully connected
  expect_true(all(abs(le - 1) < 1e-10))
})

# --- modularity ---

test_that("modularity is positive for correct partition", {
  adj <- make_two_blocks()
  communities <- c(1, 1, 1, 2, 2, 2)

  Q <- modularity(adj, communities)

  expect_gt(Q, 0)
})

test_that("modularity is lower for incorrect partition", {
  adj <- make_two_blocks()

  correct_Q <- modularity(adj, c(1, 1, 1, 2, 2, 2))
  wrong_Q <- modularity(adj, c(1, 2, 1, 2, 1, 2))

  expect_gt(correct_Q, wrong_Q)
})

# --- slidingWindowConnectivity ---

test_that("slidingWindowConnectivity returns expected structure", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 4, sr = 100)

  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)

  expect_true(is.list(dyn_conn))
  expect_true("connectivity" %in% names(dyn_conn))
  expect_true("times" %in% names(dyn_conn))
  expect_true("method" %in% names(dyn_conn))

  # connectivity should be 3D: channels x channels x n_windows
  conn_dims <- dim(dyn_conn$connectivity)
  expect_equal(conn_dims[1], 4)
  expect_equal(conn_dims[2], 4)
  expect_equal(length(dyn_conn$times), conn_dims[3])
})

test_that("slidingWindowConnectivity window count is correct", {
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)

  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 100)

  expected_windows <- floor((500 - 100) / 100) + 1
  expect_equal(dim(dyn_conn$connectivity)[3], expected_windows)
})

# --- temporalStability ---

test_that("temporalStability returns stability metrics", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)

  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)
  stab <- temporalStability(dyn_conn)

  expect_true(is.list(stab))
  expect_true("mean_stability" %in% names(stab))
  expect_true("stability_over_time" %in% names(stab))
  expect_true(is.numeric(stab$mean_stability))
})

test_that("temporalStability with single window returns NA", {
  # Create data that produces exactly 1 window
  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 100)
  stab <- temporalStability(dyn_conn)

  expect_true(is.na(stab$mean_stability))
})

# --- R fallback (use_cpp = FALSE) ---

test_that("nodeDegree works with R fallback", {
  adj <- make_triangle()
  deg <- nodeDegree(adj, use_cpp = FALSE)

  expect_equal(length(deg), 3)
  expect_true(all(deg == 2))
})

test_that("clusteringCoefficient works with R fallback", {
  adj <- make_triangle()
  cc <- clusteringCoefficient(adj, use_cpp = FALSE)

  expect_true(all(abs(cc - 1) < 1e-10))
})

test_that("pathLength works with R fallback", {
  adj <- make_path(3)
  dist_mat <- pathLength(adj, use_cpp = FALSE)

  expect_equal(dist_mat[1, 2], 1)
  expect_equal(dist_mat[1, 3], 2)
})

test_that("betweennessCentrality works with R fallback", {
  adj <- make_star(4)
  bc <- betweennessCentrality(adj, use_cpp = FALSE)

  expect_equal(which.max(bc), 1)
})

test_that("eigenvectorCentrality works with R fallback", {
  adj <- make_triangle()
  ec <- eigenvectorCentrality(adj, use_cpp = FALSE)

  expect_equal(length(ec), 3)
  expect_true(max(abs(ec - ec[1])) < 1e-4)
})

test_that("globalEfficiency works with R fallback", {
  adj <- matrix(1, 3, 3)
  diag(adj) <- 0

  eff <- globalEfficiency(adj, use_cpp = FALSE)
  expect_equal(eff, 1, tolerance = 1e-10)
})
