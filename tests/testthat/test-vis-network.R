library(testthat)
library(PhysioExperiment)

# --- Helper: create test adjacency matrix ---
make_test_adj <- function(n = 4) {
  set.seed(123)
  adj <- matrix(runif(n * n), n, n)
  adj <- (adj + t(adj)) / 2
  diag(adj) <- 0
  adj
}

# --- Function existence ---

test_that("plotNetwork function exists", {
  expect_true(is.function(plotNetwork))
})

test_that("plotAdjacencyMatrix function exists", {
  expect_true(is.function(plotAdjacencyMatrix))
})

test_that("plotNetworkMetrics function exists", {
  expect_true(is.function(plotNetworkMetrics))
})

test_that("plotDynamicConnectivity function exists", {
  expect_true(is.function(plotDynamicConnectivity))
})

test_that("plotNetworkStability function exists", {
  expect_true(is.function(plotNetworkStability))
})

# --- plotNetwork ---

test_that("plotNetwork returns a ggplot object", {
  adj <- make_test_adj(4)

  p <- plotNetwork(adj)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork works with node_names", {
  adj <- make_test_adj(4)

  p <- plotNetwork(adj, node_names = c("Fz", "Cz", "Pz", "Oz"))

  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork works with circle layout", {
  adj <- make_test_adj(4)
  p <- plotNetwork(adj, layout = "circle")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork works with spring layout", {
  adj <- make_test_adj(4)
  p <- plotNetwork(adj, layout = "spring")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork works with grid layout", {
  adj <- make_test_adj(4)
  p <- plotNetwork(adj, layout = "grid")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork works with numeric node_size", {
  adj <- make_test_adj(4)
  p <- plotNetwork(adj, node_size = 5)
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork works with different node_size metrics", {
  adj <- make_test_adj(4)

  for (metric in c("degree", "betweenness", "eigenvector")) {
    p <- plotNetwork(adj, node_size = metric)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plotNetwork handles edge_threshold", {
  adj <- make_test_adj(4)
  p <- plotNetwork(adj, edge_threshold = 0.5)
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork handles list input", {
  conn_list <- list(correlation = make_test_adj(3))
  p <- plotNetwork(conn_list)
  expect_s3_class(p, "ggplot")
})

test_that("plotNetwork accepts custom title", {
  adj <- make_test_adj(3)
  p <- plotNetwork(adj, title = "My Network")
  expect_s3_class(p, "ggplot")
})

# --- plotAdjacencyMatrix ---

test_that("plotAdjacencyMatrix returns a ggplot object", {
  adj <- make_test_adj(4)

  p <- plotAdjacencyMatrix(adj)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plotAdjacencyMatrix works with node_names", {
  adj <- make_test_adj(4)
  p <- plotAdjacencyMatrix(adj, node_names = c("Fz", "Cz", "Pz", "Oz"))
  expect_s3_class(p, "ggplot")
})

test_that("plotAdjacencyMatrix works with symmetric = FALSE", {
  adj <- make_test_adj(4)
  p <- plotAdjacencyMatrix(adj, symmetric = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plotAdjacencyMatrix supports different color palettes", {
  adj <- make_test_adj(4)

  for (palette in c("default", "viridis", "heat")) {
    p <- plotAdjacencyMatrix(adj, color_palette = palette)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plotAdjacencyMatrix supports show_values", {
  adj <- make_test_adj(3)
  p <- plotAdjacencyMatrix(adj, show_values = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plotAdjacencyMatrix handles list input with 'correlation'", {
  conn_list <- list(correlation = make_test_adj(3))
  p <- plotAdjacencyMatrix(conn_list)
  expect_s3_class(p, "ggplot")
})

test_that("plotAdjacencyMatrix handles list input with 'matrix'", {
  conn_list <- list(matrix = make_test_adj(3))
  p <- plotAdjacencyMatrix(conn_list)
  expect_s3_class(p, "ggplot")
})

# --- plotNetworkMetrics ---

test_that("plotNetworkMetrics returns a ggplot object", {
  adj <- matrix(c(0, 1, 1, 1,
                   1, 0, 1, 0,
                   1, 1, 0, 1,
                   1, 0, 1, 0), 4, 4)

  p <- plotNetworkMetrics(adj)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetworkMetrics works with custom node_names", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  p <- plotNetworkMetrics(adj, node_names = c("Fz", "Cz", "Pz"))
  expect_s3_class(p, "ggplot")
})

test_that("plotNetworkMetrics works with different metrics", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  p <- plotNetworkMetrics(adj, metrics = c("degree", "eigenvector", "local_efficiency"))
  expect_s3_class(p, "ggplot")
})

test_that("plotNetworkMetrics handles list input", {
  conn_list <- list(correlation = make_test_adj(3))
  p <- plotNetworkMetrics(conn_list)
  expect_s3_class(p, "ggplot")
})

# --- plotDynamicConnectivity ---

test_that("plotDynamicConnectivity returns a ggplot object", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)
  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)

  p <- plotDynamicConnectivity(dyn_conn, node_pair = c(1, 2))

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plotDynamicConnectivity works with mean mode", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)
  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)

  p <- plotDynamicConnectivity(dyn_conn, node_pair = "mean")

  expect_s3_class(p, "ggplot")
})

test_that("plotDynamicConnectivity accepts custom title", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)
  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)

  p <- plotDynamicConnectivity(dyn_conn, title = "Custom Title")
  expect_s3_class(p, "ggplot")
})

# --- plotNetworkStability ---

test_that("plotNetworkStability returns a ggplot object", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)
  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)
  stab <- temporalStability(dyn_conn)

  p <- plotNetworkStability(stab)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetworkStability accepts custom title", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 100)
  dyn_conn <- slidingWindowConnectivity(pe, window_size = 100, step = 50)
  stab <- temporalStability(dyn_conn)

  p <- plotNetworkStability(stab, title = "Custom Stability")
  expect_s3_class(p, "ggplot")
})

test_that("plotNetworkStability errors with insufficient data", {
  stab <- list(mean_stability = NA, stability_over_time = NA, times = NA)
  expect_error(plotNetworkStability(stab), "Insufficient data")
})
