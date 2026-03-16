// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <queue>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;

// Fast Floyd-Warshall algorithm for shortest path
// [[Rcpp::export]]
arma::mat floyd_warshall_cpp(const arma::mat& adjacency, bool weighted = false) {
    int n = adjacency.n_rows;

    arma::mat dist(n, n);
    dist.fill(R_PosInf);

    // Initialize distances
    if (weighted) {
        double max_weight = adjacency.max();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i == j) {
                    dist(i, j) = 0;
                } else if (adjacency(i, j) != 0) {
                    dist(i, j) = max_weight / std::abs(adjacency(i, j));
                }
            }
        }
    } else {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i == j) {
                    dist(i, j) = 0;
                } else if (adjacency(i, j) != 0) {
                    dist(i, j) = 1;
                }
            }
        }
    }

    // Floyd-Warshall with optimized cache access
    for (int k = 0; k < n; k++) {
        for (int i = 0; i < n; i++) {
            double dist_ik = dist(i, k);
            if (!std::isfinite(dist_ik)) continue;

            for (int j = 0; j < n; j++) {
                double new_dist = dist_ik + dist(k, j);
                if (new_dist < dist(i, j)) {
                    dist(i, j) = new_dist;
                }
            }
        }
    }

    return dist;
}

// Fast clustering coefficient computation
// [[Rcpp::export]]
arma::vec clustering_coefficient_cpp(const arma::mat& adjacency, bool weighted = false, int n_cores = 1) {
    int n = adjacency.n_rows;
    arma::vec cc(n, arma::fill::zeros);

    #ifdef _OPENMP
    omp_set_num_threads(n_cores);
    #endif

    #pragma omp parallel for
    for (int i = 0; i < n; i++) {
        // Find neighbors
        std::vector<int> neighbors;
        for (int j = 0; j < n; j++) {
            if (adjacency(i, j) != 0 && i != j) {
                neighbors.push_back(j);
            }
        }

        int k = neighbors.size();
        if (k < 2) {
            cc(i) = 0;
            continue;
        }

        if (weighted) {
            // Weighted clustering coefficient (Onnela et al., 2005)
            double max_weight = adjacency.max();
            double tri_sum = 0;

            for (size_t ji = 0; ji < neighbors.size(); ji++) {
                for (size_t li = 0; li < neighbors.size(); li++) {
                    if (ji != li) {
                        int nj = neighbors[ji];
                        int nl = neighbors[li];
                        if (adjacency(nj, nl) != 0) {
                            double w_ij = std::pow(std::abs(adjacency(i, nj)) / max_weight, 1.0/3.0);
                            double w_il = std::pow(std::abs(adjacency(i, nl)) / max_weight, 1.0/3.0);
                            double w_jl = std::pow(std::abs(adjacency(nj, nl)) / max_weight, 1.0/3.0);
                            tri_sum += w_ij * w_il * w_jl;
                        }
                    }
                }
            }
            cc(i) = tri_sum / (k * (k - 1));
        } else {
            // Binary clustering coefficient
            int n_triangles = 0;
            for (size_t ji = 0; ji < neighbors.size(); ji++) {
                for (size_t li = ji + 1; li < neighbors.size(); li++) {
                    if (adjacency(neighbors[ji], neighbors[li]) != 0) {
                        n_triangles++;
                    }
                }
            }
            cc(i) = (2.0 * n_triangles) / (k * (k - 1));
        }
    }

    return cc;
}

// Fast node degree computation
// [[Rcpp::export]]
arma::vec node_degree_cpp(const arma::mat& adjacency, bool weighted = false) {
    int n = adjacency.n_rows;
    arma::vec degrees(n);

    if (weighted) {
        for (int i = 0; i < n; i++) {
            degrees(i) = arma::accu(arma::abs(adjacency.row(i))) - std::abs(adjacency(i, i));
        }
    } else {
        for (int i = 0; i < n; i++) {
            int count = 0;
            for (int j = 0; j < n; j++) {
                if (adjacency(i, j) != 0 && i != j) {
                    count++;
                }
            }
            degrees(i) = count;
        }
    }

    return degrees;
}

// Fast betweenness centrality using Brandes algorithm
// [[Rcpp::export]]
arma::vec betweenness_centrality_cpp(const arma::mat& adjacency, bool normalized = true, int n_cores = 1) {
    int n = adjacency.n_rows;
    arma::vec bc(n, arma::fill::zeros);

    #ifdef _OPENMP
    omp_set_num_threads(n_cores);
    #endif

    #pragma omp parallel
    {
        arma::vec bc_local(n, arma::fill::zeros);

        #pragma omp for
        for (int s = 0; s < n; s++) {
            // BFS from source s
            std::vector<int> stack;
            std::vector<std::vector<int>> pred(n);
            std::vector<int> sigma(n, 0);
            std::vector<int> dist(n, -1);

            sigma[s] = 1;
            dist[s] = 0;

            std::queue<int> Q;
            Q.push(s);

            while (!Q.empty()) {
                int v = Q.front();
                Q.pop();
                stack.push_back(v);

                for (int w = 0; w < n; w++) {
                    if (adjacency(v, w) != 0 && v != w) {
                        // First time visiting w
                        if (dist[w] < 0) {
                            dist[w] = dist[v] + 1;
                            Q.push(w);
                        }
                        // Shortest path to w via v
                        if (dist[w] == dist[v] + 1) {
                            sigma[w] += sigma[v];
                            pred[w].push_back(v);
                        }
                    }
                }
            }

            // Backpropagate dependencies
            std::vector<double> delta(n, 0);
            while (!stack.empty()) {
                int w = stack.back();
                stack.pop_back();

                for (int v : pred[w]) {
                    delta[v] += (static_cast<double>(sigma[v]) / sigma[w]) * (1 + delta[w]);
                }
                if (w != s) {
                    bc_local(w) += delta[w];
                }
            }
        }

        #pragma omp critical
        bc += bc_local;
    }

    if (normalized && n > 2) {
        bc /= ((n - 1) * (n - 2));
    }

    return bc;
}

// Fast BFS-based cluster detection
// [[Rcpp::export]]
List find_clusters_cpp(const arma::mat& t_matrix, double threshold, int tail = 0) {
    int n_time = t_matrix.n_rows;
    int n_channels = t_matrix.n_cols;

    // Create significance mask
    arma::mat sig_mask(n_time, n_channels, arma::fill::zeros);

    for (int i = 0; i < n_time; i++) {
        for (int j = 0; j < n_channels; j++) {
            double val = t_matrix(i, j);
            if (std::isfinite(val)) {
                if (tail == 0) {
                    sig_mask(i, j) = (std::abs(val) > threshold) ? 1 : 0;
                } else if (tail == 1) {
                    sig_mask(i, j) = (val > threshold) ? 1 : 0;
                } else {
                    sig_mask(i, j) = (val < -threshold) ? 1 : 0;
                }
            }
        }
    }

    // Find connected components using BFS
    arma::mat visited(n_time, n_channels, arma::fill::zeros);
    List clusters;

    // Direction vectors for 4-connectivity
    int dx[] = {-1, 1, 0, 0};
    int dy[] = {0, 0, -1, 1};

    for (int start_t = 0; start_t < n_time; start_t++) {
        for (int start_ch = 0; start_ch < n_channels; start_ch++) {
            if (sig_mask(start_t, start_ch) > 0 && visited(start_t, start_ch) == 0) {
                // BFS to find cluster
                std::vector<int> cluster_t, cluster_ch;
                std::queue<std::pair<int, int>> Q;

                Q.push({start_t, start_ch});
                visited(start_t, start_ch) = 1;

                while (!Q.empty()) {
                    auto [t, ch] = Q.front();
                    Q.pop();

                    cluster_t.push_back(t + 1);  // 1-indexed for R
                    cluster_ch.push_back(ch + 1);

                    for (int d = 0; d < 4; d++) {
                        int nt = t + dx[d];
                        int nch = ch + dy[d];

                        if (nt >= 0 && nt < n_time && nch >= 0 && nch < n_channels) {
                            if (sig_mask(nt, nch) > 0 && visited(nt, nch) == 0) {
                                visited(nt, nch) = 1;
                                Q.push({nt, nch});
                            }
                        }
                    }
                }

                if (!cluster_t.empty()) {
                    clusters.push_back(List::create(
                        Named("time") = wrap(cluster_t),
                        Named("channel") = wrap(cluster_ch)
                    ));
                }
            }
        }
    }

    return clusters;
}

// Fast eigenvector centrality using power iteration
// [[Rcpp::export]]
arma::vec eigenvector_centrality_cpp(const arma::mat& adjacency, int max_iter = 100, double tol = 1e-6) {
    int n = adjacency.n_rows;

    // Make sure adjacency has no self-loops for centrality
    arma::mat adj = adjacency;
    adj.diag().zeros();

    arma::vec x(n, arma::fill::ones);
    x /= arma::norm(x);

    for (int iter = 0; iter < max_iter; iter++) {
        arma::vec x_new = adj * x;
        double norm_val = arma::norm(x_new);

        if (norm_val > 0) {
            x_new /= norm_val;
        }

        if (arma::max(arma::abs(x_new - x)) < tol) {
            x = x_new;
            break;
        }
        x = x_new;
    }

    // Ensure non-negative
    if (arma::accu(x) < 0) {
        x = -x;
    }

    return x;
}

// Fast global efficiency computation
// [[Rcpp::export]]
double global_efficiency_cpp(const arma::mat& adjacency, bool weighted = false) {
    int n = adjacency.n_rows;
    if (n < 2) return 0;

    arma::mat dist = floyd_warshall_cpp(adjacency, weighted);

    double eff_sum = 0;
    int count = 0;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j && std::isfinite(dist(i, j)) && dist(i, j) > 0) {
                eff_sum += 1.0 / dist(i, j);
                count++;
            }
        }
    }

    return (count > 0) ? eff_sum / (n * (n - 1)) : 0;
}
