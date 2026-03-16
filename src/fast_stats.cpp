// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;

// Fast t-statistic matrix computation for one-sample t-test
// [[Rcpp::export]]
arma::mat compute_t_matrix_one_sample_cpp(const arma::cube& data, double mu = 0.0, int n_cores = 1) {
    int n_time = data.n_rows;
    int n_channels = data.n_cols;
    int n_epochs = data.n_slices;

    arma::mat t_matrix(n_time, n_channels, arma::fill::zeros);

    #ifdef _OPENMP
    omp_set_num_threads(n_cores);
    #endif

    #pragma omp parallel for collapse(2)
    for (int t = 0; t < n_time; t++) {
        for (int ch = 0; ch < n_channels; ch++) {
            arma::vec vals(n_epochs);
            int valid_count = 0;

            for (int e = 0; e < n_epochs; e++) {
                double val = data(t, ch, e);
                if (!std::isnan(val)) {
                    vals(valid_count++) = val;
                }
            }

            if (valid_count >= 2) {
                vals = vals.head(valid_count);
                double m = arma::mean(vals);
                double s = arma::stddev(vals);

                if (s > 0) {
                    t_matrix(t, ch) = (m - mu) / (s / std::sqrt(valid_count));
                }
            }
        }
    }

    return t_matrix;
}

// Fast t-statistic matrix computation for two-sample t-test (Welch's)
// [[Rcpp::export]]
arma::mat compute_t_matrix_two_sample_cpp(const arma::cube& data1, const arma::cube& data2, int n_cores = 1) {
    int n_time = data1.n_rows;
    int n_channels = data1.n_cols;

    arma::mat t_matrix(n_time, n_channels, arma::fill::zeros);

    #ifdef _OPENMP
    omp_set_num_threads(n_cores);
    #endif

    #pragma omp parallel for collapse(2)
    for (int t = 0; t < n_time; t++) {
        for (int ch = 0; ch < n_channels; ch++) {
            // Collect valid values from group 1
            arma::vec vals1(data1.n_slices);
            int n1 = 0;
            for (unsigned int e = 0; e < data1.n_slices; e++) {
                double val = data1(t, ch, e);
                if (!std::isnan(val)) {
                    vals1(n1++) = val;
                }
            }

            // Collect valid values from group 2
            arma::vec vals2(data2.n_slices);
            int n2 = 0;
            for (unsigned int e = 0; e < data2.n_slices; e++) {
                double val = data2(t, ch, e);
                if (!std::isnan(val)) {
                    vals2(n2++) = val;
                }
            }

            if (n1 >= 2 && n2 >= 2) {
                vals1 = vals1.head(n1);
                vals2 = vals2.head(n2);

                double m1 = arma::mean(vals1);
                double m2 = arma::mean(vals2);
                double s1 = arma::stddev(vals1);
                double s2 = arma::stddev(vals2);

                double se = std::sqrt(s1*s1/n1 + s2*s2/n2);

                if (se > 0) {
                    t_matrix(t, ch) = (m1 - m2) / se;
                }
            }
        }
    }

    return t_matrix;
}

// Fast p-value computation from t-statistic matrix
// [[Rcpp::export]]
arma::mat compute_pvalues_cpp(const arma::mat& t_matrix, const arma::mat& df_matrix,
                               const std::string& alternative = "two.sided") {
    int n_time = t_matrix.n_rows;
    int n_channels = t_matrix.n_cols;

    arma::mat p_matrix(n_time, n_channels, arma::fill::zeros);
    p_matrix.fill(NA_REAL);

    for (int i = 0; i < n_time; i++) {
        for (int j = 0; j < n_channels; j++) {
            double t = t_matrix(i, j);
            double df = df_matrix(i, j);

            if (std::isfinite(t) && std::isfinite(df) && df > 0) {
                if (alternative == "two.sided") {
                    p_matrix(i, j) = 2 * R::pt(-std::abs(t), df, 1, 0);
                } else if (alternative == "less") {
                    p_matrix(i, j) = R::pt(t, df, 1, 0);
                } else {
                    p_matrix(i, j) = R::pt(t, df, 0, 0);
                }
            }
        }
    }

    return p_matrix;
}
