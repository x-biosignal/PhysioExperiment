#' @import methods
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<- assayNames assays assays<- rowData rowData<- colData colData<-
#' @importFrom S4Vectors DataFrame SimpleList metadata metadata<-
#' @importFrom stats sd median fft filter var cov cor na.omit
#' @importFrom abind abind
#' @importFrom ggplot2 ggplot aes geom_line geom_tile geom_point geom_ribbon geom_segment geom_rect geom_hline geom_vline geom_text geom_polygon geom_bar geom_errorbar labs scale_fill_gradient2 scale_color_manual scale_x_continuous scale_y_continuous theme_minimal theme_bw theme element_text element_blank coord_fixed coord_polar facet_wrap facet_grid annotate guides guide_legend
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head tail
#' @importFrom Rcpp evalCpp
#' @useDynLib PhysioExperiment, .registration = TRUE
NULL

.onLoad <- function(libname, pkgname) {
  # Package initialization
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "PhysioExperiment: Unified Analysis of Physiological Signals\n",
    "  Use PhysioExperiment() to create data containers\n",
    "  See vignette('quickstart') for getting started"
  )
}
