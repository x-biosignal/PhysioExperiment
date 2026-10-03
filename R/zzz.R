#' Package on-load hook
#'
#' Initializes the plugin registry (see \code{?`physio-registry`}) so downstream
#' packages can register readers/writers/operations from their own
#' \code{.onLoad}. \code{.ensure_registry()} is idempotent, so re-loading a
#' session (e.g. via \code{devtools::load_all}) leaves existing registrations
#' intact.
#'
#' @param libname Library path.
#' @param pkg Package name.
#' @keywords internal
.onLoad <- function(libname, pkg) {
  .ensure_registry()
  # Register PhysioLongitudinal <-> MultiAssayExperiment coercions only when the
  # (optional) MultiAssayExperiment package is available, so PhysioExperiment loads
  # cleanly without it.
  if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    methods::setAs("PhysioLongitudinal", "MultiAssayExperiment",
                   function(from) .plToMAE(from))
    methods::setAs("MultiAssayExperiment", "PhysioLongitudinal",
                   function(from) .maeToPL(from))
  }
  invisible(NULL)
}
