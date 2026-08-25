.onAttach <- function(libname, pkgname) {
  core <- c("PhysioCore", "PhysioIO", "PhysioPreprocess", "PhysioAnalysis")
  packageStartupMessage(
    "PhysioExperiment ", utils::packageVersion("PhysioExperiment"),
    " -- meta-package for the Physio ecosystem.\n",
    "  Re-exports: ", paste(core, collapse = ", "), "."
  )
}
