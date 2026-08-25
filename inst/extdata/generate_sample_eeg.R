#!/usr/bin/env Rscript
# Generate sample EEG-like CSV data for PhysioExperiment
#
# Run this script from the package root directory to regenerate the sample data:
#   Rscript inst/extdata/generate_sample_eeg.R
#
# Or from any directory:
#   Rscript /path/to/PhysioExperiment/inst/extdata/generate_sample_eeg.R

set.seed(42)

sr <- 250          # Sampling rate in Hz
n  <- 500          # Number of time points
t  <- (0:(n - 1)) / sr

# Dominant frequencies for each channel (Hz)
freqs <- c(Fz = 10, Cz = 8, Pz = 12, Oz = 6)

# Generate sinusoidal signals with Gaussian noise (sd = 0.3)
signals <- sapply(freqs, function(f) {
  sin(2 * pi * f * t) + rnorm(n, sd = 0.3)
})

df <- data.frame(time = t, signals)

# Determine output path: same directory as this script
script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) NULL
)

if (is.null(script_dir) || !dir.exists(script_dir)) {
  # Fall back: try inst/extdata/ relative to working directory
  if (dir.exists("inst/extdata")) {
    output_path <- "inst/extdata/sample_eeg.csv"
  } else {
    output_path <- "sample_eeg.csv"
  }
} else {
  output_path <- file.path(script_dir, "sample_eeg.csv")
}

write.csv(df, output_path, row.names = FALSE)
cat("Wrote", nrow(df), "rows to", output_path, "\n")
