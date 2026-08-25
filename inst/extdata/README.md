# Sample Data for PhysioExperiment

## sample_eeg.csv

A small synthetic EEG-like dataset for use in examples and testing.

**Format:** Wide-format CSV with a `time` column and 4 EEG channel columns.

| Column | Description |
|--------|-------------|
| time   | Time in seconds (0 to 1.996 s at 250 Hz sampling rate) |
| Fz     | Frontal midline channel: 10 Hz sinusoid + Gaussian noise (sd = 0.3) |
| Cz     | Central midline channel: 8 Hz sinusoid + Gaussian noise (sd = 0.3) |
| Pz     | Parietal midline channel: 12 Hz sinusoid + Gaussian noise (sd = 0.3) |
| Oz     | Occipital midline channel: 6 Hz sinusoid + Gaussian noise (sd = 0.3) |

**Properties:**

- 500 time points
- 4 channels (Fz, Cz, Pz, Oz)
- Sampling rate: 250 Hz
- Duration: 2 seconds
- Signals: `sin(2 * pi * freq * t) + rnorm(n, sd = 0.3)` with R seed 42

**Usage in R:**

```r
library(PhysioExperiment)

# Load sample data
path <- system.file("extdata", "sample_eeg.csv", package = "PhysioExperiment")
pe <- readCSV(path, time_col = "time", sampling_rate = 250)
pe
```

## Regenerating the data

To regenerate `sample_eeg.csv`, run the included R script:

```bash
Rscript inst/extdata/generate_sample_eeg.R
```
