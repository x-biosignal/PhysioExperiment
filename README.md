# PhysioExperiment

[![R-CMD-check](https://github.com/x-biosignal/PhysioExperiment/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/x-biosignal/PhysioExperiment/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![r-universe](https://x-biosignal.r-universe.dev/badges/PhysioExperiment)](https://x-biosignal.r-universe.dev/PhysioExperiment)

**A unified R/Bioconductor framework for multi-modal physiological signal analysis.**

---

## Overview

PhysioExperiment is a comprehensive R package that provides a standardized data model and integrated analysis pipeline for physiological signals. Built on Bioconductor's `SummarizedExperiment`, the `PhysioExperiment` class stores time-series data (time x channels, or time x channels x samples) alongside channel metadata, event markers, and sampling rate information in a single, interoperable object.

The package supports the full analysis workflow --- from file import through preprocessing, time-frequency analysis, connectivity estimation, network metrics, statistical testing, and publication-quality visualization --- across seven signal modalities: **EEG**, **EMG**, **ECG**, **EDA**, **IMU**, **MoCap**, and **fNIRS**. With 167 exported functions, C++ accelerated computations, and backends for large-scale data (DuckDB, HDF5), PhysioExperiment serves as either a standalone all-in-one solution or the foundation of the broader [x-biosignal](https://github.com/x-biosignal) ecosystem.

---

## Installation

### From R-universe (recommended)

```r
install.packages("PhysioExperiment",
  repos = c("https://x-biosignal.r-universe.dev", "https://cloud.r-project.org"))
```

### From GitHub

```r
install.packages("remotes")
remotes::install_github("x-biosignal/PhysioExperiment")
```

<details>
<summary><strong>System requirements</strong></summary>

| Dependency | Purpose | Install (Ubuntu/Debian) |
|---|---|---|
| R >= 4.2 | Runtime | `sudo apt install r-base` |
| C++ compiler (C++11) | Rcpp / RcppArmadillo | `sudo apt install build-essential` |
| BLAS / LAPACK | Linear algebra | `sudo apt install liblapack-dev libblas-dev` |
| HDF5 | HDF5 file support | `sudo apt install libhdf5-dev` |
| libxml2 | XML parsing | `sudo apt install libxml2-dev` |
| libcurl | HTTP support | `sudo apt install libcurl4-openssl-dev` |
| OpenSSL | TLS support | `sudo apt install libssl-dev` |

**macOS (Homebrew):**

```bash
brew install r hdf5 libxml2 openssl
```

**Windows:**

Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version. Rtools bundles the C++ compiler, BLAS, and LAPACK.

</details>

---

## Quick Start

```r
library(PhysioExperiment)

# --- Create a PhysioExperiment from EEG data ---
eeg_data <- matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)
colnames(eeg_data) <- c("Fz", "Cz", "Pz", "Oz")

pe <- PhysioExperiment(
  assays  = list(raw = eeg_data),
  colData = S4Vectors::DataFrame(
    label = c("Fz", "Cz", "Pz", "Oz"),
    type  = rep("EEG", 4)
  ),
  samplingRate = 250
)

# --- Or read from a file ---
pe <- readEDF("recording.edf")

# --- Preprocessing pipeline ---
pe <- filterSignals(pe, lowcut = 1, highcut = 40, order = 4)
pe <- reReference(pe, ref = "average")
pe <- detectArtifacts(pe, method = "threshold", threshold = 100)

# --- Epoch around events ---
pe_epochs <- epochData(pe, events = events(pe), tmin = -0.2, tmax = 0.8)

# --- Compute ERP and visualize ---
pe_avg <- grandAverage(pe_epochs)
plotERP(pe_avg, channels = c("Fz", "Cz", "Pz"))

# --- Time-frequency analysis ---
tf <- computeSpectrogram(pe, method = "wavelet")
plotSpectrogram(tf, channel = "Cz")

# --- Connectivity & network analysis ---
conn <- computeConnectivity(pe, method = "wpli")
net  <- computeNetworkMetrics(conn)  # C++ accelerated
plotNetwork(conn, layout = "circle")
```

---

## Key Features

### Data Model

The `PhysioExperiment` S4 class extends `SummarizedExperiment` with a `samplingRate` slot, providing a unified container for physiological data:

```
PhysioExperiment
 |-- assays       Named list of matrices/arrays (time x channels [x samples])
 |     |-- "raw"        Original signal data
 |     |-- "filtered"   After preprocessing
 |     +-- ...          Additional processing results
 |-- colData      Channel metadata (label, type, unit, x/y/z position)
 |-- rowData      Time-point or trial metadata
 |-- metadata     Events, reference scheme, processing history
 +-- samplingRate Sampling frequency in Hz
```

### File I/O

Read and write data in all major neurophysiology and biosignal formats:

| Format | Read | Write | Function |
|---|:---:|:---:|---|
| EDF / EDF+ | x | x | `readEDF()` / `writeEDF()` |
| BDF (BioSemi) | x | | `readBDF()` |
| BrainVision | x | x | `readBrainVision()` / `writeBrainVision()` |
| GDF | x | | `readGDF()` |
| HDF5 | x | x | `readHDF5()` / `writeHDF5()` |
| BIDS | x | x | `readBIDS()` / `writeBIDS()` |
| CSV | x | x | `readCSV()` / `writeCSV()` |
| MATLAB (.mat) | x | | `readMAT()` |

### Signal Preprocessing

- **Filtering:** Butterworth (bandpass, lowpass, highpass), FIR, notch filters
- **Artifact detection:** Threshold, gradient, kurtosis, and spectral methods
- **ICA:** Independent Component Analysis for artifact removal
- **Re-referencing:** Average, linked mastoid, bipolar, Laplacian
- **Resampling:** Upsampling and downsampling with anti-aliasing

### Epoching

- **Event-locked epoching:** Extract segments time-locked to stimulus/response events
- **Sliding window:** Overlapping or non-overlapping fixed-length segments
- **Grand averaging:** Across trials, subjects, or conditions
- **Baseline correction:** Subtract pre-stimulus mean

### Time-Frequency Analysis

- **FFT:** Power spectral density estimation
- **Spectrograms:** Short-time Fourier transform (STFT)
- **Wavelets:** Continuous wavelet transform (Morlet)
- **Band power:** Delta, theta, alpha, beta, gamma extraction
- **Hilbert transform:** Instantaneous amplitude and phase

### Connectivity Analysis

- **Coherence:** Magnitude-squared coherence
- **Phase-Locking Value (PLV)**
- **Phase Lag Index (PLI)**
- **Weighted Phase Lag Index (wPLI)**
- **Sliding-window connectivity:** Time-varying functional connectivity

### Network Metrics (C++ accelerated)

Graph-theoretic measures computed via Rcpp/RcppArmadillo for performance:

- Degree, strength, betweenness centrality
- Clustering coefficient
- Modularity (Louvain algorithm)
- Small-worldness (sigma, omega)
- Global and local efficiency

### Statistical Testing

- **Parametric:** t-test, paired t-test, one-way / repeated-measures ANOVA
- **SPM1D:** Statistical Parametric Mapping for 1D continuum data
- **Cluster-based permutation tests:** Family-wise error control for high-dimensional data
- **Effect size:** Cohen's d, partial eta-squared
- **Bootstrap confidence intervals**

### Visualization

- Signal traces (single and multi-channel)
- Event-related potentials (ERP)
- Power spectral density (PSD) plots
- Topographic maps (topomaps)
- Spectrograms (time-frequency images)
- Network / connectivity plots
- All built on `ggplot2` for easy customization

### Large-Scale Data

- **DuckDB backend:** SQL queries over large datasets without loading into memory
- **HDF5 backend:** Disk-backed arrays via `HDF5Array` / `DelayedArray`

### Interactive GUI

A React-based graphical interface with a REST API backend (Plumber) for interactive exploration:

```r
launchGUI()
```

---

## x-biosignal Ecosystem

PhysioExperiment is the all-in-one monolithic package within the [x-biosignal](https://github.com/x-biosignal) ecosystem. For modular workflows, the same functionality is available as separate focused packages:

| Package | Role | Repository |
|---|---|---|
| **PhysioExperiment** | All-in-one monolithic package | [x-biosignal/PhysioExperiment](https://github.com/x-biosignal/PhysioExperiment) |
| PhysioCore | Core data structures (S4 class) | [x-biosignal/PhysioCore](https://github.com/x-biosignal/PhysioCore) |
| PhysioIO | File I/O (EDF, HDF5, BIDS, etc.) | [x-biosignal/PhysioIO](https://github.com/x-biosignal/PhysioIO) |
| PhysioPreprocess | Preprocessing (filters, ICA, resampling) | [x-biosignal/PhysioPreprocess](https://github.com/x-biosignal/PhysioPreprocess) |
| PhysioAnalysis | Analysis and visualization | [x-biosignal/PhysioAnalysis](https://github.com/x-biosignal/PhysioAnalysis) |
| PhysioEEG | EEG-specific analysis | [x-biosignal/PhysioEEG](https://github.com/x-biosignal/PhysioEEG) |
| PhysioEMG | EMG analysis and muscle synergy | [x-biosignal/PhysioEMG](https://github.com/x-biosignal/PhysioEMG) |
| PhysioECG | ECG and HRV analysis | [x-biosignal/PhysioECG](https://github.com/x-biosignal/PhysioECG) |
| PhysioEDA | Electrodermal activity analysis | [x-biosignal/PhysioEDA](https://github.com/x-biosignal/PhysioEDA) |
| PhysioCrossModal | Cross-modal coupling | [x-biosignal/PhysioCrossModal](https://github.com/x-biosignal/PhysioCrossModal) |
| PhysioMoCap | Motion capture and biomechanics | [x-biosignal/PhysioMoCap](https://github.com/x-biosignal/PhysioMoCap) |
| PhysioOpenSim | OpenSim C++ bridge | [x-biosignal/PhysioOpenSim](https://github.com/x-biosignal/PhysioOpenSim) |
| PhysioMSKNet | Musculoskeletal network analysis | [x-biosignal/PhysioMSKNet](https://github.com/x-biosignal/PhysioMSKNet) |
| PhysioAnnotationHub | Anatomical knowledge graph | [x-biosignal/PhysioAnnotationHub](https://github.com/x-biosignal/PhysioAnnotationHub) |

All packages are available from [x-biosignal.r-universe.dev](https://x-biosignal.r-universe.dev).

---

## Development

```bash
# Run tests
Rscript -e "devtools::test()"

# Full package check
R CMD check .

# Regenerate documentation
Rscript -e "roxygen2::roxygenise()"

# GUI development (hot reload)
cd inst/gui && npm run dev

# GUI production build
cd inst/gui && npm run build
```

---

## Citation

If you use PhysioExperiment in your research, please cite:

```bibtex
@software{matsui2026physioexperiment,
  author    = {Yusuke Matsui},
  title     = {{PhysioExperiment}: Unified Analysis of Physiological Signals in {R}},
  year      = {2026},
  url       = {https://github.com/x-biosignal/PhysioExperiment},
  version   = {1.0.0}
}
```

---

## License

MIT &copy; Yusuke Matsui
