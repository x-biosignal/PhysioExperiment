# PhysioExperiment

[![R-CMD-check](https://github.com/matsui-lab/PhysioExperiment/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/matsui-lab/PhysioExperiment/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/matsui-lab/PhysioExperiment/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/matsui-lab/PhysioExperiment/actions/workflows/test-coverage.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

A unified **SummarizedExperiment-based data model** for physiological and multi-modal sensor data
(EEG, EMG, IMU, MoCap, etc.) — developed under the [signalverse](https://github.com/signalverse) ecosystem.

---

## Prerequisites

### System Requirements

| Dependency | Purpose | Install (Ubuntu/Debian) |
|------------|---------|------------------------|
| R >= 4.2 | Runtime | `sudo apt install r-base` |
| C++ compiler (C++11) | Rcpp/RcppArmadillo | `sudo apt install build-essential` |
| BLAS / LAPACK | Linear algebra | `sudo apt install liblapack-dev libblas-dev` |
| HDF5 | HDF5 file support | `sudo apt install libhdf5-dev` |
| libxml2 | XML parsing | `sudo apt install libxml2-dev` |
| libcurl | HTTP support | `sudo apt install libcurl4-openssl-dev` |
| OpenSSL | TLS support | `sudo apt install libssl-dev` |

<details>
<summary>macOS (Homebrew)</summary>

```bash
brew install r hdf5 libxml2 openssl
```

BLAS/LAPACK are included with the macOS Accelerate framework.
</details>

<details>
<summary>Windows</summary>

Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version. Rtools bundles a C++ compiler, BLAS, and LAPACK.
</details>

### Optional: GUI

The React-based GUI requires:

| Dependency | Version | Install |
|------------|---------|---------|
| Node.js | >= 20 LTS | [https://nodejs.org](https://nodejs.org) or `nvm install` (see `inst/gui/.nvmrc`) |
| npm | >= 9 | Bundled with Node.js |

### Optional: OpenSim Integration

For musculoskeletal simulation via [PhysioOpenSim](physio-ecosystem/PhysioOpenSim/):

```bash
export OPENSIM_HOME=/path/to/opensim  # or install via pkg-config
```

---

## Installation

### From GitHub

```r
install.packages("remotes")
remotes::install_github("matsui-lab/PhysioExperiment")
```

### From Local Clone

```bash
git clone https://github.com/matsui-lab/PhysioExperiment.git
cd PhysioExperiment
```

```r
remotes::install_local(".")
```

### GUI Build (optional)

```bash
cd inst/gui
npm ci          # install locked dependencies
npm run build   # production build -> inst/gui/dist/
```

### Docker

```bash
docker build -t physioexperiment .
docker run -p 8000:8000 physioexperiment
```

### Install Optional R Packages

```r
# GUI server
install.packages(c("plumber", "later", "callr"))

# Test coverage
install.packages("covr")
```

---

## Quick Start

```r
library(PhysioExperiment)

# create a minimal example
pe <- PhysioExperiment(
  assays = SimpleList(raw = array(rnorm(1000), dim = c(100, 5, 2))),
  rowData = DataFrame(sensor_type = rep("EMG", 5)),
  colData = DataFrame(sample_id = c("S1", "S2")),
  samplingRate = 1000
)

plotSignal(pe, channel = 1, sample = 1)
```

### Launch GUI

```r
# Check dependencies
checkGUIDependencies()

# Launch in browser
launchGUI()

# API-only mode (non-blocking)
server <- startAPIServer()
# server$kill()  # to stop
```

---

## Features

* Multi-modal data integration (EEG, EMG, IMU, fNIRS, MoCap)
* Time-Frequency-Spatial multi-axis data model
* File I/O: EDF, BDF, BrainVision, GDF, HDF5, BIDS, CSV, MATLAB
* Signal processing: filtering, artifact removal, epoching, resampling
* Time-frequency analysis: spectrograms, wavelets, band power
* Connectivity analysis: coherence, PLV, PLI
* Statistical testing: t-test, ANOVA, SPM1D, cluster permutation
* Network metrics with C++ acceleration (OpenMP)
* DuckDB / HDF5 backends for large-scale data
* React-based GUI with REST API

---

## Part of signalverse

| Package            | Role                    |
| ------------------ | ----------------------- |
| `PhysioExperiment` | Core data model         |
| `signalOps`        | Processing functions    |
| `signalVis`        | Visualization utilities |
| `signalHub`        | Data sharing            |

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
```

---

## License

MIT © Matsui Lab (Nagoya University)
