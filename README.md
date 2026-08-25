# PhysioExperiment

[![R-CMD-check](https://github.com/x-biosignal/PhysioExperiment/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/x-biosignal/PhysioExperiment/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![r-universe](https://x-biosignal.r-universe.dev/badges/PhysioExperiment)](https://x-biosignal.r-universe.dev/PhysioExperiment)

**A unified R/Bioconductor interface for physiological signal analysis.**

## Overview

PhysioExperiment is the umbrella package for the
[x-biosignal](https://github.com/x-biosignal) R ecosystem. Attaching it loads
and re-exports the public APIs of four foundation packages:

| Package | Responsibility |
|---|---|
| [PhysioCore](https://github.com/x-biosignal/PhysioCore) | `PhysioExperiment` data model, events, provenance, and operations |
| [PhysioIO](https://github.com/x-biosignal/PhysioIO) | File, database, BIDS, and HDF5 input/output |
| [PhysioPreprocess](https://github.com/x-biosignal/PhysioPreprocess) | Filtering, resampling, artifact handling, and ICA |
| [PhysioAnalysis](https://github.com/x-biosignal/PhysioAnalysis) | Time-frequency, connectivity, network, statistics, and visualization |

The package also provides the ecosystem GUI and REST launchers. Focused
packages for individual modalities, biomechanics, streaming, machine learning,
clinical research, and reporting are installed separately.

## Installation

Install the umbrella and its four foundation packages from r-universe:

```r
install.packages(
  "PhysioExperiment",
  repos = c(
    "https://x-biosignal.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

The development snapshot can also be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("x-biosignal/PhysioExperiment")
```

## Quick Start

```r
library(PhysioExperiment)

set.seed(42)
eeg <- matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)
colnames(eeg) <- c("Fz", "Cz", "Pz", "Oz")

pe <- PhysioExperiment(
  assays = list(raw = eeg),
  colData = S4Vectors::DataFrame(
    label = colnames(eeg),
    type = rep("EEG", 4)
  ),
  samplingRate = 250
)

filtered <- butterworthFilter(
  pe,
  low = 1,
  high = 40,
  type = "pass"
)

spectrum <- fftSignals(filtered)
power <- bandPower(filtered, method = "welch")

SummarizedExperiment::assayNames(spectrum)
power
```

For interactive exploration, install the suggested GUI dependencies and run:

```r
launchGUI()
```

## Ecosystem

Each package is maintained in its own public repository and can be installed
independently from [x-biosignal r-universe](https://x-biosignal.r-universe.dev).

| Package | Scope |
|---|---|
| [PhysioExperiment](https://github.com/x-biosignal/PhysioExperiment) | Umbrella interface and GUI/REST launchers |
| [PhysioCore](https://github.com/x-biosignal/PhysioCore) | Core data structures |
| [PhysioIO](https://github.com/x-biosignal/PhysioIO) | File and database input/output |
| [PhysioPreprocess](https://github.com/x-biosignal/PhysioPreprocess) | Signal preprocessing |
| [PhysioAnalysis](https://github.com/x-biosignal/PhysioAnalysis) | Analysis and visualization |
| [PhysioEEG](https://github.com/x-biosignal/PhysioEEG) | EEG analysis |
| [PhysioEMG](https://github.com/x-biosignal/PhysioEMG) | EMG and muscle synergy analysis |
| [PhysioECG](https://github.com/x-biosignal/PhysioECG) | ECG and heart-rate variability |
| [PhysioEDA](https://github.com/x-biosignal/PhysioEDA) | Electrodermal activity |
| [PhysioNIRS](https://github.com/x-biosignal/PhysioNIRS) | Near-infrared spectroscopy and SNIRF |
| [PhysioHDEMG](https://github.com/x-biosignal/PhysioHDEMG) | High-density surface EMG decomposition |
| [PhysioNeurophys](https://github.com/x-biosignal/PhysioNeurophys) | TMS and motor neurophysiology |
| [PhysioCrossModal](https://github.com/x-biosignal/PhysioCrossModal) | Cross-modal coupling |
| [PhysioMoCap](https://github.com/x-biosignal/PhysioMoCap) | Motion capture and biomechanics |
| [PhysioOpenSim](https://github.com/x-biosignal/PhysioOpenSim) | OpenSim integration |
| [PhysioMSKNet](https://github.com/x-biosignal/PhysioMSKNet) | Musculoskeletal network analysis |
| [PhysioGaitNorm](https://github.com/x-biosignal/PhysioGaitNorm) | Normative gait references |
| [PhysioHeadModels](https://github.com/x-biosignal/PhysioHeadModels) | EEG head models and forward solvers |
| [PhysioDevices](https://github.com/x-biosignal/PhysioDevices) | Wearable and laboratory device ingestion |
| [PhysioWearable](https://github.com/x-biosignal/PhysioWearable) | Free-living accelerometry |
| [PhysioStream](https://github.com/x-biosignal/PhysioStream) | Governed real-time streams |
| [PhysioML](https://github.com/x-biosignal/PhysioML) | Leakage-aware machine learning |
| [PhysioTrial](https://github.com/x-biosignal/PhysioTrial) | Trial randomization and blinding |
| [PhysioClinStats](https://github.com/x-biosignal/PhysioClinStats) | Clinical inference |
| [PhysioClinical](https://github.com/x-biosignal/PhysioClinical) | Clinical outcomes and responder analysis |
| [PhysioCompliance](https://github.com/x-biosignal/PhysioCompliance) | Evidence, privacy, and lifecycle controls |
| [PhysioReport](https://github.com/x-biosignal/PhysioReport) | Clinical report generation |
| [PhysioAnnotationHub](https://github.com/x-biosignal/PhysioAnnotationHub) | Anatomical and clinical knowledge graph |

Install a focused package with the same repository configuration:

```r
install.packages(
  c("PhysioEEG", "PhysioNIRS", "PhysioClinical"),
  repos = c(
    "https://x-biosignal.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## Development

```bash
Rscript -e "devtools::test()"
R CMD build .
R CMD check PhysioExperiment_*.tar.gz
```

## Citation

```bibtex
@software{matsui2026physioexperiment,
  author  = {Yusuke Matsui},
  title   = {{PhysioExperiment}: Unified Analysis of Physiological Signals in {R}},
  year    = {2026},
  url     = {https://github.com/x-biosignal/PhysioExperiment},
  version = {1.0.0}
}
```

## License

MIT &copy; Yusuke Matsui

## Governance and Support

- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Contributing](CONTRIBUTING.md)
- [Governance](GOVERNANCE.md)
- [Support](SUPPORT.md)
- [Security policy](SECURITY.md)
- [Deprecation and lifecycle policy](DEPRECATION.md)
