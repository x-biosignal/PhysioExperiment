# PhysioExperiment

[![R-CMD-check](https://github.com/x-biosignal/PhysioExperiment/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/x-biosignal/PhysioExperiment/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![r-universe](https://x-biosignal.r-universe.dev/badges/PhysioExperiment)](https://x-biosignal.r-universe.dev/PhysioExperiment)

**The shared data model for physiological signal analysis.**

## Overview

PhysioExperiment provides the data structures the
[x-biosignal](https://github.com/x-biosignal) R ecosystem is built on. Domain
packages apply specialised methods to these shared containers.

| Container | Holds |
|---|---|
| `PhysioExperiment` | one recording: all assays, time and channel annotation, events, provenance |
| `MultiPhysioExperiment` | several streams recorded together, each at its own rate, on one shared clock |
| `PhysioLongitudinal` | repeated sessions of one subject, with their conditions and time points |
| `PhysioCohort` | several subjects, with their subject-level data |

Within one recording, repeated trials are recorded as intervals
(`trials()`, `trialIntervals()`), extracted with `trial()`, and aggregated with
`aggregateTrials()`, which keeps partial trials out of an average unless asked.
`streamCoverage()` reports what a span actually covers, so a trial that contains
a gap is stored as the fact it is and refused only by the operations that need
continuous data.

It depends on no other `Physio*` package, so it can be installed and used on its
own.

> **Moving from an earlier version.** This package used to be an umbrella that
> attached `PhysioCore`, `PhysioIO`, `PhysioPreprocess` and `PhysioAnalysis`.
> That role now belongs to
> [PhysioEcosystem](https://github.com/x-biosignal/PhysioEcosystem), which also
> provides the GUI and REST launchers. `library(PhysioCore)` still works and
> re-exports this package unchanged. See the migration guide for details.

## Installation

The containers build on Bioconductor, so its repositories have to be on the list
as well -- without them the install stops at `SummarizedExperiment`.

```r
install.packages("BiocManager", repos = "https://cloud.r-project.org")
install.packages(
  "PhysioExperiment",
  repos = c("https://x-biosignal.r-universe.dev", BiocManager::repositories())
)
```

For the whole stack -- I/O, preprocessing, analysis and the GUI/REST launchers --
install the umbrella instead:

```r
install.packages("BiocManager", repos = "https://cloud.r-project.org")
install.packages(
  "PhysioEcosystem",
  repos = c("https://x-biosignal.r-universe.dev", BiocManager::repositories())
)
```

The development snapshot can also be installed from GitHub:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("x-biosignal/PhysioExperiment")
```

## Quick Start

This package provides the containers and the operations on them. Everything here
runs with `PhysioExperiment` alone.

```r
library(PhysioExperiment)

set.seed(42)
eeg <- matrix(rnorm(1000 * 4), nrow = 1000, ncol = 4)
colnames(eeg) <- c("Fz", "Cz", "Pz", "Oz")

pe <- PhysioExperiment(
  assays = list(raw = eeg),
  colData = S4Vectors::DataFrame(label = colnames(eeg), type = rep("EEG", 4)),
  samplingRate = 250
)
samplingRate(pe)
channelNames(pe)
duration(pe)
```

Several streams recorded together, each keeping its own rate, on one shared
clock. Nothing is resampled by holding them:

```r
emg <- PhysioExperiment(
  assays = list(raw = matrix(rnorm(4000 * 2), 4000, 2)),
  samplingRate = 1000
)

mpe <- MultiPhysioExperiment(
  streams = list(eeg = pe, emg = emg),
  offsets = c(eeg = 0, emg = 0.137)   # the EMG started 137 ms later
)
streamRates(mpe)
commonClock(mpe)$offsets

# select a span by the samples' real time on the shared clock
win <- timeWindow(mpe, 1.0, 2.0)
vapply(streamTimeIndex(win), function(t) t[1], numeric(1))
```

Trials inside a recording are stored as intervals and extracted by the same
rules:

```r
trials(mpe) <- data.frame(subject_id = "P1", session_id = "S1",
                          trial_id = c("T1", "T2"), observed = TRUE)
trialIntervals(mpe) <- data.frame(
  subject_id = "P1", session_id = "S1", trial_id = c("T1", "T2"),
  recording_id = "R1", stream = "eeg",
  start = c(0.0, 2.0), end = c(1.5, 3.5)
)

trialCoverage(mpe, "T1")          # what each stream actually covers
t1 <- trial(mpe, "T1")
streamNames(t1)
```

Filtering, spectra, connectivity, I/O and the GUI live in the domain packages —
`butterworthFilter()` in `PhysioPreprocess`, `fftSignals()` and `bandPower()` in
`PhysioAnalysis`, `launchGUI()` in `PhysioEcosystem`. Install
[PhysioEcosystem](https://github.com/x-biosignal/PhysioEcosystem) to get them all
under one `library()` call; its README has the corresponding examples.

## Ecosystem

Each package is maintained in its own public repository and can be installed
independently from [x-biosignal r-universe](https://x-biosignal.r-universe.dev).

| Package | Scope |
|---|---|
| [PhysioExperiment](https://github.com/x-biosignal/PhysioExperiment) | The shared data model: containers, clock, events, provenance, trials |
| [PhysioEcosystem](https://github.com/x-biosignal/PhysioEcosystem) | Umbrella interface and GUI/REST launchers |
| [PhysioCore](https://github.com/x-biosignal/PhysioCore) | Compatibility layer re-exporting PhysioExperiment |
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
  repos = c("https://x-biosignal.r-universe.dev", BiocManager::repositories())
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
