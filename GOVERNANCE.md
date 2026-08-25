# Governance

This document describes how the Physio ecosystem is governed. It applies to all
packages in the ecosystem (PhysioCore, PhysioIO, PhysioPreprocess, PhysioAnalysis,
PhysioEEG, PhysioECG, PhysioEMG, PhysioEDA, PhysioMoCap, PhysioCrossModal,
PhysioMSKNet, PhysioOpenSim, PhysioAnnotationHub, PhysioClinical, PhysioClinStats,
PhysioReport, and the PhysioExperiment umbrella).

## Model

The Physio ecosystem currently follows a **single-maintainer (BDFL-style)**
model. The maintainer is responsible for the technical direction, release
management, and final decisions across all packages.

**Maintainer:** Yusuke Matsui — Department of Rehabilitation Medicine,
Nagoya University (<mail.to.matsui@gmail.com>).

This model is intentional for the project's current stage: a small, coherent set
of packages developed to a consistent design. As the contributor base grows, the
project may transition to a multi-maintainer or committee model; any such change
will be recorded in this document.

## Decision-making

- **Routine changes** (bug fixes, documentation, additive features that follow
  the existing design) are decided by the maintainer, usually via pull request
  review.
- **Substantial changes** (new public API, breaking changes, cross-package
  architecture, new dependencies) are proposed as a GitHub issue first, so the
  rationale and alternatives are recorded before implementation.
- **Design principles** that guide decisions: a consistent S4 data model built on
  `SummarizedExperiment`; reproducibility and provenance; validation against
  public data; and a clean, documented public API per package.

## Contributing

Contributions are welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for development
setup, coding conventions, and the pull-request process. All participants are
expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md).

A typical contribution path:

1. Open an issue describing the bug or proposed change.
2. For non-trivial work, wait for maintainer feedback on the approach before
   implementing.
3. Submit a pull request with tests and documentation; CI (`R CMD check`) must
   pass.
4. The maintainer reviews, requests changes if needed, and merges.

## Escalation

If a decision or a Code of Conduct matter needs to be escalated, contact the
maintainer directly at <mail.to.matsui@gmail.com>. Because the project is
currently single-maintainer, there is no separate appeals body; disagreements are
resolved through discussion on the relevant issue or pull request, with the
maintainer making the final call.

## Releases and lifecycle

Public API stability is communicated with [lifecycle](https://lifecycle.r-lib.org/)
stages and badges. The deprecation and lifecycle policy — including the minimum
deprecation window — is documented in [DEPRECATION.md](DEPRECATION.md).
