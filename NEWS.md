# PhysioExperiment 2.3.1

- `tests/testthat/test-trial-features.R` is shipped. It was left untracked when
  `extractTrialFeatures()` was committed, so 2.3.0 published the feature without
  its 65 assertions. The package's own suite goes from 1211 to 1276 with it
  present. `sync_public.sh` publishes `git archive` from HEAD, so an untracked
  file is invisible to it however green the working tree looks, while
  `devtools::test()` reads the working directory and does not distinguish the
  two — the check that catches this is the built tarball's file list, not the
  test count.

# PhysioExperiment 2.3.0

## New features

- Added `extractTrialFeatures()` to apply named feature functions across trial,
  session and participant containers, with coverage-based trial selection,
  channel-name matching and references to original samples. Its result can be
  passed directly to `aggregatePhysioFeatures()`; selections and source links
  remain available through subsequent aggregation levels.

# PhysioExperiment 2.2.0

## New features

- Added `aggregateTrialWaveforms()` for explicit trial-relative or phase-grid
  alignment and equal-weight session, subject and cohort waveform means.
  Retains trial selections, pointwise contributor counts and interpolation
  lineage. Gaps, unavailable support and nonfinite samples are not interpolated
  across; pointwise omission requires `na.rm = TRUE`. Windowed inputs require
  explicit alignment anchors. No inferential uncertainty is estimated.

# PhysioExperiment 2.1.4

## Bug fixes

- `aggregateTrials()` ignored the one thing `streamCoverage()` reports about what
  it could not do. A gap recorded by an earlier `timeWindow()` is reusable only
  under the rate and `gap_factor` it was detected with; queried with
  `gap_factor = 1.6` a record written under 1.5 comes back as
  `unusable_carried_gaps = 1` and no gap. The classifier read only the gap count
  and the sample total, so the trial that was `partial` on the original container
  became `complete` after windowing and was taken by the default aggregation.
  Losing the evidence promoted it.

  The states are now decided in a stated order: shown to be short (a gap or a
  shortfall this query can demonstrate), then unable to tell (an unusable record,
  undecidable coverage, or more samples than the declared grid predicts), then
  shown to be whole. A question that can no longer be answered is not an answer of
  yes; and a shortfall that is still demonstrable outranks a record that cannot be
  reused, so a partial trial is not softened to `unknown` either.

# PhysioExperiment 2.1.3

## Bug fixes

- `streamCoverage()` selected the window's samples and differenced those, so a
  gap straddling an edge of the window was never seen: one of its two bounding
  samples lies outside the window, and the step across it is therefore never
  formed. On a 10 Hz stream with a 0.2 s hole between 0.2 and 0.4 s, the window
  `[0.25, 0.45]` reported two samples of an expected two, no gap, and
  `aggregateTrials()` took the trial as complete -- while PhysioCrossModal refused
  the same window for the same recording. Detection now runs on the whole series
  and the window selects afterwards, which is the order CrossModal already used.

- The detection is no longer written twice. It is `detectGaps()`, newly exported,
  and `PhysioCrossModal::.reject_gap_in_window()` calls it. Sharing the 1.5x
  criterion was not enough to make two implementations agree, which is how the
  disagreement above survived a session that had checked the factor was shared.

- `timeWindow()` records the gaps it is about to make undetectable. Subsetting
  removes the sample that bounded a straddling gap from outside, so after a window
  the series can no longer show it; the record is then the only place it survives,
  and a later coverage query merges it in. Gaps recorded under a different rate or
  `gap_factor` cannot be re-derived, and are reported as such rather than dropped.
  `[` keeps only the rows of the streams it retains.

# PhysioExperiment 2.1.2

## Bug fixes

- `streamCoverage()` computed `n_expected` as `floor((end - start) * rate) + 1`,
  which measures a span rather than a grid and overstates the count whenever the
  bounds do not land on sample times. A whole, evenly sampled 10 Hz stream asked
  about `[0.01, 1.01]` holds ten samples and ten grid points, but was told to
  expect eleven, so it read as 0.909 covered and `aggregateTrials()` called the
  trial partial. The expected count now counts the reference grid points inside
  the interval, at step `1/rate` anchored on the stream's first sample time, with
  the same tolerance as `timeWindow()`. For a measured stream that anchor is an
  assumption, so `rule$anchor`, `rule$step` and `rule$basis` report which grid the
  count was taken on.

- `aggregateTrials()` did not use the gaps `streamCoverage()` reports. Partiality
  was decided by the sample total alone, so a dropout whose missing samples were
  made up by off-grid samples elsewhere in the interval passed as complete: at
  10 Hz over `[0, 1]` with a 0.2 s hole and one extra sample at 0.45, the count
  reached eleven of an expected eleven and the reported gap was ignored. A
  reported gap now makes the trial partial, which is what "one definition of a
  dropout" was supposed to mean in 2.1.1 and did not.

- A stream holding more samples in an interval than its declared rate predicts is
  classified `unknown` rather than `complete`. Nothing is missing, but the grid
  does not describe the stream, so full coverage is not established either.

## Behaviour

- `aggregateTrials(include = )` defaults to `"complete"` again. 2.1.1 had widened
  the default to `c("complete", "undescribed")`, which was a change of
  specification carried in as part of a bug fix: the rule settled during design
  was that only trials shown to be whole take part unless another state is named.
  A trial table of per-trial values with no intervals is aggregated by naming
  `include = "undescribed"`, or by calling `aggregatePhysioFeatures()` directly,
  which needs nothing from this function.

# PhysioExperiment 2.1.1

## Bug fixes

- `aggregateTrials()` treated a trial as complete unless a window had truncated
  it, so a trial whose stored interval ran past the end of the recording, or had
  a dropout inside it, was averaged in with whole trials under the default
  `include = "complete"`. On a ten-second recording a trial that `trialCoverage()`
  reported as 0.199 covered was pooled without comment, and passing
  `include = c("complete", "partial")` returned the identical result and the
  identical checksum -- the argument selected nothing. Completeness is now
  measured with `streamCoverage()`, the same rule that reports the dropout, so
  partiality from a window, from the end of the recording and from a gap are one
  definition rather than three.

- Completeness that cannot be decided is no longer reported as completeness.
  `aggregateTrials()` classifies each trial as `complete`, `partial`, `unknown`
  (intervals present, coverage undecidable) or `undescribed` (no interval
  recorded), returns the state of every trial in `states`, counts each state, and
  includes only `c("complete", "undescribed")` by default. A trial table carrying
  per-trial values and no intervals still aggregates, but is counted under its
  own name instead of as evidence of completeness.

# PhysioExperiment 2.1.0

## Trials

* `trials()` and `trialIntervals()` record that a trial happened and where it
  sits, per recording and per stream, so one trial captured by two devices can
  occupy two different spans. An interval containing a gap **is stored** -- the
  trial happened, and whether the span can be analysed is asked separately at the
  point of use. Reversed, non-finite, dangling, duplicate and wholly-outside
  intervals are refused; a trial that happened outside the recording is carried
  as `observed = FALSE`.

* `streamCoverage()` reports how much of a span a stream covers, keeping
  observation apart from derivation: `n_present` counts real samples, while
  `n_expected` and `coverage` follow a named rule and are `NA` when the rule
  cannot decide. Gaps come back as intervals rather than as a shortfall in a
  count. For a stream without measured times, "no gaps" follows from the
  reconstruction rather than from a measurement, and `rule$basis` says so.

* `trial()` extracts one trial through `timeWindow()`, so the selection, the
  boundary tolerance, the clock and the provenance are the same as for any window.
  `timeWindow()` now keeps the trial tables true instead of carrying them: partial
  trials are truncated and marked with their original bounds,
  `complete_only = TRUE` keeps only whole ones, and every excluded trial is
  recorded with its reason.

* `recordingMap()` and `elapsedBetweenTrials()` handle trials recorded
  independently. A duration is computed only when the recordings are genuinely
  placed relative to one another: a row declaring `"measured"` without a
  `reference` or an `offset` is refused, `"order_only"` and `"unknown"` are
  refused by name, and offsets on different references are refused as
  incomparable. Row order is never converted into seconds. An absent absolute
  origin (`t0 = NA`) does not block any of this -- what matters is the
  correspondence between recordings, not an absolute clock.

* `aggregateTrials()` chooses which trials take part and delegates to
  `aggregatePhysioFeatures()`. Partial trials are excluded by default rather than
  silently averaged with whole ones; `include = c("complete", "partial")` includes
  them on purpose, and the counts and exclusion reasons are returned either way.
  The existing weighting, source-row links and checksum are unchanged.

* `trialSet()` holds several independently recorded trials together, with
  `recordings()` returning the containers themselves. A map naming a recording
  that is not held is refused -- the ids are no longer labels pointing outside the
  object. Each recording keeps its own clock, so nothing is concatenated, and
  `elapsedBetweenTrials()` applies the same correspondence rules to a trial set.

* Intervals that overlap on the same recording and stream are refused;
  `allow_overlap = "<reason>"` permits them and keeps the reason. Intervals that
  merely touch are not overlaps.

* `trialCoverage()` reports one trial's coverage for every stream it names and
  flags disagreement between them, rather than deciding which stream to trust.

* `subjectTrials()` and `cohortTrials()` list trials across the hierarchy,
  carrying the session and the subject so that a recurring `trial_id` stays
  unambiguous.

* **Not implemented:** waveform averaging across trials, which needs its own
  specification.

# PhysioExperiment 2.0.0

## The multimodal container is unified

* `MultiPhysioExperiment` is the one container for several simultaneously
  recorded streams. It holds named `PhysioExperiment` streams and a single
  shared `clock`; there is no second alignment table. `alignment()` is a view
  computed from the clock and `alignment<-` writes back into it, so the two
  cannot disagree. Streams that share a sampling rate use the same class.

* The clock records `t0` (`NA` when there is no known absolute origin, rather
  than a fabricated zero), per-stream `offsets` (negatives allowed),
  `reference_rate`, and `offsets_assumed` -- which distinguishes a measured
  alignment from an assumed simultaneous start.

* `MultiRatePhysioExperiment` remains as an empty subclass and as a constructor,
  so objects saved under the old name stay valid and `is()` tests keep passing.
  The legacy accessors `experiments()`, `modalities()`, `samplingRates()`,
  `nModalities()` are kept and return their original types.

* **New `timeWindow(x, start, end)`** selects the closed interval `[start, end]`
  on the shared clock, using each sample's actual time. `x[c(start, end), ]`
  delegates to it. The former `PhysioCrossModal` `[` computed indices from the
  sampling rate alone: it ignored each stream's start offset, reset every offset
  to zero when rebuilding the alignment, and discarded the `sampleMap`. For
  windows whose bounds land on sample times -- including all zero-offset data
  with grid-aligned bounds -- the selection is unchanged.

* A stream that does not overlap the window is an error naming the stream, not a
  silently smaller container; `drop_empty = TRUE` excludes it on purpose and
  records the reason.

* Validity now rejects duplicate stream names, non-finite or non-positive rates,
  a non-scalar `t0`, and offsets that are unnamed, missing a stream, or naming
  one that does not exist. **A partially specified `offsets` is now an error**
  instead of being completed with zeros; omit `offsets` entirely to declare a
  simultaneous start.

* `migrateContainer()`, `updateObject()` and `readPhysioRDS()` convert containers
  saved before the unification. The old model had no absolute origin, so `t0`
  becomes `NA`; `sampleMap` and the cached `couplingResults` are carried into
  `clock$migrated_from` with their origin, and the cache is not reused because
  its key never covered the signals or the clock. Source files are never
  rewritten.


## Breaking change: this package is now the shared data model, not a meta-package

* `PhysioExperiment` previously existed only as an umbrella that attached
  `PhysioCore`, `PhysioIO`, `PhysioPreprocess` and `PhysioAnalysis` and
  re-exported their APIs. It now **contains** the shared data model itself: the
  `PhysioExperiment` class and the containers for multiple simultaneously
  recorded streams, repeated sessions of one subject, and cohorts of subjects,
  together with accessors, channel and event management, and provenance. This
  is the implementation that used to live in `PhysioCore`.

* **The umbrella moved to the new `PhysioEcosystem` package.** Code that called
  `library(PhysioExperiment)` to obtain the whole stack (I/O, preprocessing,
  analysis, GUI/REST launchers) should call `library(PhysioEcosystem)` instead.
  `library(PhysioExperiment)` now attaches the data model only.

* `PhysioCore` becomes a compatibility package that re-exports this one
  unchanged, so `library(PhysioCore)` and `PhysioCore::fn()` keep working --
  including `S4` objects saved while the classes were defined in `PhysioCore`,
  which still load, validate and dispatch.

* Provenance records written by this package now name `PhysioExperiment` as the
  producing package rather than `PhysioCore`, and the registry session option is
  `PhysioExperiment.registry`. Objects created before the move keep whatever
  they recorded at the time.

# PhysioExperiment 1.0.0

## New Features

* `PhysioExperiment` S4 class extending `SummarizedExperiment` with `samplingRate` slot
* File I/O: `readEDF()`, `readBrainVision()`, `readGDF()`, `readPhysioHDF5()`, `readBIDS()`, `readCSV()`, `readMAT()` and corresponding write functions
* Generic I/O dispatcher: `readPhysio()`, `writePhysio()` with automatic format detection
* Signal processing: `filterSignals()`, `butterworthFilter()`, `firFilter()`, `notchFilter()`
* FFT and time-frequency: `fftSignals()`, `spectrogram()`, `waveletTransform()`, `bandPower()`
* Epoching: `epochData()`, `epochSliding()`, `averageEpochs()`, `grandAverage()`
* Artifact handling: `detectArtifacts()`, `detectBadChannels()`, `icaDecompose()`, `icaRemove()`
* Re-referencing: `rereference()` with average, channel, and REST support
* Resampling: `resample()`, `decimate()`
* Connectivity: `coherence()`, `plv()`, `pli()`, `wPLI()`, `connectivityMatrix()`
* Network analysis: `adjacencyMatrix()`, `thresholdNetwork()`, `nodeDegree()`, `clusteringCoefficient()`, `globalEfficiency()`, `smallWorldness()`, `modularity()`
* Statistical testing: `tTestEpochs()`, `anovaEpochs()`, `clusterPermutationTest()`
* SPM1D: `spmTTest()`, `spmPairedTTest()`, `spmAnova()`
* Visualization: `plotSignal()`, `plotMultiChannel()`, `plotPSD()`, `plotERP()`, `plotSpectrogram()`, `plotTopomap()`, `plotNetwork()`
* DuckDB integration: `connectDatabase()`, `registerExperiment()`, `queryExperiments()`
* Channel management: `channelInfo()`, `pickChannels()`, `dropChannels()`, `renameChannels()`, `setChannelTypes()`
* Event system: `PhysioEvents` class with `getEvents()`, `addEvents()`, `eventQuery()`
* NA handling: `checkNA()`, `handleNA()`, `replaceNA()`, `fillEdgeNA()`
* C++ acceleration via Rcpp/RcppArmadillo for network metrics and SPM statistics
* React-based GUI with Plumber REST API backend
* HDF5-backed arrays for large datasets
