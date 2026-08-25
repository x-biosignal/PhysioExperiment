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
