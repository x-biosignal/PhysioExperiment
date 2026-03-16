# Comprehensive error handling tests for PhysioExperiment
# This file tests that functions properly validate inputs and provide
# informative error messages.

# ---------------------------------------------------------------------
# PhysioExperiment class errors
# ---------------------------------------------------------------------

test_that("PhysioExperiment rejects negative sampling rate", {
  expect_error(
    PhysioExperiment(
      assays = list(raw = matrix(1:4, nrow = 2)),
      samplingRate = -100
    ),
    "positive"
  )
})

test_that("PhysioExperiment rejects vector sampling rate", {
  expect_error(
    PhysioExperiment(
      assays = list(raw = matrix(1:4, nrow = 2)),
      samplingRate = c(100, 200)
    ),
    "scalar"
  )
})

# ---------------------------------------------------------------------
# Event handling errors
# ---------------------------------------------------------------------

test_that("PhysioEvents rejects mismatched lengths", {
  expect_error(
    PhysioEvents(
      onset = c(1, 2, 3),
      type = c("a", "b")  # Wrong length
    ),
    "length"
  )
})

test_that("getEvents requires PhysioExperiment", {
  expect_error(getEvents(list()), "PhysioExperiment")
})

test_that("setEvents requires PhysioExperiment", {
  expect_error(setEvents(list(), PhysioEvents()), "PhysioExperiment")
})

test_that("addEvents requires PhysioExperiment", {
  expect_error(addEvents(list(), onset = 1), "PhysioExperiment")
})

test_that("timeToSamples requires valid sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:4, nrow = 2)),
    samplingRate = NA
  )
  expect_error(timeToSamples(pe, 1), "sampling rate")
})

test_that("samplesToTime requires valid sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:4, nrow = 2)),
    samplingRate = NA
  )
  expect_error(samplesToTime(pe, 1), "sampling rate")
})

# ---------------------------------------------------------------------
# Channel handling errors
# ---------------------------------------------------------------------

test_that("pickChannels errors on missing channels", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    colData = S4Vectors::DataFrame(label = c("A", "B", "C", "D")),
    samplingRate = 100
  )

  expect_error(pickChannels(pe, c("A", "Z")), "not found")
})

test_that("pickChannels errors on out-of-range indices", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    samplingRate = 100
  )

  expect_error(pickChannels(pe, c(1, 10)), "not found|out of range")
})

test_that("dropChannels cannot drop all channels", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    samplingRate = 100
  )

  expect_error(dropChannels(pe, 1:4), "Cannot drop all")
})

test_that("setChannelTypes rejects wrong length", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:4)),
    samplingRate = 100
  )

  expect_error(setChannelTypes(pe, c("EEG", "EMG")), "[Ll]ength")
})

test_that("renameChannels rejects mismatched lengths", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:4)),
    samplingRate = 100
  )

  expect_error(
    renameChannels(pe, old_names = c("Ch1", "Ch2"), new_names = "NewCh"),
    "same length"
  )
})

test_that("setElectrodePositions requires x, y, z columns", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:8, nrow = 2, ncol = 4)),
    samplingRate = 100
  )

  expect_error(
    setElectrodePositions(pe, data.frame(a = 1:4, b = 1:4)),
    "x, y, z"
  )
})

# ---------------------------------------------------------------------
# Filter errors
# ---------------------------------------------------------------------

test_that("butterworthFilter requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )

  expect_error(butterworthFilter(pe, high = 30, type = "low"), "sampling rate")
})

test_that("butterworthFilter requires frequency for type", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(butterworthFilter(pe, type = "low"), "high.*required")
  expect_error(butterworthFilter(pe, type = "high"), "low.*required")
  expect_error(butterworthFilter(pe, low = 1, type = "pass"), "high.*required")
})

test_that("butterworthFilter rejects invalid frequencies", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  # Frequency above Nyquist
  expect_error(butterworthFilter(pe, high = 60, type = "low"), "Nyquist")
})

test_that("notchFilter requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )

  expect_error(notchFilter(pe), "sampling rate")
})

test_that("firFilter requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )

  expect_error(firFilter(pe, high = 30, type = "low"), "sampling rate")
})

# ---------------------------------------------------------------------
# Epoching errors
# ---------------------------------------------------------------------

test_that("epochData requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )
  pe <- addEvents(pe, onset = 0.5, type = "stim")

  expect_error(epochData(pe), "sampling rate")
})

test_that("epochData requires events", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 100)),
    samplingRate = 100
  )

  expect_error(epochData(pe), "No events")
})

test_that("epochData requires valid baseline parameter", {
  # Use longer data (10 seconds) to have valid epochs
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(1000), nrow = 1000)),
    samplingRate = 100
  )
  pe <- addEvents(pe, onset = c(1, 2, 3), type = "stim")

  expect_error(
    epochData(pe, baseline = c(-0.2)),  # Should be length 2
    "length 2"
  )
})

test_that("averageEpochs requires 4D data", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(averageEpochs(pe), "4D")
})

test_that("averageEpochs errors on missing group column", {
  epochs <- array(rnorm(100 * 4 * 10), dim = c(100, 4, 10, 1))
  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100,
    metadata = list(epoch_info = S4Vectors::DataFrame(cond = rep("A", 10)))
  )

  expect_error(averageEpochs(pe, by = "missing_col"), "not found")
})

test_that("grandAverage requires at least 2 objects", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(1:4, nrow = 2)),
    samplingRate = 100
  )

  expect_error(grandAverage(pe), "[Aa]t least 2")
})

test_that("grandAverage requires matching dimensions", {
  pe1 <- PhysioExperiment(
    assays = list(avg = matrix(1:4, nrow = 2)),
    samplingRate = 100
  )
  pe2 <- PhysioExperiment(
    assays = list(avg = matrix(1:6, nrow = 3)),
    samplingRate = 100
  )

  expect_error(grandAverage(pe1, pe2), "dimensions")
})

# ---------------------------------------------------------------------
# Statistical testing errors
# ---------------------------------------------------------------------

test_that("tTestEpochs requires 4D data", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(tTestEpochs(pe), "4D")
})

test_that("tTestEpochs paired test requires equal sizes", {
  epochs <- array(rnorm(100 * 4 * 20), dim = c(100, 4, 20, 1))
  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  expect_error(
    tTestEpochs(pe, condition1 = 1:10, condition2 = 11:15, paired = TRUE),
    "equal sample sizes"
  )
})

test_that("anovaEpochs requires 4D data", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(anovaEpochs(pe, groups = c("A", "B")), "4D")
})

test_that("anovaEpochs requires matching group length", {
  epochs <- array(rnorm(100 * 4 * 20), dim = c(100, 4, 20, 1))
  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  expect_error(
    anovaEpochs(pe, groups = c("A", "B")),  # Only 2 groups for 20 epochs
    "match"
  )
})

test_that("anovaEpochs requires at least 2 groups", {
  epochs <- array(rnorm(100 * 4 * 20), dim = c(100, 4, 20, 1))
  pe <- PhysioExperiment(
    assays = list(epoched = epochs),
    samplingRate = 100
  )

  expect_error(
    anovaEpochs(pe, groups = rep("A", 20)),
    "[Aa]t least 2"
  )
})

test_that("clusterPermutationTest requires 4D data", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(clusterPermutationTest(pe), "4D")
})

test_that("effectSize requires 4D data", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(effectSize(pe), "4D")
})

test_that("bootstrapCI requires 4D data", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(bootstrapCI(pe), "4D")
})

test_that("findSignificantWindows requires matching lengths", {
  p <- c(0.01, 0.02, 0.5)
  times <- c(1, 2)

  expect_error(findSignificantWindows(p, times), "match")
})

# ---------------------------------------------------------------------
# Time-frequency analysis errors
# ---------------------------------------------------------------------

test_that("spectrogram requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )

  expect_error(spectrogram(pe), "sampling rate")
})

test_that("waveletTransform requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )

  expect_error(waveletTransform(pe), "sampling rate")
})

# ---------------------------------------------------------------------
# Artifact removal errors
# ---------------------------------------------------------------------

test_that("icaDecompose errors without assays", {
  pe <- PhysioExperiment(
    assays = list(),
    samplingRate = 100
  )

  expect_error(icaDecompose(pe), "assay")
})

# ---------------------------------------------------------------------
# Resampling errors
# ---------------------------------------------------------------------

test_that("resample requires sampling rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = NA
  )

  expect_error(resample(pe, target_rate = 50), "sampling rate")
})

test_that("resample requires positive target rate", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(100), nrow = 10)),
    samplingRate = 100
  )

  expect_error(resample(pe, target_rate = -50), "positive")
})

# ---------------------------------------------------------------------
# I/O errors
# ---------------------------------------------------------------------

test_that("readPhysio errors on missing file", {
  expect_error(readPhysio("nonexistent_file.rds"), "not found|does not exist")
})

test_that("readEDF errors on missing file", {
  expect_error(readEDF("nonexistent_file.edf"), "not found|does not exist")
})

# ---------------------------------------------------------------------
# Visualization errors
# ---------------------------------------------------------------------

test_that("plotMultiChannel errors without assays", {
  pe <- PhysioExperiment(
    assays = list(),
    samplingRate = 100
  )

  expect_error(plotMultiChannel(pe), "assay")
})

test_that("plotMultiChannel errors on out-of-range channels", {
  pe <- PhysioExperiment(
    assays = list(raw = matrix(rnorm(20), nrow = 10, ncol = 2)),
    samplingRate = 100
  )

  expect_error(plotMultiChannel(pe, channels = c(1, 5)), "range")
})
