library(testthat)
library(PhysioExperiment)

make_test_eeg <- function(n_samples = 500, n_channels = 4) {

  # Create test EEG data with different characteristics per channel
  t <- seq(0, n_samples - 1) / 250  # 250 Hz, 2 seconds
  data <- matrix(NA_real_, nrow = n_samples, ncol = n_channels)

  for (ch in seq_len(n_channels)) {
    # Base signal: sum of sinusoids
    signal <- sin(2 * pi * 10 * t) + 0.5 * sin(2 * pi * 20 * t)
    # Add channel-specific noise
    signal <- signal + rnorm(n_samples, sd = 0.1 * ch)
    data[, ch] <- signal
  }

  # rowData must have n_samples rows (matching dim[1] = time points)
  row_data <- S4Vectors::DataFrame(time_idx = seq_len(n_samples))
  # colData must have n_channels rows (matching dim[2] = channels)
  col_data <- S4Vectors::DataFrame(
    label = paste0("Ch", seq_len(n_channels)),
    type = rep("EEG", n_channels)
  )

  # Convert to 3D array (time x channels x samples)
  assays <- S4Vectors::SimpleList(raw = array(data, dim = c(n_samples, n_channels, 1)))
  PhysioExperiment(assays, rowData = row_data, colData = col_data, samplingRate = 250)
}

test_that("icaDecompose runs without error", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # n_components must equal n_channels for matrix inversion
  ica <- icaDecompose(x, n_components = 4)

  expect_type(ica, "list")
  expect_true("mixing" %in% names(ica))
  expect_true("unmixing" %in% names(ica))
  expect_true("components" %in% names(ica))
  expect_equal(dim(ica$components)[2], 4)  # 3D array: time x components x samples
})

test_that("icaRemove removes specified components", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # First decompose (n_components must equal n_channels)
  ica <- icaDecompose(x, n_components = 4)

  # Remove component 1 - use the object from icaDecompose which has ICA info stored
  y <- icaRemove(ica$object, components = 1, output_assay = "raw")

  # Data should be modified
  orig_data <- SummarizedExperiment::assay(x)
  new_data <- SummarizedExperiment::assay(y)

  expect_false(identical(orig_data, new_data))
  expect_equal(dim(orig_data), dim(new_data))
})

test_that("detectBadChannels identifies outlier channels", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Add a bad channel with high amplitude (3D array access)
  data <- SummarizedExperiment::assay(x)
  data[, 1, 1] <- data[, 1, 1] * 100  # Artificially inflate
  SummarizedExperiment::assay(x, "raw") <- data

  bad <- detectBadChannels(x, method = "zscore", threshold = 2)

  expect_type(bad, "integer")
  expect_true(1 %in% bad)  # Channel 1 should be detected
})

test_that("detectBadChannels with correlation method", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Make one channel uncorrelated (random noise) - 3D array access
  data <- SummarizedExperiment::assay(x)
  data[, 2, 1] <- rnorm(dim(data)[1])
  SummarizedExperiment::assay(x, "raw") <- data

  bad <- detectBadChannels(x, method = "correlation", threshold = 0.5)

  expect_type(bad, "integer")
})

test_that("detectBadChannels with flatline method", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Make one channel flat - 3D array access
  data <- SummarizedExperiment::assay(x)
  data[, 3, 1] <- 0
  SummarizedExperiment::assay(x, "raw") <- data

  bad <- detectBadChannels(x, method = "flatline", threshold = 0.001)

  expect_type(bad, "integer")
  expect_true(3 %in% bad)
})

test_that("interpolateBadChannels works with average method", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Mark channel 2 as bad
  y <- interpolateBadChannels(x, bad_channels = 2, method = "average", output_assay = "raw")

  orig <- SummarizedExperiment::assay(x)
  new <- SummarizedExperiment::assay(y)

  # Channel 2 should be modified - 3D array access
  expect_false(identical(orig[, 2, 1], new[, 2, 1]))
  # Other channels should be unchanged
  expect_equal(orig[, 1, 1], new[, 1, 1])
})

test_that("rejectBadEpochs works with epoched data", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Create fake epoched data - 4D (time x channels x epochs x samples)
  data <- SummarizedExperiment::assay(x)
  epoched <- array(NA_real_, dim = c(100, 4, 5, 1))
  for (e in 1:5) {
    start <- (e - 1) * 100 + 1
    epoched[, , e, 1] <- data[start:(start + 99), , 1]
  }

  # Make epoch 3 have high amplitude (artifact)
  epoched[, , 3, 1] <- epoched[, , 3, 1] * 100

  SummarizedExperiment::assay(x, "raw") <- epoched

  rejected <- rejectBadEpochs(x, method = "amplitude", threshold = 50, remove = FALSE)

  expect_type(rejected, "integer")
  expect_true(3 %in% rejected)
})

test_that("baselineCorrect applies correction", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Add epoch timing metadata
  S4Vectors::metadata(x)$epoch_tmin <- 0
  S4Vectors::metadata(x)$epoch_tmax <- 2

  # Add DC offset
  data <- SummarizedExperiment::assay(x)
  data <- data + 100
  SummarizedExperiment::assay(x, "raw") <- data

  y <- baselineCorrect(x, baseline = c(0, 0.1), output_assay = "corrected")

  new_data <- SummarizedExperiment::assay(y, "corrected")

  # Mean of corrected data should be closer to 0
  expect_true(abs(mean(new_data)) < abs(mean(data)))
})

test_that("rejectBadEpochs works with 3D epoched data", {
  # 3D: time x channels x epochs
  data_3d <- array(rnorm(100 * 4 * 5), dim = c(100, 4, 5))
  # Make epoch 3 have artifact
  data_3d[, , 3] <- data_3d[, , 3] * 200

  pe <- PhysioExperiment(
    assays = list(raw = data_3d),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:4), type = rep("EEG", 4)),
    samplingRate = 250
  )

  bad <- rejectBadEpochs(pe, method = "amplitude", threshold = 50, remove = FALSE)
  expect_true(3 %in% bad)

  cleaned <- rejectBadEpochs(pe, method = "amplitude", threshold = 50, remove = TRUE)
  expect_equal(dim(SummarizedExperiment::assay(cleaned))[3], 4)
})

test_that("detectArtifacts detects amplitude artifacts in continuous data", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)
  # Inject artifact at samples 200-250
  data <- SummarizedExperiment::assay(pe)
  data[200:250, 1] <- 500
  SummarizedExperiment::assay(pe, "raw") <- data

  artifacts <- detectArtifacts(pe, method = "amplitude", threshold = 100)

  expect_s3_class(artifacts, "data.frame")
  expect_true(nrow(artifacts) > 0)
  expect_true(all(c("onset", "offset", "channel") %in% names(artifacts)))
  # Artifact should be detected around sample 200-250
  expect_true(any(artifacts$onset <= 200 / 250 & artifacts$offset >= 250 / 250))
})

test_that("detectArtifacts detects gradient artifacts", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)
  data <- SummarizedExperiment::assay(pe)
  # Large jump at sample 500
  data[500, 2] <- data[499, 2] + 1000
  SummarizedExperiment::assay(pe, "raw") <- data

  artifacts <- detectArtifacts(pe, method = "gradient", threshold = 100)

  expect_s3_class(artifacts, "data.frame")
  expect_true(nrow(artifacts) > 0)
})

test_that("classifyICAComponents identifies artifact components by kurtosis", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)
  ica <- icaDecompose(x, n_components = 4)
  pe <- ica$object

  result <- classifyICAComponents(pe, method = "kurtosis")

  expect_type(result, "list")
  expect_true("artifact_indices" %in% names(result))
  expect_true("scores" %in% names(result))
  expect_type(result$artifact_indices, "integer")
  expect_equal(length(result$scores), 4)
})

test_that("classifyICAComponents works with autocorrelation method", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)
  ica <- icaDecompose(x, n_components = 4)
  pe <- ica$object

  result <- classifyICAComponents(pe, method = "autocorrelation")

  expect_type(result, "list")
  expect_true("artifact_indices" %in% names(result))
})

test_that("classifyICAComponents works with frequency method", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)
  ica <- icaDecompose(x, n_components = 4)
  pe <- ica$object

  result <- classifyICAComponents(pe, method = "frequency")

  expect_type(result, "list")
  expect_true("artifact_indices" %in% names(result))
})

test_that("detectArtifacts with auto threshold", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)
  data <- SummarizedExperiment::assay(pe)
  data[200:250, 1] <- 500
  SummarizedExperiment::assay(pe, "raw") <- data

  artifacts <- detectArtifacts(pe, method = "amplitude", threshold = NULL)

  expect_s3_class(artifacts, "data.frame")
})

test_that("cleanData runs default pipeline", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)

  # Inflate channel 1 to make it "bad"
  data <- SummarizedExperiment::assay(x)
  data[, 1, 1] <- data[, 1, 1] * 100
  SummarizedExperiment::assay(x, "raw") <- data

  cleaned <- cleanData(x, steps = c("bad_channels", "ica"))

  expect_s4_class(cleaned, "PhysioExperiment")
  expect_true("cleaned" %in% SummarizedExperiment::assayNames(cleaned))
})

test_that("cleanData with only bad_channels step", {
  x <- make_test_eeg(n_samples = 500, n_channels = 4)
  data <- SummarizedExperiment::assay(x)
  data[, 1, 1] <- data[, 1, 1] * 100
  SummarizedExperiment::assay(x, "raw") <- data

  cleaned <- cleanData(x, steps = "bad_channels")

  expect_s4_class(cleaned, "PhysioExperiment")
})

test_that("detectArtifacts returns empty data.frame for clean data", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)
  artifacts <- detectArtifacts(pe, method = "amplitude", threshold = 1000)
  expect_equal(nrow(artifacts), 0)
})

test_that("classifyICAComponents errors without ICA", {
  pe <- make_pe_2d(n_time = 500, n_channels = 4, sr = 250)
  expect_error(classifyICAComponents(pe), "ICA decomposition not found")
})

test_that("rejectBadEpochs errors when all epochs rejected", {
  data_3d <- array(1000, dim = c(100, 4, 3))
  pe <- PhysioExperiment(
    assays = list(raw = data_3d),
    colData = S4Vectors::DataFrame(label = paste0("Ch", 1:4), type = rep("EEG", 4)),
    samplingRate = 250
  )
  expect_error(rejectBadEpochs(pe, threshold = 1, remove = TRUE),
               "All epochs would be rejected")
})

test_that("detectArtifacts with joint method", {
  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)
  data <- SummarizedExperiment::assay(pe)
  data[200:250, 1] <- 500
  SummarizedExperiment::assay(pe, "raw") <- data

  artifacts <- detectArtifacts(pe, method = "joint", threshold = 100)
  expect_s3_class(artifacts, "data.frame")
  expect_true(nrow(artifacts) > 0)
})
