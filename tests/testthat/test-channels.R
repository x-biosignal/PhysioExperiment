library(testthat)
library(PhysioExperiment)

make_eeg_data <- function() {
  # Array is (time x channels x samples) = (100 x 5 x 2)
  assays <- S4Vectors::SimpleList(raw = array(rnorm(1000), dim = c(100, 5, 2)))
  # rowData must have 100 rows (matching dim[1] = time points)
  rowData <- S4Vectors::DataFrame(time_idx = seq_len(100))
  # colData must have 5 rows (matching dim[2] = channels)
  colData <- S4Vectors::DataFrame(label = c("Fp1", "Fp2", "Cz", "O1", "O2"))
  PhysioExperiment(assays, rowData, colData, samplingRate = 500)
}

test_that("channelNames returns correct names", {
  x <- make_eeg_data()
  names <- channelNames(x)

  expect_equal(names, c("Fp1", "Fp2", "Cz", "O1", "O2"))
})

test_that("channelNames<- sets names", {
  x <- make_eeg_data()
  channelNames(x) <- c("F3", "F4", "Fz", "P3", "P4")

  expect_equal(channelNames(x), c("F3", "F4", "Fz", "P3", "P4"))
})

test_that("nChannels returns correct count", {
  x <- make_eeg_data()
  expect_equal(nChannels(x), 5)
})

test_that("setChannelTypes sets types", {
  x <- make_eeg_data()
  x <- setChannelTypes(x, "EEG")

  info <- channelInfo(x)
  expect_true(all(info$type == "EEG"))
})

test_that("setChannelTypes works with named vector", {
  x <- make_eeg_data()
  x <- setChannelTypes(x, c(Fp1 = "EEG", Fp2 = "EEG", Cz = "REF"))

  info <- channelInfo(x)
  expect_equal(info$type[1], "EEG")
  expect_equal(info$type[3], "REF")
})

test_that("getChannelsByType returns correct indices", {
  x <- make_eeg_data()
  x <- setChannelTypes(x, c("EEG", "EEG", "REF", "EEG", "EEG"))

  eeg_idx <- getChannelsByType(x, "EEG")
  expect_equal(eeg_idx, c(1, 2, 4, 5))

  ref_idx <- getChannelsByType(x, "REF")
  expect_equal(ref_idx, 3)
})

test_that("pickChannels selects correct channels", {
  x <- make_eeg_data()
  y <- pickChannels(x, c("Fp1", "Cz"))

  expect_equal(nChannels(y), 2)
  expect_equal(channelNames(y), c("Fp1", "Cz"))
})

test_that("pickChannels works with indices", {
  x <- make_eeg_data()
  y <- pickChannels(x, c(1, 3, 5))

  expect_equal(nChannels(y), 3)
  expect_equal(channelNames(y), c("Fp1", "Cz", "O2"))
})

test_that("dropChannels removes channels", {
  x <- make_eeg_data()
  y <- dropChannels(x, c("Fp1", "Fp2"))

  expect_equal(nChannels(y), 3)
  expect_false("Fp1" %in% channelNames(y))
})

test_that("renameChannels renames correctly", {
  x <- make_eeg_data()
  x <- renameChannels(x, c("Fp1", "Fp2"), c("FP1", "FP2"))

  expect_equal(channelNames(x)[1:2], c("FP1", "FP2"))
})

test_that("setReference and getReference work", {
  x <- make_eeg_data()
  x <- setReference(x, "Cz")

  expect_equal(getReference(x), "Cz")
})

test_that("setElectrodePositions and getElectrodePositions work", {
  x <- make_eeg_data()
  positions <- data.frame(
    x = c(-0.3, 0.3, 0, -0.3, 0.3),
    y = c(0.9, 0.9, 0.7, -0.9, -0.9),
    z = c(0, 0, 0.7, 0, 0)
  )
  x <- setElectrodePositions(x, positions)

  pos <- getElectrodePositions(x)
  expect_equal(nrow(pos), 5)
  expect_true(all(c("x", "y", "z") %in% names(pos)))
})

test_that("applyMontage sets positions for known electrodes", {
  x <- make_eeg_data()
  x <- applyMontage(x, "10-20")

  pos <- getElectrodePositions(x)
  expect_false(is.na(pos$x[pos$channel == "Cz"]))
})

test_that("setChannelUnits sets units", {
  x <- make_eeg_data()
  x <- setChannelUnits(x, "uV")

  info <- channelInfo(x)
  expect_true(all(info$unit == "uV"))
})
