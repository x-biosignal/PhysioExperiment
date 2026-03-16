library(testthat)
library(PhysioExperiment)

# --- Helper: create a minimal EDF file for read-back testing ---
write_minimal_edf <- function(pe, path) {

  writeEDF(pe, path)
}

# --- Function existence ---

test_that("readEDF function exists", {
  expect_true(is.function(readEDF))
})

test_that("writeEDF function exists", {
  expect_true(is.function(writeEDF))
})

test_that("readBDF function exists", {
  expect_true(is.function(readBDF))
})

test_that("writeBDF function exists", {
  expect_true(is.function(writeBDF))
})

# --- Input validation ---

test_that("readEDF errors on non-existent file", {
  expect_error(readEDF("nonexistent_file.edf"), "File not found")
})

test_that("readBDF errors on non-existent file", {
  expect_error(readBDF("nonexistent_file.bdf"), "File not found")
})

test_that("writeEDF requires PhysioExperiment object", {
  expect_error(writeEDF(list(), tempfile(fileext = ".edf")))
})

test_that("writeBDF requires PhysioExperiment object", {
  expect_error(writeBDF(list(), tempfile(fileext = ".bdf")))
})

# --- EDF round-trip write -> read ---

test_that("EDF write-read round-trip preserves structure", {
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  writeEDF(pe, tmp)
  expect_true(file.exists(tmp))

  pe_read <- readEDF(tmp)
  expect_s4_class(pe_read, "PhysioExperiment")
  expect_equal(samplingRate(pe_read), 250)

  # Dimensions should match
  orig_dim <- dim(SummarizedExperiment::assay(pe))
  read_dim <- dim(SummarizedExperiment::assay(pe_read))
  expect_equal(read_dim[2], orig_dim[2])  # same number of channels
})

test_that("EDF round-trip preserves data approximately", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 250, n_channels = 2, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  writeEDF(pe, tmp)
  pe_read <- readEDF(tmp)

  orig_data <- SummarizedExperiment::assay(pe)
  read_data <- SummarizedExperiment::assay(pe_read)

  # EDF is 16-bit, so there will be quantization error, but correlation should be high
  for (ch in seq_len(ncol(orig_data))) {
    r <- cor(orig_data[seq_len(nrow(read_data)), ch], read_data[, ch])
    expect_gt(r, 0.99)
  }
})

test_that("EDF round-trip preserves channel labels", {
  pe <- make_pe_2d(n_time = 250, n_channels = 3, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  writeEDF(pe, tmp)
  pe_read <- readEDF(tmp)

  col_data <- SummarizedExperiment::colData(pe_read)
  expect_true("label" %in% names(col_data))
  expect_equal(nrow(col_data), 3)
})

test_that("writeEDF accepts patient_id and recording_id", {
  pe <- make_pe_2d(n_time = 250, n_channels = 2, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  expect_silent(writeEDF(pe, tmp, patient_id = "Subject01", recording_id = "Session1"))
  expect_true(file.exists(tmp))
})

test_that("writeEDF returns invisible NULL", {
  pe <- make_pe_2d(n_time = 250, n_channels = 2, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  result <- writeEDF(pe, tmp)
  expect_null(result)
})

# --- BDF round-trip write -> read ---

test_that("BDF write-read round-trip preserves structure", {
  pe <- make_pe_2d(n_time = 500, n_channels = 3, sr = 256)

  tmp <- tempfile(fileext = ".bdf")
  on.exit(unlink(tmp))

  writeBDF(pe, tmp)
  expect_true(file.exists(tmp))

  pe_read <- readBDF(tmp)
  expect_s4_class(pe_read, "PhysioExperiment")
  expect_equal(samplingRate(pe_read), 256)

  read_dim <- dim(SummarizedExperiment::assay(pe_read))
  expect_equal(read_dim[2], 3)
})

test_that("BDF round-trip preserves data approximately (24-bit precision)", {
  set.seed(42)
  pe <- make_pe_2d(n_time = 256, n_channels = 2, sr = 256)

  tmp <- tempfile(fileext = ".bdf")
  on.exit(unlink(tmp))

  writeBDF(pe, tmp)
  pe_read <- readBDF(tmp)

  orig_data <- SummarizedExperiment::assay(pe)
  read_data <- SummarizedExperiment::assay(pe_read)

  # BDF has 24-bit resolution, so correlation should be very high
  for (ch in seq_len(ncol(orig_data))) {
    r <- cor(orig_data[seq_len(nrow(read_data)), ch], read_data[, ch])
    expect_gt(r, 0.999)
  }
})

test_that("readBDF rejects non-BDF file", {
  # Write an EDF file and try to read it as BDF
  pe <- make_pe_2d(n_time = 250, n_channels = 2, sr = 250)

  tmp <- tempfile(fileext = ".bdf")
  on.exit(unlink(tmp))

  # Write EDF header (starts with "0", not 0xFF)
  writeEDF(pe, tmp)

  expect_error(readBDF(tmp), "Not a valid BDF file")
})

# --- EDF reading with channel selection ---

test_that("readEDF with specific channels works", {
  pe <- make_pe_2d(n_time = 250, n_channels = 4, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  writeEDF(pe, tmp)

  pe_sub <- readEDF(tmp, channels = c("Ch1", "Ch3"))
  expect_equal(ncol(SummarizedExperiment::assay(pe_sub)), 2)
})

test_that("readEDF errors on missing channels", {
  pe <- make_pe_2d(n_time = 250, n_channels = 2, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  writeEDF(pe, tmp)

  expect_error(readEDF(tmp, channels = c("Nonexistent")), "Channels not found")
})

# --- Metadata in EDF ---

test_that("readEDF populates metadata", {
  pe <- make_pe_2d(n_time = 250, n_channels = 2, sr = 250)

  tmp <- tempfile(fileext = ".edf")
  on.exit(unlink(tmp))

  writeEDF(pe, tmp, patient_id = "TestPat", recording_id = "TestRec")
  pe_read <- readEDF(tmp)

  meta <- S4Vectors::metadata(pe_read)
  expect_true("patient_id" %in% names(meta))
  expect_true("file_type" %in% names(meta))
  expect_true("original_file" %in% names(meta))
})
