library(testthat)
library(PhysioExperiment)

# --- Function existence ---

test_that("writePhysioHDF5 function exists", {
  expect_true(is.function(writePhysioHDF5))
})

test_that("readPhysioHDF5 function exists", {
  expect_true(is.function(readPhysioHDF5))
})

test_that("isHDF5Backed function exists", {
  expect_true(is.function(isHDF5Backed))
})

test_that("realizeHDF5 function exists", {
  expect_true(is.function(realizeHDF5))
})

test_that("writeAssayHDF5 function exists", {
  expect_true(is.function(writeAssayHDF5))
})

# --- Input validation ---

test_that("writePhysioHDF5 requires PhysioExperiment object", {
  expect_error(writePhysioHDF5(list(), tempfile(fileext = ".h5")))
})

test_that("readPhysioHDF5 errors on non-existent file", {
  expect_error(readPhysioHDF5("nonexistent.h5"), "File not found")
})

test_that("isHDF5Backed requires PhysioExperiment object", {
  expect_error(isHDF5Backed(list()))
})

test_that("realizeHDF5 requires PhysioExperiment object", {
  expect_error(realizeHDF5(list()))
})

# --- isHDF5Backed on in-memory object ---

test_that("isHDF5Backed returns FALSE for in-memory PhysioExperiment", {
  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)
  expect_false(isHDF5Backed(pe))
})

# --- HDF5 round-trip (skip if rhdf5 not available) ---

test_that("HDF5 write-read round-trip preserves structure", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  set.seed(42)
  pe <- make_pe_2d(n_time = 500, n_channels = 4, sr = 250)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  expect_true(file.exists(tmp))

  pe_read <- readPhysioHDF5(tmp, as_delayed = FALSE)
  expect_s4_class(pe_read, "PhysioExperiment")
  expect_equal(samplingRate(pe_read), 250)

  orig_dim <- dim(SummarizedExperiment::assay(pe))
  read_dim <- dim(SummarizedExperiment::assay(pe_read))
  expect_equal(orig_dim, read_dim)
})

test_that("HDF5 round-trip preserves data exactly (in-memory)", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  set.seed(42)
  pe <- make_pe_2d(n_time = 100, n_channels = 3, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  pe_read <- readPhysioHDF5(tmp, as_delayed = FALSE)

  orig_data <- SummarizedExperiment::assay(pe)
  read_data <- SummarizedExperiment::assay(pe_read)

  expect_equal(orig_data, read_data, tolerance = 1e-10)
})

test_that("HDF5 round-trip preserves sampling rate", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 512)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  pe_read <- readPhysioHDF5(tmp, as_delayed = FALSE)

  expect_equal(samplingRate(pe_read), 512)
})

test_that("writePhysioHDF5 errors when file exists without overwrite", {
  skip_if_not_installed("rhdf5")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  expect_error(
    writePhysioHDF5(pe, tmp, overwrite = FALSE),
    "File already exists"
  )
})

test_that("writePhysioHDF5 with overwrite = TRUE replaces file", {
  skip_if_not_installed("rhdf5")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  expect_silent(writePhysioHDF5(pe, tmp, overwrite = TRUE))
})

test_that("writePhysioHDF5 returns invisible NULL", {
  skip_if_not_installed("rhdf5")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  result <- writePhysioHDF5(pe, tmp)
  expect_null(result)
})

# --- DelayedArray backend ---

test_that("readPhysioHDF5 with as_delayed = TRUE returns DelayedArray-backed data", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  pe_delayed <- readPhysioHDF5(tmp, as_delayed = TRUE)

  expect_s4_class(pe_delayed, "PhysioExperiment")
  expect_true(isHDF5Backed(pe_delayed))
})

test_that("realizeHDF5 converts DelayedArray to in-memory", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  pe_delayed <- readPhysioHDF5(tmp, as_delayed = TRUE)
  expect_true(isHDF5Backed(pe_delayed))

  pe_mem <- realizeHDF5(pe_delayed)
  expect_false(isHDF5Backed(pe_mem))
})

# --- writeAssayHDF5 ---

test_that("writeAssayHDF5 adds an assay to existing HDF5 file", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  pe <- make_pe_2d(n_time = 100, n_channels = 2, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)

  # Add a second assay
  new_data <- SummarizedExperiment::assay(pe) * 2
  SummarizedExperiment::assay(pe, "doubled") <- new_data

  writeAssayHDF5(pe, tmp, "doubled")

  pe_read <- readPhysioHDF5(tmp, as_delayed = FALSE)
  expect_true("doubled" %in% SummarizedExperiment::assayNames(pe_read))
})

# --- Compression levels ---

test_that("writePhysioHDF5 respects compression_level parameter", {
  skip_if_not_installed("rhdf5")

  pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 250)

  tmp_low <- tempfile(fileext = ".h5")
  tmp_high <- tempfile(fileext = ".h5")
  on.exit({
    unlink(tmp_low)
    unlink(tmp_high)
  })

  writePhysioHDF5(pe, tmp_low, compression_level = 0L)
  writePhysioHDF5(pe, tmp_high, compression_level = 9L)

  # Both files should be valid
  pe_low <- readPhysioHDF5(tmp_low, as_delayed = FALSE)
  pe_high <- readPhysioHDF5(tmp_high, as_delayed = FALSE)
  expect_s4_class(pe_low, "PhysioExperiment")
  expect_s4_class(pe_high, "PhysioExperiment")

  # High compression should produce smaller file (usually)
  size_low <- file.info(tmp_low)$size
  size_high <- file.info(tmp_high)$size
  expect_true(size_high <= size_low)
})

# --- colData round-trip ---

test_that("HDF5 round-trip preserves colData", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")

  pe <- make_pe_2d(n_time = 100, n_channels = 3, sr = 100)

  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp))

  writePhysioHDF5(pe, tmp)
  pe_read <- readPhysioHDF5(tmp, as_delayed = FALSE)

  orig_col <- SummarizedExperiment::colData(pe)
  read_col <- SummarizedExperiment::colData(pe_read)

  expect_equal(nrow(orig_col), nrow(read_col))
  expect_true("label" %in% names(read_col))
})
