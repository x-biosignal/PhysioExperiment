library(testthat)
library(PhysioExperiment)

# Keep the shared registry clean regardless of test outcome.
cleanup <- function(...) {
  for (k in list(...)) {
    try(unregisterReader(k), silent = TRUE)
    try(unregisterWriter(k), silent = TRUE)
    try(unregisterOperation(k), silent = TRUE)
  }
}

test_that("register then get dispatches to the correct reader", {
  on.exit(cleanup("brainvision"))
  rd <- function(file, ...) paste0("read:", file)
  registerReader("brainvision", rd, ext = c("vhdr", "eeg"))
  expect_identical(getReader("brainvision"), rd)
  expect_identical(getReader("brainvision")("s.vhdr"), "read:s.vhdr")
})

test_that("keys are case-insensitive", {
  on.exit(cleanup("edf"))
  rd <- function(file, ...) file
  registerReader("EDF", rd)
  expect_identical(getReader("edf"), rd)
  expect_identical(getReader("Edf"), rd)
  expect_true("edf" %in% availableReaders()$format)   # stored normalized
})

test_that("duplicate registration errors unless overwrite = TRUE", {
  on.exit(cleanup("gdf"))
  registerReader("gdf", function(file, ...) 1)
  expect_error(registerReader("gdf", function(file, ...) 2), "already registered")
  new_fn <- function(file, ...) 2
  expect_silent(registerReader("gdf", new_fn, overwrite = TRUE))
  expect_identical(getReader("gdf"), new_fn)
})

test_that("get on an unknown key errors and lists what is registered", {
  on.exit(cleanup("csv"))
  registerReader("csv", function(file, ...) file)
  expect_error(getReader("nope"), "no reader registered")
  expect_error(getReader("nope"), "csv")               # names the available one
})

test_that("unregister removes the entry and reports whether it existed", {
  on.exit(cleanup("hdf5"))
  registerReader("hdf5", function(file, ...) file)
  expect_true(unregisterReader("hdf5"))
  expect_false(unregisterReader("hdf5"))               # already gone
  expect_error(getReader("hdf5"), "no reader registered")
})

test_that("writers and operations are independent stores", {
  on.exit(cleanup("matlab", "bandpass"))
  registerWriter("matlab", function(x, file, ...) file, ext = "mat")
  registerOperation("bandpass", function(x) x, modality = "eeg")
  # A reader key does not leak across kinds.
  expect_error(getReader("matlab"), "no reader registered")
  expect_error(getOperation("matlab"), "no operation registered")
  expect_type(getWriter("matlab"), "closure")
  expect_identical(getOperation("bandpass")(5), 5)
})

test_that("available* returns tidy discovery tables", {
  on.exit(cleanup("aa_reader", "aa_op"))
  registerReader("aa_reader", function(file, ...) file, ext = c("a", "b"))
  registerOperation("aa_op", function(x) x, modality = "ecg")
  rr <- availableReaders()
  expect_true(all(c("format", "ext") %in% names(rr)))
  expect_equal(rr$ext[rr$format == "aa_reader"], "a,b")
  oo <- availableOperations()
  expect_true(all(c("name", "modality") %in% names(oo)))
  expect_equal(oo$modality[oo$name == "aa_op"], "ecg")
})

test_that("registration validates its inputs", {
  expect_error(registerReader("x", "not a function"), "must be a function")
  expect_error(registerReader(c("a", "b"), function(f) f), "single non-empty string")
  expect_error(registerReader("", function(f) f), "single non-empty string")
})

test_that("registry survives being emptied and re-initialized", {
  # .ensure_registry() is idempotent and never clears existing entries.
  registerOperation("keepme", function(x) x, overwrite = TRUE)
  PhysioExperiment:::.ensure_registry()
  expect_type(getOperation("keepme"), "closure")
  unregisterOperation("keepme")
})

test_that("discovery tables are empty (not an error) on a cold registry", {
  reg <- PhysioExperiment:::.registry_env()
  saved <- list(readers = reg$readers, writers = reg$writers,
                operations = reg$operations)
  on.exit({
    reg$readers <- saved$readers
    reg$writers <- saved$writers
    reg$operations <- saved$operations
  })
  reg$readers <- list(); reg$writers <- list(); reg$operations <- list()
  r <- availableReaders()                          # was a crash on names(list())
  expect_s3_class(r, "data.frame")
  expect_identical(nrow(r), 0L)
  expect_identical(names(r), c("format", "ext"))
  expect_type(r$format, "character")
  expect_identical(nrow(availableWriters()), 0L)
  expect_identical(names(availableOperations()), c("name", "modality"))
})

test_that("keys are whitespace-trimmed and all-whitespace is rejected", {
  on.exit(cleanup("edf"))
  fn <- function(file, ...) file
  registerReader("  EDF  ", fn)
  expect_identical(getReader("edf"), fn)           # trimmed and lower-cased
  expect_true("edf" %in% availableReaders()$format)
  expect_error(registerReader("   ", function(f) f), "single non-empty string")
})

test_that("the store is anchored in a session option so it survives a reload", {
  registerOperation("anchored", function(x) x, overwrite = TRUE)
  on.exit(cleanup("anchored"))
  opt <- getOption("PhysioExperiment.registry")
  expect_true(is.environment(opt))                 # runtime-anchored, not NULL
  expect_identical(opt, PhysioExperiment:::.registry_env())
  expect_false(is.null(opt$operations[["anchored"]]))  # store lives in the option
})

test_that("availableOperations filters by modality (case/space/multi-tag safe)", {
  on.exit(cleanup("op_eeg", "op_ecg", "op_multi"))
  registerOperation("op_eeg", function(x) x, modality = "eeg")
  registerOperation("op_ecg", function(x) x, modality = "ecg")
  registerOperation("op_multi", function(x) x, modality = c("eeg", " emg "))
  eeg <- availableOperations(modality = "EEG")     # case-insensitive query
  expect_true(all(c("op_eeg", "op_multi") %in% eeg$name))
  expect_false("op_ecg" %in% eeg$name)
  expect_true("op_multi" %in% availableOperations(modality = "emg")$name)
  expect_equal(nrow(availableOperations(modality = "nope")), 0L)
})

test_that("NA tags inside ext are dropped from the discovery table", {
  on.exit(cleanup("withna"))
  registerReader("withna", function(f) f, ext = c("a", NA))
  expect_equal(availableReaders()$ext[availableReaders()$format == "withna"], "a")
})
