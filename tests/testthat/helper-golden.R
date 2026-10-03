# Golden-fixture validation utility (CANONICAL COPY).
#
# This file is synced into each package's tests/testthat/helper-golden.R by
# the shared golden-fixture tooling. Edit THIS copy, then run the sync.
#
# Goldens live in <pkg>/tests/testthat/_golden/<key>.rds and MUST be captured
# from an EXTERNAL reference (MNE-Python, Kubios, spm1d/rft1d, igraph, WFDB,
# published values, ...), NEVER from current package code. Each golden has a
# sibling <key>.dcf manifest recording its source, tolerance, and capture time.

.golden_dir <- function() testthat::test_path("_golden")

# Write a golden fixture + provenance manifest (called from a package's golden
# generator, which must obtain `value` from an external reference).
write_golden <- function(value, key, source, tol = 1e-8, dir = .golden_dir()) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(value, file.path(dir, paste0(key, ".rds")))
  write.dcf(
    data.frame(
      Key = key, Source = source, Tolerance = format(tol),
      Captured = format(Sys.time(), tz = "UTC", usetz = TRUE),
      R = as.character(getRversion()),
      stringsAsFactors = FALSE
    ),
    file.path(dir, paste0(key, ".dcf"))
  )
  invisible(value)
}

# Compare `actual` against the stored golden for `key` within `tol`.
# Skips (does not fail) when the golden is absent, so a fresh checkout is green
# until fixtures are captured; fails loudly on any mismatch beyond tolerance.
expect_equal_golden <- function(actual, key, tol = 1e-8, dir = .golden_dir()) {
  path <- file.path(dir, paste0(key, ".rds"))
  if (!file.exists(path)) {
    testthat::skip(sprintf(
      "golden '%s' missing - capture it from an external reference (regen_golden.R)", key))
  }
  expected <- readRDS(path)
  testthat::expect_equal(actual, expected, tolerance = tol,
                         info = sprintf("golden mismatch for '%s' (vs external reference)", key))
}
