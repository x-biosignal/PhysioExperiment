library(testthat)
library(PhysioExperiment)

make_bm <- function() {
  physioBiomarker(
    value = 2.35, name = "DAR", unit = "ratio",
    ci = c(1.90, 2.80), reference_range = c(0.5, 1.2),
    reliability = list(icc = 0.82, sem = 0.15, mdc = 0.42),
    provenance = list(assay = "psd", band = "delta/alpha",
                      method = "welch", software_version = "1.0.0"),
    interpretation = "elevated")
}

test_that("physioBiomarker builds a PhysioBiomarker carrying all fields", {
  bm <- make_bm()
  expect_true(is.PhysioBiomarker(bm))
  expect_s4_class(bm, "PhysioBiomarker")
  expect_equal(biomarkerValue(bm), 2.35)
  expect_equal(bm@reliability$icc, 0.82)
  expect_equal(bm@reference_range, c(0.5, 1.2))
  expect_equal(bm@provenance_info$method, "welch")
  expect_false(is.PhysioBiomarker(42))
})

test_that("as.data.frame round-trips with reliability + provenance intact", {
  bm <- make_bm()
  df <- as.data.frame(bm)
  expect_equal(nrow(df), 1L)
  # reliability fields intact
  expect_equal(df$icc, 0.82)
  expect_equal(df$sem, 0.15)
  expect_equal(df$mdc, 0.42)
  # provenance fields intact
  expect_equal(df$prov_assay, "psd")
  expect_equal(df$prov_band, "delta/alpha")
  expect_equal(df$prov_method, "welch")
  expect_equal(df$prov_software_version, "1.0.0")
  # value / ci / reference range intact
  expect_equal(df$value, 2.35)
  expect_equal(c(df$ci_lower, df$ci_upper), c(1.90, 2.80))
  expect_equal(c(df$ref_lower, df$ref_upper), c(0.5, 1.2))

  # reconstruct from the flattened row -> same fields
  bm2 <- physioBiomarker(
    value = df$value, name = df$name, unit = df$unit,
    ci = c(df$ci_lower, df$ci_upper),
    reference_range = c(df$ref_lower, df$ref_upper),
    reliability = list(icc = df$icc, sem = df$sem, mdc = df$mdc),
    provenance = list(assay = df$prov_assay, band = df$prov_band,
                      method = df$prov_method,
                      software_version = df$prov_software_version),
    interpretation = df$interpretation)
  expect_equal(as.data.frame(bm2), df)
})

test_that("print shows value, CI, reliability and a normative percentile", {
  bm <- make_bm()
  out <- paste(capture.output(print(bm)), collapse = "\n")
  expect_match(out, "DAR = 2.35")
  expect_match(out, "1.9")               # CI lower
  expect_match(out, "2.8")               # CI upper
  expect_match(out, "percentile")
  expect_match(out, "reliability")
  expect_match(out, "ICC=0.82")
})

test_that("format returns a compact one-line string", {
  bm <- make_bm()
  expect_equal(format(bm), "DAR = 2.35 ratio [1.9, 2.8]")
})

test_that("normativeLookup returns z-score/percentile for a known marker", {
  bm <- make_bm()
  res <- normativeLookup(bm, age = 40)
  expect_true(res$matched)
  expect_equal(res$mean, 0.80)
  expect_equal(res$z_score, (2.35 - 0.80) / 0.30)
  expect_true(res$percentile > 99)       # far above the healthy mean
  expect_equal(res$source, "Finnigan 2016 (scaffold seed)")

  # age selects the correct stratum
  res_old <- normativeLookup(bm, age = 70)
  expect_true(res_old$matched)
  expect_equal(res_old$mean, 1.10)
})

test_that("normativeLookup returns NA gracefully for unknown marker/age", {
  # unknown marker name -> NA, no error
  res <- expect_error(normativeLookup("UNKNOWN_MARKER"), NA)
  res <- normativeLookup("UNKNOWN_MARKER")
  expect_false(res$matched)
  expect_true(is.na(res$z_score))
  expect_true(is.na(res$percentile))

  # ambiguous (age omitted, marker has multiple age strata) -> NA gracefully
  amb <- normativeLookup(make_bm())
  expect_false(amb$matched)
  expect_true(is.na(amb$z_score))

  # age outside any band -> NA gracefully
  oob <- normativeLookup(make_bm(), age = 5)
  expect_false(oob$matched)
})

test_that("PhysioBiomarker validity rejects a malformed reference_range", {
  expect_error(
    methods::new("PhysioBiomarker", name = "x", value = 1,
                 reference_range = c(1, 2, 3)),
    "reference_range")
})

test_that("the legacy PhysioBiomarker constructor still works", {
  bm <- PhysioBiomarker("SDNN", 42, unit = "ms", ci = c(38, 46))
  expect_true(is.PhysioBiomarker(bm))
  expect_equal(biomarkerValue(bm), 42)
  expect_length(bm@reliability, 0L)      # new slots default empty
  expect_length(bm@reference_range, 0L)
})
