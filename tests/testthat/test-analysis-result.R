test_that("AnalysisResult holds type and payload", {
  r <- AnalysisResult("hrv_time", result = list(sdnn = 42, rmssd = 30),
                      parameters = list(window = 300))
  expect_s4_class(r, "AnalysisResult")
  expect_equal(resultType(r), "hrv_time")
  expect_equal(resultValue(r)$sdnn, 42)
  expect_output(show(r), "AnalysisResult")
})

test_that("PhysioBiomarker carries value, unit, ci and interprets", {
  b <- PhysioBiomarker("SDNN", 42, unit = "ms", ci = c(38, 46),
                       interpretation = "normal")
  expect_s4_class(b, "PhysioBiomarker")
  expect_s4_class(b, "AnalysisResult")            # inheritance
  expect_equal(biomarkerValue(b), 42)
  expect_equal(resultValue(b), 42)
  expect_equal(resultType(b), "biomarker")
  expect_output(show(b), "SDNN = 42")
  expect_output(show(b), "\\[38, 46\\]")
})

test_that("PhysioBiomarker validity rejects a bad ci length", {
  expect_error(PhysioBiomarker("x", 1, ci = c(1, 2, 3)), "length 0 or 2")
  # no-ci biomarker is valid
  expect_s4_class(PhysioBiomarker("y", 5, unit = "mV"), "PhysioBiomarker")
})
