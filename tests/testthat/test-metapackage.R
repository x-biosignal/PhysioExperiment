test_that("umbrella metadata has one author and four core dependencies", {
  description <- utils::packageDescription("PhysioExperiment")
  expect_identical(description$Package, "PhysioExperiment")
  expect_match(description$Title, "Meta-Package", fixed = TRUE)

  authors <- eval(parse(text = description$`Authors@R`))
  expect_length(authors, 1L)
  expect_identical(authors$given, "Yusuke")
  expect_identical(authors$family, "Matsui")
  expect_setequal(authors$role, c("aut", "cre"))

  expect_match(description$Depends, "PhysioCore", fixed = TRUE)
  expect_match(description$Depends, "PhysioIO", fixed = TRUE)
  expect_match(description$Depends, "PhysioPreprocess", fixed = TRUE)
  expect_match(description$Depends, "PhysioAnalysis", fixed = TRUE)
})

test_that("representative core APIs are re-exported from their owner modules", {
  owners <- c(
    PhysioExperiment = "PhysioCore",
    readEDF = "PhysioIO",
    butterworthFilter = "PhysioPreprocess",
    fftSignals = "PhysioAnalysis"
  )

  umbrella_exports <- getNamespaceExports("PhysioExperiment")
  expect_true(all(names(owners) %in% umbrella_exports))
  for (symbol in names(owners)) {
    expect_identical(
      getExportedValue("PhysioExperiment", symbol),
      getExportedValue(unname(owners[[symbol]]), symbol)
    )
  }
})

test_that("umbrella-owned launchers remain public", {
  expect_true(all(c("launchGUI", "checkGUIDependencies", "startAPIServer") %in%
                    getNamespaceExports("PhysioExperiment")))
  expect_identical(
    environmentName(environment(getExportedValue("PhysioExperiment", "launchGUI"))),
    "PhysioExperiment"
  )
})
