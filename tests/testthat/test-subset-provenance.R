test_that("subsetting logs exact selections without changing linked data", {
  raw <- matrix(seq_len(24), 8, 3,
                dimnames = list(paste0("t", 1:8), c("C1", "C2", "C3")))
  x <- PhysioExperiment(list(raw = raw, derived = raw / 2),
    rowData = S4Vectors::DataFrame(time = (0:7) / 100),
    colData = S4Vectors::DataFrame(label = colnames(raw)), samplingRate = 100,
    metadata = list(study = "fixture"))
  x <- logStep(x, "import")
  selections <- list(c(5L, 2L, 2L), c(-1L, -8L), rep(c(TRUE, FALSE), 4),
                     character(0), c("t5", "t2"), c(1.999999999, 2.999999999))
  for (i in selections) {
    y <- x[i, c("C3", "C1")]
    expect_equal(SummarizedExperiment::assay(y, "raw"), raw[i, c("C3", "C1"), drop = FALSE])
    expect_equal(SummarizedExperiment::assay(y, "derived"), raw[i, c("C3", "C1"), drop = FALSE] / 2)
    expected_rows <- SummarizedExperiment::rowData(x)[i, , drop = FALSE]
    expect_identical(as.list(SummarizedExperiment::rowData(y)), as.list(expected_rows))
    expect_identical(as.character(rownames(SummarizedExperiment::rowData(y))),
                     as.character(rownames(expected_rows)))
    expect_identical(SummarizedExperiment::colData(y)$label, c("C3", "C1"))
    expect_identical(S4Vectors::metadata(y)$study, "fixture")
    log <- S4Vectors::metadata(y)$provenance
    expect_length(log, 2L)
    expect_identical(log[[2]]$params$rows, i)
    expect_identical(log[[2]]$params$columns, c("C3", "C1"))
    expect_identical(log[[2]]$activity, "subset")
    expect_identical(log[[2]]$params$output_dim, dim(y))
    expect_identical(samplingRate(y), samplingRate(x))
  }
  y <- x[, ]
  log <- S4Vectors::metadata(y)$provenance[[2]]
  expect_true(log$params$rows_missing)
  expect_true(log$params$columns_missing)
  expect_null(log$params$rows)
  expect_identical(SummarizedExperiment::assay(y), raw)
  expect_equal(nrow(provenance(x)), 1L)
})

test_that("subset JSON preserves characters and full numeric precision", {
  labels <- c("a\\b", "c\nd", "e\tf", 'g"h')
  x <- PhysioExperiment(list(raw = matrix(1:16, 4, 4,
    dimnames = list(NULL, labels))), samplingRate = 100)
  y <- x[c(1.999999999, 2.999999999), labels]
  j <- tail(provenance(y)$params_json, 1)
  expect_match(j, 'a\\\\b', fixed = TRUE)
  expect_match(j, 'c\\u000ad', fixed = TRUE)
  expect_match(j, 'e\\u0009f', fixed = TRUE)
  expect_match(j, 'g\\"h', fixed = TRUE)
  expect_match(j, format(1.999999999, digits = 17, scientific = FALSE), fixed = TRUE)
})
