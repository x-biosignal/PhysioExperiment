.inference_repo <- function() {
  candidates <- c(
    Sys.getenv("PHYSIO_REPO_ROOT"),
    file.path(testthat::test_path(), "..", "..", "..", ".."),
    getwd()
  )
  for (candidate in candidates[nzchar(candidates)]) {
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (file.exists(file.path(
      candidate, "physio-ecosystem", "validation", "inference", "surface.csv"
    ))) return(candidate)
  }
  NULL
}

.require_inference_repo <- function() {
  repo <- .inference_repo()
  if (is.null(repo)) {
    skip("central inference fixtures are not shipped with the package")
  }
  repo
}

.inference_lib <- function() {
  repo <- .require_inference_repo()
  root <- file.path(
    repo, "physio-ecosystem", "validation", "inference"
  )
  environment <- new.env(parent = globalenv())
  environment$options <- options
  options(physio.inference.root = root)
  sys.source(file.path(root, "lib.R"), envir = environment)
  list(root = root, env = environment)
}

test_that("WS8 PhysioExperiment contracts pass the offline parity gate", {
  repo <- .require_inference_repo()
  script <- file.path(
    repo, "physio-ecosystem", "validation", "inference", "run_parity.R"
  )
  output <- tempfile("physiocore-parity-")
  log <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      shQuote(script), "--packages", "PhysioExperiment",
      "--output", shQuote(output), "--fail-fast"
    ),
    stdout = TRUE, stderr = TRUE
  )
  expect_null(attr(log, "status"), info = paste(log, collapse = "\n"))
  results <- utils::read.csv(
    file.path(output, "inference_parity.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(results), 6L)
  expect_true(all(results$status == "STRUCTURAL"))
})

test_that("fixture files and generator hashes fail closed", {
  gate <- .inference_lib()
  temporary <- tempfile("inference-fixture-")
  dir.create(file.path(temporary, "fixtures", "carrier"), recursive = TRUE)
  dir.create(file.path(temporary, "generators"), recursive = TRUE)
  source_fixture <- file.path(
    gate$root, "fixtures", "carrier", "carrier-v1"
  )
  expect_true(file.copy(
    source_fixture, file.path(temporary, "fixtures", "carrier"),
    recursive = TRUE
  ))
  expect_true(file.copy(
    file.path(gate$root, "generators", "generate_references.R"),
    file.path(temporary, "generators")
  ))

  fixture <- file.path(temporary, "fixtures", "carrier", "carrier-v1")
  unlink(file.path(fixture, "input.rds"))
  expect_error(
    gate$env$validate_inference_fixture("carrier", "carrier-v1", temporary),
    "missing file"
  )

  unlink(temporary, recursive = TRUE)
  dir.create(file.path(temporary, "fixtures", "carrier"), recursive = TRUE)
  dir.create(file.path(temporary, "generators"), recursive = TRUE)
  file.copy(
    source_fixture, file.path(temporary, "fixtures", "carrier"),
    recursive = TRUE
  )
  file.copy(
    file.path(gate$root, "generators", "generate_references.R"),
    file.path(temporary, "generators")
  )
  expected <- file.path(
    temporary, "fixtures", "carrier", "carrier-v1", "expected.json"
  )
  bytes <- readBin(expected, "raw", n = file.info(expected)$size)
  bytes[20] <- as.raw(bitwXor(as.integer(bytes[20]), 1L))
  writeBin(bytes, expected)
  expect_error(
    gate$env$validate_inference_fixture("carrier", "carrier-v1", temporary),
    "expected hash mismatch"
  )

  expect_true(file.copy(
    file.path(source_fixture, "expected.json"), expected, overwrite = TRUE
  ))
  generator <- file.path(temporary, "generators", "generate_references.R")
  write(" ", generator, append = TRUE)
  expect_error(
    gate$env$validate_inference_fixture("carrier", "carrier-v1", temporary),
    "generator hash mismatch"
  )
})

test_that("surface omissions and stale owner declarations fail audit", {
  gate <- .inference_lib()
  surface <- gate$env$read_inference_surface()
  missing <- surface[-1, , drop = FALSE]
  audit <- gate$env$audit_inference_surface(
    missing, gate$root, require_fixtures = FALSE
  )
  expect_false(audit$valid)
  expect_match(paste(audit$errors, collapse = " "), "absent from surface")

  stale <- surface
  stale$owner_file[1] <- "R/does-not-exist.R"
  audit <- gate$env$audit_inference_surface(
    stale, gate$root, require_fixtures = FALSE
  )
  expect_false(audit$valid)
  expect_match(paste(audit$errors, collapse = " "), "stale owner")
})

test_that("zero references use absolute tolerance and reports are deterministic", {
  gate <- .inference_lib()
  compare <- gate$env$.compare_metric
  expect_true(compare(0, 0, c(abs = 0, rel = 0))$pass)
  expect_false(compare(1e-12, 0, c(abs = 0, rel = 1))$pass)
  expect_true(compare(5e-13, 0, c(abs = 1e-12, rel = 0))$pass)

  repo <- .require_inference_repo()
  script <- file.path(
    repo, "physio-ecosystem", "validation", "inference", "run_parity.R"
  )
  outputs <- c(tempfile("report-a-"), tempfile("report-b-"))
  for (output in outputs) {
    log <- system2(
      file.path(R.home("bin"), "Rscript"),
      c(
        shQuote(script), "--packages", "PhysioExperiment",
        "--output", shQuote(output), "--fail-fast"
      ),
      stdout = TRUE, stderr = TRUE
    )
    expect_null(attr(log, "status"), info = paste(log, collapse = "\n"))
  }
  files <- c(
    "inference_parity.csv", "inference_parity.json", "inference_parity.md"
  )
  hashes <- vapply(outputs, function(output) {
    vapply(file.path(output, files), tools::md5sum, character(1))
  }, character(length(files)))
  expect_identical(hashes[, 1], hashes[, 2])
})
