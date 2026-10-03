aggregation_fixture <- function() {
  data.frame(subject_id = c(rep("P1", 4), rep("P2", 3)),
    session_id = c("S1", "S1", "S1", "S2", "S1", "S1", "S1"),
    trial_id = c("T1", "T1", "T2", "T1", "T1", "T1", "T2"),
    cycle_id = c("C1", "C2", "C1", "C1", "C1", "C2", "C1"),
    amplitude = c(0, 2, 9, 13, 20, 24, 30))
}

test_that("unbalanced hierarchies match independent hand arithmetic", {
  x <- aggregation_fixture()
  y <- aggregatePhysioFeatures(x, "cohort", "amplitude")
  # P1: S1 = ((0+2)/2 + 9)/2 = 5, S2 = 13; subject mean = 9.
  # P2: S1 = ((20+24)/2 + 30)/2 = 26; cohort = (9+26)/2 = 17.5.
  expect_equal(y$steps$trial$data$amplitude, c(1, 9, 13, 22, 30))
  expect_equal(y$steps$session$data$amplitude, c(5, 13, 26))
  expect_equal(y$steps$subject$data$amplitude, c(9, 26))
  expect_equal(y$data$amplitude, 17.5)
  expect_false(isTRUE(all.equal(y$data$amplitude, mean(x$amplitude))))
  expect_equal(y$counts$n_units, 2)
  expect_equal(y$counts$n_source, 7)
  expect_identical(y$source, x)
  expect_equal(y$source_rows[[1]], 1:7)
})

test_that("direct, chained and serialized operations agree", {
  x <- aggregation_fixture()
  direct <- aggregatePhysioFeatures(x, "cohort", "amplitude")
  a <- aggregatePhysioFeatures(x, "trial", "amplitude")
  b <- aggregatePhysioFeatures(a, "session")
  c <- aggregatePhysioFeatures(b, "subject")
  chained <- aggregatePhysioFeatures(c, "cohort")
  expect_identical(chained, direct)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  saveRDS(b, file)
  expect_identical(aggregatePhysioFeatures(readRDS(file), "cohort"), direct)
  b$data$amplitude[1] <- 99
  expect_error(aggregatePhysioFeatures(b, "subject"), "modified")
})

test_that("strata and reused local identifiers remain distinct", {
  x <- aggregation_fixture(); x$side <- "L"
  right <- x; right$side <- "R"; right$amplitude <- right$amplitude + 100
  both <- rbind(x, right)
  y <- aggregatePhysioFeatures(both, "cohort", "amplitude", "side")
  expect_equal(y$data$amplitude, c(17.5, 117.5))
  expect_identical(y$data$side, c("L", "R"))
  expect_equal(y$counts$n_units, c(2, 2))
  expect_equal(y$source_rows, list(1:7, 8:14))
  expect_error(aggregatePhysioFeatures(both, "cohort", "amplitude"), "Duplicate")
  by_session <- aggregatePhysioFeatures(both, "session", "amplitude", "side")
  expect_error(aggregatePhysioFeatures(by_session, "subject", strata = character()),
               "strata cannot change")
  ord <- c(14, 1, 8, 4, 12, 3, 7, 2, 6, 5, 9, 10, 11, 13)
  z <- aggregatePhysioFeatures(both[ord, ], "cohort", "amplitude", "side")
  expect_equal(z$data$amplitude[match(y$data$side, z$data$side)], y$data$amplitude)
  for (i in seq_len(nrow(z$data)))
    expect_true(all(z$source$side[z$source_rows[[i]]] == z$data$side[i]))
})

test_that("missingness is explicit and audited per feature and per level", {
  x <- aggregation_fixture()
  x$other <- x$amplitude
  x$amplitude[c(1, 2, 5, 6, 7)] <- NA_real_
  x$other[1] <- NA_real_
  expect_error(aggregatePhysioFeatures(x, "session", c("amplitude", "other")),
               "Missing feature")
  y <- aggregatePhysioFeatures(x, "cohort", c("amplitude", "other"), na.rm = TRUE)
  expect_equal(y$data$amplitude, 11) # P1 mean(9,13); P2 all missing.
  expect_equal(y$data$other, 17.625) # P1 mean(mean(2,9),13)=9.25; P2=26.
  expect_equal(y$counts$n_used, c(1, 2))
  expect_equal(y$counts$n_missing, c(1, 0))
  expect_equal(y$steps$trial$counts$n_used[1:5], c(0, 1, 1, 0, 0))
  expect_true(is.na(y$steps$subject$data$amplitude[2]))
  expect_equal(y$counts$n_source, c(7, 7))
  expect_equal(sort(y$source_rows[[1]]), 1:7)
  all_missing <- x; all_missing$amplitude[] <- NA_real_
  empty <- aggregatePhysioFeatures(all_missing, "cohort", "amplitude", na.rm = TRUE)
  expect_identical(empty$data$amplitude, NA_real_)
  expect_equal(empty$counts$n_used, 0)
})

test_that("functions are applied to immediate children, with arguments recorded", {
  x <- aggregation_fixture()
  y <- aggregatePhysioFeatures(x, "cohort", "amplitude", FUN = sum)
  expect_equal(y$data$amplitude, 98)
  z <- aggregatePhysioFeatures(x, "session", "amplitude", FUN = "mean", trim = .1)
  expect_equal(z$data$amplitude, c(5, 13, 26))
  expect_equal(z$steps$trial$arguments, list(trim = .1))
  # At one level, compare with a separate base-R grouping implementation.
  ref <- stats::aggregate(amplitude ~ subject_id + session_id + trial_id, x, mean)
  a <- aggregatePhysioFeatures(x, "trial", "amplitude")$data
  joined <- merge(a, ref, by = c("subject_id", "session_id", "trial_id"))
  expect_equal(joined$amplitude.x, joined$amplitude.y)
  expect_error(aggregatePhysioFeatures(x, "trial", "amplitude", FUN = range),
               "one finite numeric")
  expect_error(aggregatePhysioFeatures(x, "trial", "amplitude", FUN = function(v) Inf),
               "one finite numeric")
  expect_error(aggregatePhysioFeatures(x, "trial", "amplitude", weights = 1:7),
               "weights")
})

test_that("input can start at each declared level, including singletons", {
  x <- data.frame(subject_id = c("A", "B"), feature = c(4, 8))
  expect_equal(aggregatePhysioFeatures(x, "cohort", "feature")$data$feature, 6)
  s <- data.frame(subject_id = "A", session_id = "S", trial_id = "T", feature = 4)
  expect_equal(aggregatePhysioFeatures(s, "cohort", "feature")$data$feature, 4)
  s$subject_id <- factor(s$subject_id)
  expect_identical(aggregatePhysioFeatures(S4Vectors::DataFrame(s), "session", "feature")$
                     data$subject_id, "A")
  expect_error(aggregatePhysioFeatures(s, "trial", "feature"), "strictly above")
  expect_error(aggregatePhysioFeatures(x, "trial", "feature"), "strictly above")
})

test_that("ambiguous identities and unsupported feature shapes fail explicitly", {
  x <- aggregation_fixture()
  expect_error(aggregatePhysioFeatures(x, features = "amplitude"), "target level")
  expect_error(aggregatePhysioFeatures(x, "session"), "feature column")
  expect_error(aggregatePhysioFeatures(x[FALSE, ], "session", "amplitude"), "nonempty")
  bad <- x; bad$trial_id <- NULL
  expect_error(aggregatePhysioFeatures(bad, "session", "amplitude"), "without gaps")
  for (value in c(NA_character_, "", "  ")) {
    bad <- x; bad$subject_id[1] <- value
    expect_error(aggregatePhysioFeatures(bad, "session", "amplitude"), "identity")
  }
  bad <- rbind(x, x[1, ])
  expect_error(aggregatePhysioFeatures(bad, "session", "amplitude"), "Duplicate")
  bad <- x; bad$amplitude[1] <- Inf
  expect_error(aggregatePhysioFeatures(bad, "session", "amplitude", na.rm = TRUE), "infinity")
  bad <- x; bad$amplitude <- as.character(bad$amplitude)
  expect_error(aggregatePhysioFeatures(bad, "session", "amplitude"), "plain numeric")
  bad <- x; bad$amplitude <- I(matrix(1:14, nrow = 7))
  expect_error(aggregatePhysioFeatures(bad, "session", "amplitude"), "plain numeric")
  expect_error(aggregatePhysioFeatures(x, "session", "subject_id"), "overlap")
  expect_error(aggregatePhysioFeatures(x, "session", "amplitude", na.rm = NA), "logical")
  # Delimiters in identifiers must not conflate distinct parent/child tuples.
  special <- data.frame(subject_id = c("a:b", "a"), session_id = c("c", "b:c"),
                        trial_id = "trial", value = c(10, 20))
  expect_equal(nrow(aggregatePhysioFeatures(special, "session", "value")$data), 2)
})
