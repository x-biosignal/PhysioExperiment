library(testthat)

mk_pe <- function(sr = 250) PhysioExperiment(
  S4Vectors::SimpleList(raw = matrix(rnorm(100 * 2), 100, 2)), samplingRate = sr)

mk_pl <- function(id = "sub-01", dx = "stroke") PhysioLongitudinal(
  baseline = mk_pe(), discharge = mk_pe(),
  design = S4Vectors::DataFrame(
    session_id = c("baseline", "discharge"),
    visit_label = c("baseline", "discharge"), days_from_baseline = c(0, 42)),
  subject = S4Vectors::DataFrame(id = id, dx = dx, side = "L"))

test_that("PhysioCohort builds from PhysioLongitudinal + bare experiments", {
  coh <- PhysioCohort("sub-01" = mk_pl(), "sub-02" = mk_pe(),
                      group = c("treatment", "control"))
  expect_s4_class(coh, "PhysioCohort")
  expect_equal(nSubjects(coh), 2L)
  expect_equal(length(coh), 2L)
  expect_equal(subjectIds(coh), c("sub-01", "sub-02"))
  expect_equal(groups(coh), c("treatment", "control"))
  # bare experiment auto-wrapped into a single-session PhysioLongitudinal
  expect_s4_class(subject(coh, "sub-02"), "PhysioLongitudinal")
  expect_equal(length(subject(coh, "sub-02")), 1L)
  # subject metadata carried into colData
  expect_true(all(c("subject_id", "dx", "side", "group") %in%
                    colnames(cohortData(coh))))
  expect_equal(as.character(cohortData(coh)$subject_id), c("sub-01", "sub-02"))
})

test_that("cohortDesign flattens the cohort into subjects x sessions", {
  coh <- PhysioCohort("sub-01" = mk_pl(), "sub-02" = mk_pe(),
                      group = c("treatment", "control"))
  cd <- cohortDesign(coh)
  expect_s3_class(cd, "data.frame")
  expect_equal(nrow(cd), 3L)                        # 2 + 1 sessions
  expect_true(all(c("subject_id", "group", "session_id", "visit_label",
                    "days_from_baseline") %in% names(cd)))
  expect_equal(sort(cd$subject_id), c("sub-01", "sub-01", "sub-02"))
  expect_equal(cd$group[cd$subject_id == "sub-01"], c("treatment", "treatment"))
})

test_that("subsetCohort selects by group and by predicate; [ realigns colData", {
  coh <- PhysioCohort("s1" = mk_pe(), "s2" = mk_pe(), "s3" = mk_pe(),
                      group = c("A", "B", "A"))
  expect_equal(subjectIds(subsetCohort(coh, group = "A")), c("s1", "s3"))
  expect_equal(nSubjects(subsetCohort(coh, subset = group == "B")), 1L)
  sub <- coh[c("s2", "s3")]
  expect_equal(subjectIds(sub), c("s2", "s3"))
  expect_equal(as.character(cohortData(sub)$subject_id), c("s2", "s3"))
  expect_equal(groups(sub), c("B", "A"))
})

test_that("addSubject appends and rejects duplicate ids", {
  coh <- PhysioCohort("s1" = mk_pe(), group = "A")
  coh2 <- addSubject(coh, "s2", mk_pl(id = "s2"), group = "B")
  expect_equal(nSubjects(coh2), 2L)
  expect_equal(groups(coh2), c("A", "B"))
  expect_equal(length(subject(coh2, "s2")), 2L)
  expect_error(addSubject(coh2, "s1", mk_pe()), "already exists")
})

test_that("explicit colData is reordered to match subjects; validity enforced", {
  cd <- S4Vectors::DataFrame(subject_id = c("s2", "s1"), age = c(70, 55))
  coh <- PhysioCohort("s1" = mk_pe(), "s2" = mk_pe(), colData = cd)
  # colData realigned to subject order (s1, s2)
  expect_equal(as.character(cohortData(coh)$subject_id), c("s1", "s2"))
  expect_equal(cohortData(coh)$age, c(55, 70))
  # wrong-length colData rejected
  expect_error(PhysioCohort("s1" = mk_pe(), "s2" = mk_pe(),
                            colData = S4Vectors::DataFrame(subject_id = "s1")),
               "one row per subject")
})

test_that("empty cohort and accessors behave", {
  coh <- PhysioCohort()
  expect_equal(nSubjects(coh), 0L)
  expect_null(groups(coh))
  expect_equal(nrow(cohortDesign(coh)), 0L)
  expect_error(subject(coh, "nope"), "no subject")
})

test_that("provenance aggregates across subjects with a subject column", {
  coh <- PhysioCohort("s1" = mk_pe(), "s2" = mk_pe())
  p <- provenance(coh)
  expect_true("subject" %in% colnames(p))
})
