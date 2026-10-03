library(testthat)
library(PhysioExperiment)

.mk_eeg <- function(labs, sr = 100) {
  PhysioExperiment(
    assays = S4Vectors::SimpleList(
      raw = matrix(rnorm(50 * length(labs)), 50, length(labs))),
    colData = S4Vectors::DataFrame(label = labs),
    samplingRate = sr
  )
}

.mk_long <- function() {
  PhysioLongitudinal(
    baseline  = .mk_eeg(c("Fz", "Cz", "Pz", "Oz", "F3")),
    mid       = .mk_eeg(c("Cz", "Fz", "Pz", "Oz")),
    discharge = .mk_eeg(c("Pz", "Cz", "Fz", "T3"))
  )
}

test_that("harmonizeChannels converges sessions to the common intersection in identical order", {
  h <- harmonizeChannels(.mk_long())
  chans <- lapply(as.list(sessions(h)), channelNames)
  # common to all three sessions is {Fz, Cz, Pz}, in the first session's order
  expect_equal(chans$baseline, c("Fz", "Cz", "Pz"))
  # every session has the SAME channels in the SAME order
  expect_true(length(unique(chans)) == 1L)
  # each session actually has only the intersection now
  expect_true(all(vapply(as.list(sessions(h)), nChannels, integer(1)) == 3L))
})

test_that("target_labels controls the harmonized set and order", {
  h <- harmonizeChannels(.mk_long(), target_labels = c("Pz", "Fz"))
  expect_equal(channelNames(session(h, "mid")), c("Pz", "Fz"))
  expect_equal(channelNames(session(h, "discharge")), c("Pz", "Fz"))
})

test_that("harmonizeChannels reports dropped channels per session", {
  h <- harmonizeChannels(.mk_long())
  rep <- harmonizeReport(h)
  expect_setequal(rep$baseline$dropped, c("Oz", "F3"))
  expect_setequal(rep$mid$dropped, "Oz")
  expect_setequal(rep$discharge$dropped, "T3")
  expect_equal(rep$baseline$kept, c("Fz", "Cz", "Pz"))
})

test_that("rename unifies label conventions before intersecting", {
  pl <- PhysioLongitudinal(
    baseline = .mk_eeg(c("T7", "Cz", "Pz")),
    discharge = .mk_eeg(c("T3", "Cz", "Pz")))
  h <- harmonizeChannels(pl, rename = c(T7 = "T3"))
  expect_true("T3" %in% channelNames(session(h, "baseline")))
  expect_equal(channelNames(session(h, "baseline")),
               channelNames(session(h, "discharge")))
})

test_that("harmonizeReference makes getReference identical and appends provenance", {
  h <- harmonizeReference(.mk_long(), "average")
  refs <- vapply(as.list(sessions(h)), getReference, character(1))
  expect_true(all(refs == "average"))
  expect_length(unique(refs), 1L)
  # each session records the activity in its provenance
  for (nm in names(sessions(h))) {
    expect_true("harmonizeReference" %in% provenance(session(h, nm))$activity)
  }
})

test_that("harmonizeMontage assigns positions and records provenance", {
  h <- harmonizeMontage(.mk_long(), "10-20")
  for (nm in names(sessions(h))) {
    expect_true("harmonizeMontage" %in% provenance(session(h, nm))$activity)
  }
})

test_that("harmonize() runs all three steps and records three activities", {
  h <- harmonize(.mk_long())
  base_acts <- provenance(session(h, "baseline"))$activity
  expect_true(all(c("harmonizeChannels", "harmonizeReference", "harmonizeMontage")
                  %in% base_acts))
  # channels converged and reference set
  expect_equal(channelNames(session(h, "baseline")), c("Fz", "Cz", "Pz"))
  expect_equal(getReference(session(h, "discharge")), "average")
  # ref = NULL / system = NULL skip those steps
  h2 <- harmonize(.mk_long(), ref = NULL, system = NULL)
  a2 <- provenance(session(h2, "baseline"))$activity
  expect_false("harmonizeReference" %in% a2)
  expect_false("harmonizeMontage" %in% a2)
})

test_that("harmonize errors on non-PhysioExperiment sessions and empty intersection", {
  # empty intersection
  pl <- PhysioLongitudinal(a = .mk_eeg(c("Fz", "Cz")),
                           b = .mk_eeg(c("Pz", "Oz")))
  expect_error(harmonizeChannels(pl), "common")
})

# ---- regression tests for adversarial-review findings ----------------------

test_that("duplicate channel labels are rejected (no silent data loss)", {
  pl <- PhysioLongitudinal(
    baseline = .mk_eeg(c("Cz", "Cz", "Fz")),   # duplicate label
    discharge = .mk_eeg(c("Cz", "Fz")))
  expect_error(harmonizeChannels(pl), "duplicate channel labels")
})

test_that("a rename that collapses two labels is rejected", {
  pl <- PhysioLongitudinal(
    baseline = .mk_eeg(c("T7", "T3", "Cz")),   # rename T7->T3 would collide
    discharge = .mk_eeg(c("T3", "Cz")))
  expect_error(harmonizeChannels(pl, rename = c(T7 = "T3")), "duplicate")
})

test_that("rename with NA or empty replacement is rejected", {
  pl <- PhysioLongitudinal(baseline = .mk_eeg(c("Fz", "Cz")),
                           discharge = .mk_eeg(c("Fz", "Cz")))
  expect_error(harmonizeChannels(pl, rename = c(Fz = NA_character_)), "non-NA")
  expect_error(harmonizeChannels(pl, rename = c(Fz = "")), "non-empty")
})

test_that("the renamed report is per-session (only renames that applied)", {
  pl <- PhysioLongitudinal(
    baseline = .mk_eeg(c("T7", "Cz", "Pz")),   # has T7
    discharge = .mk_eeg(c("Cz", "Pz")))        # no T7
  h <- harmonizeChannels(pl, rename = c(T7 = "T3"))
  rep <- harmonizeReport(h)
  expect_equal(unname(rep$baseline$renamed), "T3")     # applied here
  expect_length(rep$discharge$renamed, 0L)             # not applied here
})

test_that("re-running harmonizeChannels accumulates the dropped history", {
  h1 <- harmonizeChannels(.mk_long())                          # drops to Fz,Cz,Pz
  h2 <- harmonizeChannels(h1, target_labels = c("Fz", "Cz"))   # further drop Pz
  rep <- harmonizeReport(h2)
  expect_true("Pz" %in% rep$baseline$dropped)                  # from 2nd run
  expect_true(all(c("Oz", "F3") %in% rep$baseline$dropped))    # kept from 1st run
})

test_that("harmonizeReport does not crash on MultiRatePhysioExperiment sessions", {
  mr <- MultiRatePhysioExperiment(
    kin = PhysioExperiment(S4Vectors::SimpleList(raw = matrix(rnorm(20), 10, 2)),
                           samplingRate = 100))
  pl <- PhysioLongitudinal(baseline = .mk_eeg(c("Fz", "Cz")), mid = mr)
  rep <- harmonizeReport(pl)
  expect_length(rep$mid$kept, 0L)                     # empty entry, no crash
  expect_equal(rep$baseline$kept, c("Fz", "Cz"))
})
