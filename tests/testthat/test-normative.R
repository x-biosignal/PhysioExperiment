test_that("NormativeReference computes z-scores per stratum", {
  ref <- NormativeReference(
    "gait_speed",
    strata = data.frame(
      sex = c("M", "F"), mean = c(1.34, 1.24), sd = c(0.20, 0.19),
      stringsAsFactors = FALSE
    ),
    source = "Bohannon 2011", unit = "m/s"
  )
  expect_s4_class(ref, "NormativeReference")
  expect_equal(zScore(ref, 1.34, by = list(sex = "M")), 0)
  expect_equal(zScore(ref, 1.14, by = list(sex = "M")), (1.14 - 1.34) / 0.20)
  expect_equal(zScore(ref, 1.24, by = list(sex = "F")), 0)
})

test_that("percentPredicted returns 100 at the mean", {
  ref <- NormativeReference(
    "sixmwt",
    strata = data.frame(mean = 550, sd = 90),
    unit = "m"
  )
  expect_equal(percentPredicted(ref, 550), 100)
  expect_equal(percentPredicted(ref, 275), 50)
  # single stratum: by = NULL is allowed
  expect_equal(zScore(ref, 640), (640 - 550) / 90)
})

test_that("stratum matching validates keys and uniqueness", {
  ref <- NormativeReference(
    "grip",
    strata = data.frame(
      sex = c("M", "F"), mean = c(45, 28), sd = c(8, 6),
      stringsAsFactors = FALSE
    )
  )
  expect_error(zScore(ref, 40), "more than one stratum")
  expect_error(zScore(ref, 40, by = list(age = "60")), "unknown stratum key")
  expect_error(zScore(ref, 40, by = list(sex = "X")), "no stratum matches")
})

test_that("validity requires mean and sd columns", {
  expect_error(
    NormativeReference("bad", strata = data.frame(mu = 1, sigma = 1)),
    "must contain 'mean' and 'sd'"
  )
  expect_output(
    show(NormativeReference("m", data.frame(mean = 1, sd = 1))),
    "NormativeReference"
  )
})
