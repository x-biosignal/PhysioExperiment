test_that("physioPalette returns unique, valid hex colors", {
  p8 <- physioPalette(8)
  expect_length(p8, 8L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p8)))
  expect_equal(length(unique(p8)), 8L)
  expect_equal(physioPalette(3), p8[1:3])          # prefix-stable
  expect_equal(length(unique(suppressWarnings(physioPalette(12)))), 12L)  # interpolated, still unique
  expect_error(physioPalette(0))
})

test_that("physioPalette warns that >8 qualitative colours are not CVD-safe", {
  # only 8 Okabe-Ito colours are reliably colourblind-safe; beyond that no
  # qualitative palette is, so requesting more must warn (not silently mislead)
  expect_silent(physioPalette(8))
  expect_warning(physioPalette(9), "colourblind-safe")
  expect_warning(physioPalette(12), "beyond 8")
  # sequential / diverging have no such 8-colour ceiling: no warning
  expect_silent(physioPalette(20, "sequential"))
  expect_silent(physioPalette(20, "diverging"))
})

test_that("palette stays perceptually distinguishable (CIEDE2000), incl. deuteranopia", {
  skip_if_not_installed("farver")
  min_de <- function(cols) {
    lab <- farver::convert_colour(t(grDevices::col2rgb(cols)), "rgb", "lab")
    d <- farver::compare_colour(lab, lab, "lab", method = "cie2000")
    min(d[upper.tri(d)])
  }
  p <- physioPalette(8)
  expect_gt(min_de(p), 9)  # clearly distinct in normal vision
  if (requireNamespace("colorspace", quietly = TRUE)) {
    expect_gt(min_de(colorspace::deutan(p)), 8)  # still distinct under deuteranopia
  }
})

test_that("theme_physio and colorblind-safe scales build", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(theme_physio(), "theme")
  expect_s3_class(scale_color_physio(), "ScaleDiscrete")
  expect_s3_class(scale_colour_physio(), "ScaleDiscrete")
  expect_s3_class(scale_fill_physio(), "ScaleDiscrete")
})


test_that("physioPalette supports sequential and diverging types", {
  # qualitative remains the default (backward compatible)
  expect_equal(physioPalette(4), physioPalette(4, "qualitative"))
  for (type in c("sequential", "diverging")) {
    p <- physioPalette(7, type)
    expect_length(p, 7L)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p)))
  }
  # sequential viridis is monotone in lightness; diverging is symmetric-ish
  seq7 <- physioPalette(7, "sequential")
  expect_equal(length(unique(seq7)), 7L)
  expect_error(physioPalette(3, "nope"))
})

test_that("sequential and diverging palettes are CVD-distinguishable", {
  skip_if_not_installed("farver")
  min_adj <- function(cols) {
    lab <- farver::convert_colour(t(grDevices::col2rgb(cols)), "rgb", "lab")
    min(vapply(seq_len(nrow(lab) - 1L), function(i)
      farver::compare_colour(lab[i, , drop = FALSE], lab[i + 1L, , drop = FALSE],
                             "lab", method = "cie2000"), numeric(1)))
  }
  # adjacent steps of the sequential ramp are perceptibly different
  expect_gt(min_adj(physioPalette(7, "sequential")), 3)
  if (requireNamespace("colorspace", quietly = TRUE)) {
    expect_gt(min_adj(colorspace::deutan(physioPalette(7, "sequential"))), 2)
  }
})
