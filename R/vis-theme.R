#' Colorblind-safe palettes (qualitative, sequential, diverging)
#'
#' Returns \code{n} colors from a colour-vision-deficiency-safe palette. The
#' \code{"qualitative"} palette is the Okabe & Ito set (distinguishable under
#' deuteranopia / protanopia); \code{"sequential"} is viridis; and
#' \code{"diverging"} is a CVD-safe blue-red diverging ramp. Shared across all
#' ecosystem visualizations so figures are consistent and accessible.
#'
#' @param n Number of colors to return.
#' @param type Palette family: \code{"qualitative"} (default), \code{"sequential"}
#'   or \code{"diverging"}.
#' @return Character vector of \code{n} hex colors. The qualitative palette has
#'   only 8 colour-vision-safe entries; requesting \code{n > 8} qualitative
#'   colours emits a warning and returns a best-effort interpolation that is no
#'   longer reliably distinguishable under colour-vision deficiency (no
#'   qualitative palette is). Prefer faceting or a sequential encoding for more
#'   than 8 categories.
#' @references Okabe, M. & Ito, K. (2008). Color Universal Design. Garnier et
#'   al. (viridis). Zeileis et al. (2020). colorspace, JSS 96(1).
#' @examples
#' physioPalette(4)
#' physioPalette(7, "sequential")
#' physioPalette(5, "diverging")
#' @export
physioPalette <- function(n = 8L, type = c("qualitative", "sequential",
                                           "diverging")) {
  type <- match.arg(type)
  n <- as.integer(n)
  if (is.na(n) || n < 1L) stop("'n' must be a positive integer")
  if (type == "qualitative") {
    okabe_ito <- c(
      "#000000", "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
    )
    if (n <= length(okabe_ito)) return(okabe_ito[seq_len(n)])
    # Beyond the 8 Okabe-Ito colours no qualitative palette stays reliably
    # distinguishable under colour-vision deficiency (interpolation and every
    # alternative collapse to CIEDE2000 ~2 under deuteranopia). Warn and return
    # a best-effort interpolation.
    warning(sprintf(paste0("physioPalette(): %d qualitative colours requested; ",
                          "no qualitative palette is reliably colourblind-safe ",
                          "beyond %d categories. Consider faceting or a ",
                          "sequential/continuous encoding."),
                    n, length(okabe_ito)), call. = FALSE)
    return(grDevices::colorRampPalette(okabe_ito)(n))
  }
  # sequential / diverging via grDevices::hcl.colors (base R, CVD-safe schemes);
  # names match grDevices::hcl.pals() exactly.
  palette_name <- if (type == "sequential") "Viridis" else "Blue-Red 3"
  toupper(substr(grDevices::hcl.colors(n, palette = palette_name), 1L, 7L))
}

#' A clean, accessible ggplot2 theme for x-biosignal figures
#'
#' @param base_size,base_family Passed to the underlying base theme.
#' @return A \code{ggplot2} theme object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() + theme_physio()
#' }
#' @export
theme_physio <- function(base_size = 11, base_family = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("theme_physio() requires the 'ggplot2' package.")
  }
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold"),
      strip.text       = ggplot2::element_text(face = "bold"),
      legend.position  = "bottom"
    )
}

.physio_discrete_scale <- function(aesthetics, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("colorblind-safe scales require the 'ggplot2' package.")
  }
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = function(n) physioPalette(n),
    ...
  )
}

#' Colorblind-safe discrete colour / fill scales for ggplot2
#'
#' @param ... Passed to \code{ggplot2::discrete_scale()}.
#' @return A \code{ggplot2} discrete scale using \code{\link{physioPalette}}.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(
#'       iris,
#'       ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)) +
#'     ggplot2::geom_point() + scale_color_physio() + theme_physio()
#' }
#' @export
scale_color_physio <- function(...) .physio_discrete_scale("colour", ...)

#' @rdname scale_color_physio
#' @export
scale_colour_physio <- scale_color_physio

#' @rdname scale_color_physio
#' @export
scale_fill_physio <- function(...) .physio_discrete_scale("fill", ...)
