#' Create epochs using sliding window
#'
#' Segments continuous data into overlapping epochs using a sliding window approach.
#' This is useful for time-frequency analysis or when events are not available.
#'
#' @param x A PhysioExperiment object
#' @param window Window size in seconds
#' @param step Step size in seconds (default: window/2 for 50% overlap)
#' @param baseline Optional baseline correction window as c(start, end) in seconds
#'   relative to epoch start. NULL for no correction.
#' @return PhysioExperiment with epoched data
#' @export
#' @examples
#' pe <- make_pe_2d(n_time = 1000, n_channels = 4, sr = 100)
#' # Create 0.5 second windows with 0.1 second step
#' epoched <- epochSliding(pe, window = 0.5, step = 0.1)
epochSliding <- function(x, window, step = NULL, baseline = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (is.null(step)) {
    step <- window / 2
  }

  sr <- samplingRate(x)
  assay_name <- defaultAssay(x)
  data <- SummarizedExperiment::assay(x, assay_name)
  n_timepoints <- dim(data)[1]

  # Calculate window parameters
  window_samples <- as.integer(round(window * sr))
  step_samples <- as.integer(round(step * sr))

  # Generate window start positions (in samples, 1-indexed)
  starts <- seq(1, n_timepoints - window_samples + 1, by = step_samples)
  n_epochs <- length(starts)

  if (n_epochs == 0) {
    stop("Window size exceeds data length", call. = FALSE)
  }

  # Create synthetic events for epochData
  # Convert sample positions to time (0-indexed time)
  event_times <- (starts - 1) / sr

  # Store original events if any
  original_events <- NULL
  if (length(S4Vectors::metadata(x)$events) > 0) {
    original_events <- S4Vectors::metadata(x)$events
  }

  # Add sliding window events
  x <- addEvents(x, onset = event_times, type = ".sliding_window")

  # Use epochData internally
  result <- epochData(x, tmin = 0, tmax = window,
                      events = eventQuery(x) |> filterType(".sliding_window"),
                      baseline = baseline)

  # Update metadata to indicate sliding window origin
  meta <- S4Vectors::metadata(result)
  meta$sliding_window <- list(
    window = window,
    step = step,
    n_windows = n_epochs
  )
  if (!is.null(original_events)) {
    meta$original_events <- original_events
  }
  S4Vectors::metadata(result) <- meta

  result
}
