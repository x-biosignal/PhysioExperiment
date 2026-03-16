#' Event management for PhysioExperiment
#'
#' Functions for managing experimental events (triggers, markers, annotations)
#' within PhysioExperiment objects.

#' PhysioEvents class
#'
#' A simple S4 class to store event information as a DataFrame.
#'
#' @slot events A DataFrame containing event information with columns:
#'   onset (numeric), duration (numeric), type (character), value (character).
#' @exportClass PhysioEvents
setClass(
"PhysioEvents",
  slots = list(
    events = "DFrame"
  ),
  prototype = list(
    events = S4Vectors::DataFrame(
      onset = numeric(0),
      duration = numeric(0),
      type = character(0),
      value = character(0)
    )
  )
)

#' Create a PhysioEvents object
#'
#' @param onset Numeric vector of event onset times in seconds.
#' @param duration Numeric vector of event durations in seconds.
#' @param type Character vector of event types (e.g., "stimulus", "response").
#' @param value Character vector of event values/labels.
#' @return A PhysioEvents object.
#' @export
#' @examples
#' # Create events for a simple experiment
#' events <- PhysioEvents(
#'   onset = c(1.0, 2.5, 4.0, 5.5),
#'   duration = c(0.5, 0.5, 0.5, 0.5),
#'   type = c("stimulus", "response", "stimulus", "response"),
#'   value = c("target", "hit", "distractor", "false_alarm")
#' )
#' events
#'
#' # Create events with single type
#' stim_events <- PhysioEvents(
#'   onset = c(1, 2, 3, 4, 5),
#'   type = "stimulus"
#' )
PhysioEvents <- function(onset = numeric(0), duration = numeric(0),
                         type = character(0), value = character(0)) {
  n <- length(onset)
  if (length(duration) == 0) duration <- rep(0, n)
  if (length(type) == 0) type <- rep("event", n)
  if (length(value) == 0) value <- rep("", n)

  # Validate lengths
  if (!all(c(length(duration), length(type), length(value)) %in% c(1, n))) {
    stop("All arguments must have length 1 or same length as 'onset'", call. = FALSE)
  }

  # Recycle if needed
  if (length(duration) == 1) duration <- rep(duration, n)
  if (length(type) == 1) type <- rep(type, n)
  if (length(value) == 1) value <- rep(value, n)

  df <- S4Vectors::DataFrame(
    onset = as.numeric(onset),
    duration = as.numeric(duration),
    type = as.character(type),
    value = as.character(value)
  )

  # Sort by onset time
  df <- df[order(df$onset), ]

  methods::new("PhysioEvents", events = df)
}

#' Get events from a PhysioExperiment object
#'
#' @param x A PhysioExperiment object.
#' @param type Optional character vector of event types to filter.
#' @return A PhysioEvents object or DataFrame of events.
#' @export
#' @examples
#' # Create PhysioExperiment with events
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000), nrow = 100)),
#'   samplingRate = 100
#' )
#' events <- PhysioEvents(
#'   onset = c(1, 2, 3),
#'   type = c("stimulus", "response", "stimulus")
#' )
#' pe <- setEvents(pe, events)
#'
#' # Get all events
#' getEvents(pe)
#'
#' # Get only stimulus events
#' getEvents(pe, type = "stimulus")
getEvents <- function(x, type = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  events <- S4Vectors::metadata(x)$events
  if (is.null(events)) {
    return(PhysioEvents())
  }

  if (!is.null(type)) {
    df <- events@events
    idx <- df$type %in% type
    events <- PhysioEvents(
      onset = df$onset[idx],
      duration = df$duration[idx],
      type = df$type[idx],
      value = df$value[idx]
    )
  }

  events
}

#' Set events for a PhysioExperiment object
#'
#' @param x A PhysioExperiment object.
#' @param events A PhysioEvents object or a data.frame with columns:
#'   onset, duration, type, value.
#' @return The modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000), nrow = 100)),
#'   samplingRate = 100
#' )
#'
#' # Set events using PhysioEvents object
#' events <- PhysioEvents(onset = c(1, 2, 3), type = "stimulus")
#' pe <- setEvents(pe, events)
#'
#' # Set events using data.frame
#' pe <- setEvents(pe, data.frame(onset = c(1, 2), type = "response"))
setEvents <- function(x, events) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (inherits(events, "data.frame") && !inherits(events, "PhysioEvents")) {
    # Convert data.frame to PhysioEvents
    events <- PhysioEvents(
      onset = events$onset,
      duration = if ("duration" %in% names(events)) events$duration else 0,
      type = if ("type" %in% names(events)) events$type else "event",
      value = if ("value" %in% names(events)) events$value else ""
    )
  }

  stopifnot(inherits(events, "PhysioEvents"))

  meta <- S4Vectors::metadata(x)
  meta$events <- events
  S4Vectors::metadata(x) <- meta
  x
}

#' Add events to a PhysioExperiment object
#'
#' @param x A PhysioExperiment object.
#' @param onset Numeric vector of event onset times in seconds.
#' @param duration Numeric vector of event durations in seconds.
#' @param type Character vector of event types.
#' @param value Character vector of event values/labels.
#' @return The modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000), nrow = 100)),
#'   samplingRate = 100
#' )
#'
#' # Add stimulus events
#' pe <- addEvents(pe, onset = c(1, 2, 3), type = "stimulus")
#'
#' # Add response events
#' pe <- addEvents(pe, onset = c(1.5, 2.5), type = "response", value = c("hit", "hit"))
#' nEvents(pe)  # 5 events total
addEvents <- function(x, onset, duration = 0, type = "event", value = "") {
  stopifnot(inherits(x, "PhysioExperiment"))

  new_events <- PhysioEvents(onset, duration, type, value)

  existing <- getEvents(x)
  existing_df <- existing@events
  new_df <- new_events@events

  combined <- rbind(existing_df, new_df)
  combined <- combined[order(combined$onset), ]

  combined_events <- PhysioEvents(
    onset = combined$onset,
    duration = combined$duration,
    type = combined$type,
    value = combined$value
  )

  setEvents(x, combined_events)
}

#' Remove events from a PhysioExperiment object
#'
#' @param x A PhysioExperiment object.
#' @param type Optional event types to remove. If NULL, removes all events.
#' @param indices Optional integer indices of events to remove.
#' @return The modified PhysioExperiment object.
#' @export
removeEvents <- function(x, type = NULL, indices = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (is.null(type) && is.null(indices)) {
    # Remove all events
    return(setEvents(x, PhysioEvents()))
  }

  events <- getEvents(x)
  df <- events@events

  keep <- rep(TRUE, nrow(df))

  if (!is.null(type)) {
    keep <- keep & !(df$type %in% type)
  }

  if (!is.null(indices)) {
    keep[indices] <- FALSE
  }

  filtered <- PhysioEvents(
    onset = df$onset[keep],
    duration = df$duration[keep],
    type = df$type[keep],
    value = df$value[keep]
  )

  setEvents(x, filtered)
}

#' Convert event times to sample indices
#'
#' @param x A PhysioExperiment object.
#' @param times Numeric vector of times in seconds.
#' @return Integer vector of sample indices.
#' @export
timeToSamples <- function(x, times) {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate is required", call. = FALSE)
  }

  as.integer(round(times * sr)) + 1L
}

#' Convert sample indices to times
#'
#' @param x A PhysioExperiment object.
#' @param samples Integer vector of sample indices.
#' @return Numeric vector of times in seconds.
#' @export
samplesToTime <- function(x, samples) {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate is required", call. = FALSE)
  }

  (samples - 1L) / sr
}

#' Show method for PhysioEvents
#' @param object A PhysioEvents object.
#' @export
setMethod("show", "PhysioEvents", function(object) {
  n <- nrow(object@events)
  cat("PhysioEvents with", n, "events\n")
  if (n > 0) {
    types <- unique(object@events$type)
    cat("Event types:", paste(types, collapse = ", "), "\n")
    if (n <= 10) {
      print(as.data.frame(object@events))
    } else {
      cat("First 5 events:\n")
      print(as.data.frame(object@events[1:5, ]))
      cat("...\n")
    }
  }
})

#' Get number of events
#' @param x A PhysioEvents object.
#' @return Integer count of events.
#' @export
nEvents <- function(x) {
  if (inherits(x, "PhysioEvents")) {
    nrow(x@events)
  } else if (inherits(x, "PhysioExperiment")) {
    nrow(getEvents(x)@events)
  } else {
    stop("Object must be PhysioEvents or PhysioExperiment", call. = FALSE)
  }
}
