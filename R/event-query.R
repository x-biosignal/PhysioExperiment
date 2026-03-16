#' EventQuery class for composable event filtering
#'
#' @slot source PhysioExperiment object
#' @slot filters List of filter functions to apply
#' @slot resolved Cached resolved events (NULL if not yet resolved)
#' @exportClass EventQuery
setClass("EventQuery",
  slots = c(
    source = "PhysioExperiment",
    filters = "list",
    resolved = "ANY"  # NULL or data.frame
  ),
  prototype = list(
    filters = list(),
    resolved = NULL
  )
)

#' Create an EventQuery from a PhysioExperiment
#'
#' @param x A PhysioExperiment object
#' @return An EventQuery object
#' @export
#' @examples
#' pe <- make_pe_2d()
#' pe <- addEvents(pe, onset = c(1, 2, 3), type = "stimulus")
#' q <- eventQuery(pe)
eventQuery <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  methods::new("EventQuery", source = x)
}

#' Filter events by type
#'
#' @param q An EventQuery object
#' @param types Character vector of event types to keep
#' @return Modified EventQuery
#' @export
filterType <- function(q, types) {
  stopifnot(inherits(q, "EventQuery"))

  filter_fn <- function(events) {
    events[events$type %in% types, , drop = FALSE]
  }

  q@filters <- c(q@filters, list(filter_fn))
  q@resolved <- NULL  # Invalidate cache
  q
}

#' Resolve an EventQuery to get filtered events
#'
#' @param q An EventQuery object
#' @return Data frame of filtered events
#' @export
resolveQuery <- function(q) {
  stopifnot(inherits(q, "EventQuery"))

  # Return cached result if available
  if (!is.null(q@resolved)) {
    return(q@resolved)
  }

  # Get all events from source
  events_obj <- getEvents(q@source)
  events <- events_obj@events

  # Apply all filters in order
  for (filter_fn in q@filters) {
    events <- filter_fn(events)
  }

  events
}

#' Filter events by value
#'
#' @param q An EventQuery object
#' @param values Character vector of event values to keep
#' @return Modified EventQuery
#' @export
filterValue <- function(q, values) {
  stopifnot(inherits(q, "EventQuery"))

  filter_fn <- function(events) {
    events[events$value %in% values, , drop = FALSE]
  }

  q@filters <- c(q@filters, list(filter_fn))
  q@resolved <- NULL
  q
}
