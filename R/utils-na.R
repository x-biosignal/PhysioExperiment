#' NA Value Handling Utilities for PhysioExperiment
#'
#' This file provides standardized NA handling functions and documents
#' the NA handling policy for the PhysioExperiment package.
#'
#' @section NA Handling Policy:
#'
#' The PhysioExperiment package follows these principles for NA handling:
#'
#' 1. **Preservation**: NA values in input data are preserved by default.
#'    Functions do not remove or replace NA values unless explicitly requested.
#'

#' 2. **Propagation**: Operations that encounter NA values will propagate them
#'    in the output (standard R behavior) unless na.rm = TRUE is specified.
#'
#' 3. **Explicit Control**: Functions that can handle NA values provide
#'    na.rm or na.action parameters for user control.
#'
#' 4. **Documentation**: Each function documents its NA handling behavior.
#'
#' 5. **Validation**: Input validation functions check for NA and provide
#'    informative warnings or errors as appropriate.
#'
#' @name na-handling
#' @keywords internal
NULL

#' Check for NA values in assay data
#'
#' Validates assay data for NA values and reports statistics.
#'
#' @param x A PhysioExperiment object or numeric array.
#' @param action Action to take: "warn" (default), "error", or "none".
#' @return Invisibly returns a list with NA statistics.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(c(1, NA, 3, 4), nrow = 2)),
#'   samplingRate = 100
#' )
#' checkNA(pe)
checkNA <- function(x, action = c("warn", "error", "none")) {
  action <- match.arg(action)

  if (inherits(x, "PhysioExperiment")) {
    assay_name <- defaultAssay(x)
    if (is.na(assay_name)) {
      return(invisible(list(has_na = FALSE, n_na = 0, pct_na = 0)))
    }
    data <- SummarizedExperiment::assay(x, assay_name)
  } else {
    data <- x
  }

  n_na <- sum(is.na(data))
  n_total <- length(data)
  pct_na <- 100 * n_na / n_total

  result <- list(
    has_na = n_na > 0,
    n_na = n_na,
    n_total = n_total,
    pct_na = pct_na
  )

  if (n_na > 0) {
    msg <- sprintf("Data contains %d NA values (%.2f%%)", n_na, pct_na)

    if (action == "warn") {
      warning(msg, call. = FALSE)
    } else if (action == "error") {
      stop(msg, call. = FALSE)
    }
  }

  invisible(result)
}

#' Handle NA values in signal data
#'
#' Provides various strategies for handling NA values in signal data.
#'
#' @param x Numeric vector or matrix.
#' @param method Method for handling NA: "omit" (remove), "interpolate" (linear),
#'   "zero" (replace with 0), "mean" (replace with mean), "locf" (last observation
#'   carried forward), or "none" (no action).
#' @param ... Additional arguments passed to interpolation methods.
#' @return Data with NA values handled according to the specified method.
#' @export
#' @examples
#' x <- c(1, NA, 3, NA, 5)
#'
#' # Linear interpolation
#' handleNA(x, method = "interpolate")
#'
#' # Replace with mean
#' handleNA(x, method = "mean")
#'
#' # Last observation carried forward
#' handleNA(x, method = "locf")
handleNA <- function(x, method = c("interpolate", "omit", "zero", "mean", "locf", "none"),
                     ...) {
  method <- match.arg(method)

  if (method == "none") {
    return(x)
  }

  is_matrix <- is.matrix(x)
  if (is_matrix) {
    # Apply to each column
    result <- apply(x, 2, handleNA, method = method, ...)
    return(result)
  }

  if (!any(is.na(x))) {
    return(x)
  }

  switch(method,
    omit = {
      x[!is.na(x)]
    },
    interpolate = {
      .interpolateNA(x)
    },
    zero = {
      x[is.na(x)] <- 0
      x
    },
    mean = {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
      x
    },
    locf = {
      .locf(x)
    }
  )
}

#' Linear interpolation for NA values
#' @noRd
.interpolateNA <- function(x) {
  na_idx <- which(is.na(x))
  if (length(na_idx) == 0) return(x)

  valid_idx <- which(!is.na(x))
  if (length(valid_idx) < 2) {
    # Can't interpolate with fewer than 2 points
    return(x)
  }

  # Use linear interpolation
  x[na_idx] <- stats::approx(
    x = valid_idx,
    y = x[valid_idx],
    xout = na_idx,
    rule = 2  # Extrapolate using nearest value
  )$y

  x
}

#' Last observation carried forward
#' @noRd
.locf <- function(x) {
  na_idx <- which(is.na(x))
  if (length(na_idx) == 0) return(x)

  for (i in na_idx) {
    if (i == 1) {
      # Find first non-NA value
      first_valid <- which(!is.na(x))[1]
      if (!is.na(first_valid)) {
        x[1] <- x[first_valid]
      }
    } else {
      x[i] <- x[i - 1]
    }
  }

  x
}

#' Fill NA values at edges
#'
#' Fills NA values at the beginning and end of a signal that may result
#' from filtering operations.
#'
#' @param x Numeric vector.
#' @param method Fill method: "extend" (extend nearest valid value) or
#'   "zero" (fill with zeros).
#' @return Vector with edge NA values filled.
#' @export
#' @examples
#' x <- c(NA, NA, 1, 2, 3, NA, NA)
#' fillEdgeNA(x, method = "extend")
fillEdgeNA <- function(x, method = c("extend", "zero")) {
  method <- match.arg(method)

  if (!any(is.na(x))) return(x)

  valid_idx <- which(!is.na(x))
  if (length(valid_idx) == 0) return(x)

  first_valid <- min(valid_idx)
  last_valid <- max(valid_idx)

  if (method == "extend") {
    if (first_valid > 1) {
      x[1:(first_valid - 1)] <- x[first_valid]
    }
    if (last_valid < length(x)) {
      x[(last_valid + 1):length(x)] <- x[last_valid]
    }
  } else {
    if (first_valid > 1) {
      x[1:(first_valid - 1)] <- 0
    }
    if (last_valid < length(x)) {
      x[(last_valid + 1):length(x)] <- 0
    }
  }

  x
}

#' Check if data contains any NA values
#'
#' Quick check for NA presence in PhysioExperiment data.
#'
#' @param x A PhysioExperiment object.
#' @param assay_name Optional specific assay to check.
#' @return Logical indicating presence of NA values.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(1:4, nrow = 2)),
#'   samplingRate = 100
#' )
#' hasNA(pe)
hasNA <- function(x, assay_name = NULL) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (is.null(assay_name)) {
    assay_name <- defaultAssay(x)
  }

  if (is.na(assay_name)) return(FALSE)

  data <- SummarizedExperiment::assay(x, assay_name)
  anyNA(data)
}

#' Get NA summary for all assays
#'
#' Returns a summary of NA values across all assays.
#'
#' @param x A PhysioExperiment object.
#' @return A data.frame with NA statistics for each assay.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(
#'     raw = matrix(c(1, NA, 3, 4), nrow = 2),
#'     filtered = matrix(1:4, nrow = 2)
#'   ),
#'   samplingRate = 100
#' )
#' naSummary(pe)
naSummary <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_names <- SummarizedExperiment::assayNames(x)

  if (length(assay_names) == 0) {
    return(data.frame(
      assay = character(0),
      n_na = integer(0),
      n_total = integer(0),
      pct_na = numeric(0)
    ))
  }

  results <- lapply(assay_names, function(name) {
    data <- SummarizedExperiment::assay(x, name)
    n_na <- sum(is.na(data))
    n_total <- length(data)
    data.frame(
      assay = name,
      n_na = n_na,
      n_total = n_total,
      pct_na = 100 * n_na / n_total
    )
  })

  do.call(rbind, results)
}

#' Replace NA values in assay
#'
#' Creates a new assay with NA values handled according to the specified method.
#'
#' @param x A PhysioExperiment object.
#' @param method Method for handling NA (see handleNA).
#' @param input_assay Input assay name. If NULL, uses default assay.
#' @param output_assay Output assay name. Default is "na_handled".
#' @return Modified PhysioExperiment with new assay.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(c(1, NA, 3, NA, 5, 6), nrow = 3)),
#'   samplingRate = 100
#' )
#'
#' # Interpolate NA values
#' pe <- replaceNA(pe, method = "interpolate")
replaceNA <- function(x, method = "interpolate",
                      input_assay = NULL, output_assay = "na_handled") {
  stopifnot(inherits(x, "PhysioExperiment"))

  # Validate method - "omit" is not allowed for assay replacement

  # because it changes the number of rows/time points
  valid_methods <- c("interpolate", "zero", "mean", "locf")
  if (!method %in% valid_methods) {
    stop(sprintf(
      "Method '%s' is not supported for replaceNA. Use one of: %s. ",
      method, paste(valid_methods, collapse = ", ")
    ), "The 'omit' method cannot be used because it changes array dimensions.",
    call. = FALSE)
  }

  if (is.null(input_assay)) {
    input_assay <- defaultAssay(x)
  }

  if (is.na(input_assay)) {
    stop("No assays available", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, input_assay)
  dims <- dim(data)

  # Apply NA handling along time dimension
  if (length(dims) == 2) {
    result <- apply(data, 2, handleNA, method = method)
  } else if (length(dims) == 3) {
    result <- array(NA_real_, dim = dims)
    for (s in seq_len(dims[3])) {
      result[, , s] <- apply(data[, , s], 2, handleNA, method = method)
    }
  } else {
    stop("Data must be 2D or 3D", call. = FALSE)
  }

  dim(result) <- dims

  assays <- SummarizedExperiment::assays(x)
  assays[[output_assay]] <- result
  SummarizedExperiment::assays(x) <- assays

  x
}
