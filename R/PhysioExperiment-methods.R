#' S4 Methods for PhysioExperiment
#'
#' Standard S4 methods for PhysioExperiment objects including show, subsetting,
#' and combining.

#' Check if object is HDF5-backed
#'
#' Returns FALSE by default in PhysioCore.
#' PhysioIO provides the full implementation.
#'
#' @param x A PhysioExperiment object.
#' @return Logical indicating if backed by HDF5.
#' @export
isHDF5Backed <- function(x) {
  FALSE
}

#' Show method for PhysioExperiment
#'
#' Displays a summary of the PhysioExperiment object.
#'
#' @param object A PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#' pe  # Displays summary
setMethod("show", "PhysioExperiment", function(object) {
  cat("class: PhysioExperiment\n")

  # Dimensions
  assay_name <- defaultAssay(object)
  if (!is.na(assay_name)) {
    data <- SummarizedExperiment::assay(object, assay_name)
    dims <- dim(data)
    cat("dim:", paste(dims, collapse = " x "), "\n")
  }

  # Assays
  assay_names <- SummarizedExperiment::assayNames(object)
  cat("assays(", length(assay_names), "): ",
      paste(head(assay_names, 3), collapse = ", "),
      if (length(assay_names) > 3) " ..." else "", "\n", sep = "")

  # Sampling rate
  sr <- samplingRate(object)
  if (!is.na(sr)) {
    cat("samplingRate:", sr, "Hz\n")
  }

  # Channels
  n_ch <- nChannels(object)
  if (n_ch > 0) {
    ch_names <- channelNames(object)
    cat("channels(", n_ch, "): ",
        paste(head(ch_names, 5), collapse = ", "),
        if (n_ch > 5) " ..." else "", "\n", sep = "")
  }

  # Row data
  row_data <- SummarizedExperiment::rowData(object)
  if (ncol(row_data) > 0) {
    cat("rowData names(", ncol(row_data), "): ",
        paste(head(names(row_data), 5), collapse = ", "),
        if (ncol(row_data) > 5) " ..." else "", "\n", sep = "")
  }

  # Column data
  col_data <- SummarizedExperiment::colData(object)
  if (ncol(col_data) > 0) {
    cat("colData names(", ncol(col_data), "): ",
        paste(head(names(col_data), 5), collapse = ", "),
        if (ncol(col_data) > 5) " ..." else "", "\n", sep = "")
  }

  # Events
  events <- S4Vectors::metadata(object)$events
  if (!is.null(events) && inherits(events, "PhysioEvents")) {
    n_events <- nEvents(events)
    cat("events:", n_events, "\n")
  }

  # HDF5 status
  if (isHDF5Backed(object)) {
    cat("backend: HDF5 (out-of-memory)\n")
  }
})

#' Length method for PhysioExperiment
#'
#' Returns the number of time points.
#'
#' @param x A PhysioExperiment object.
#' @return Integer number of time points.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#' length(pe)  # 100
setMethod("length", "PhysioExperiment", function(x) {
  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) return(0L)

  data <- SummarizedExperiment::assay(x, assay_name)
  dim(data)[1]
})

#' Dim method for PhysioExperiment
#'
#' Returns dimensions of the default assay.
#'
#' @param x A PhysioExperiment object.
#' @return Integer vector of dimensions.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#' dim(pe)  # 100 4
setMethod("dim", "PhysioExperiment", function(x) {
  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) return(NULL)

  dim(SummarizedExperiment::assay(x, assay_name))
})

#' Subset PhysioExperiment by time indices
#'
#' @param x A PhysioExperiment object.
#' @param i Time indices (rows).
#' @param j Channel indices (columns in first non-time dimension).
#' @param ... Additional arguments (not used).
#' @param drop Logical. If TRUE, drops dimensions of size 1.
#' @return A subsetted PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#'
#' # Subset by time
#' pe_subset <- pe[1:50, ]
#' dim(pe_subset)  # 50 4
#'
#' # Subset by channels
#' pe_channels <- pe[, 1:2]
#' dim(pe_channels)  # 100 2
setMethod("[", c("PhysioExperiment", "ANY", "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    assay_names <- SummarizedExperiment::assayNames(x)
    new_assays <- list()

    for (name in assay_names) {
      data <- SummarizedExperiment::assay(x, name)
      dims <- dim(data)

      if (missing(i)) i <- seq_len(dims[1])
      if (missing(j)) j <- seq_len(dims[2])

      if (length(dims) == 2) {
        new_assays[[name]] <- data[i, j, drop = drop]
      } else if (length(dims) == 3) {
        new_assays[[name]] <- data[i, j, , drop = drop]
      } else if (length(dims) == 4) {
        new_assays[[name]] <- data[i, j, , , drop = drop]
      }
    }

    # Subset row data (time points)
    row_data <- SummarizedExperiment::rowData(x)
    if (nrow(row_data) > 0 && !missing(i)) {
      row_data <- row_data[i, , drop = FALSE]
    }

    # Subset col data (channels)
    col_data <- SummarizedExperiment::colData(x)
    if (nrow(col_data) > 0 && !missing(j)) {
      col_data <- col_data[j, , drop = FALSE]
    }

    # Update sampling rate if time was subsetted
    sr <- samplingRate(x)

    # Create new object
    PhysioExperiment(
      assays = S4Vectors::SimpleList(new_assays),
      rowData = row_data,
      colData = col_data,
      metadata = S4Vectors::metadata(x),
      samplingRate = sr
    )
  }
)

#' Combine PhysioExperiment objects by channels
#'
#' Combines two PhysioExperiment objects by adding channels.
#'
#' @param x A PhysioExperiment object.
#' @param y A PhysioExperiment object to combine.
#' @return Combined PhysioExperiment object.
#' @export
#' @examples
#' pe1 <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(200), nrow = 100, ncol = 2)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz")),
#'   samplingRate = 100
#' )
#' pe2 <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(200), nrow = 100, ncol = 2)),
#'   colData = S4Vectors::DataFrame(label = c("Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Combine channels
#' pe_combined <- cbindPhysio(pe1, pe2)
#' nChannels(pe_combined)  # 4
cbindPhysio <- function(x, y) {
  stopifnot(inherits(x, "PhysioExperiment"))
  stopifnot(inherits(y, "PhysioExperiment"))

  # Check compatibility
  sr_x <- samplingRate(x)
  sr_y <- samplingRate(y)

  if (!is.na(sr_x) && !is.na(sr_y) && abs(sr_x - sr_y) > 1e-6) {
    stop("Sampling rates must match for cbind", call. = FALSE)
  }

  # Check time points
  len_x <- length(x)
  len_y <- length(y)

  if (len_x != len_y) {
    stop("Time dimensions must match for cbind", call. = FALSE)
  }

  # Combine assays
  assay_names_x <- SummarizedExperiment::assayNames(x)
  assay_names_y <- SummarizedExperiment::assayNames(y)
  common_assays <- intersect(assay_names_x, assay_names_y)

  if (length(common_assays) == 0) {
    stop("No common assay names found", call. = FALSE)
  }

  new_assays <- list()
  for (name in common_assays) {
    data_x <- SummarizedExperiment::assay(x, name)
    data_y <- SummarizedExperiment::assay(y, name)
    dims_x <- dim(data_x)
    dims_y <- dim(data_y)

    if (length(dims_x) != length(dims_y)) {
      stop("Assay dimensions must match", call. = FALSE)
    }

    if (length(dims_x) == 2) {
      new_assays[[name]] <- cbind(data_x, data_y)
    } else if (length(dims_x) == 3) {
      new_assays[[name]] <- abind::abind(data_x, data_y, along = 2)
    }
  }

  # Combine col data (channels)
  col_data <- rbind(
    SummarizedExperiment::colData(x),
    SummarizedExperiment::colData(y)
  )

  PhysioExperiment(
    assays = S4Vectors::SimpleList(new_assays),
    rowData = SummarizedExperiment::rowData(x),
    colData = col_data,
    metadata = S4Vectors::metadata(x),
    samplingRate = sr_x
  )
}

#' Combine PhysioExperiment objects by time
#'
#' Concatenates two PhysioExperiment objects along the time axis.
#'
#' @param x A PhysioExperiment object.
#' @param y A PhysioExperiment object to concatenate.
#' @return Combined PhysioExperiment object.
#' @export
#' @examples
#' pe1 <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#' pe2 <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#'
#' # Concatenate in time
#' pe_concat <- rbindPhysio(pe1, pe2)
#' length(pe_concat)  # 200
rbindPhysio <- function(x, y) {
  stopifnot(inherits(x, "PhysioExperiment"))
  stopifnot(inherits(y, "PhysioExperiment"))

  # Check compatibility
  sr_x <- samplingRate(x)
  sr_y <- samplingRate(y)

  if (!is.na(sr_x) && !is.na(sr_y) && abs(sr_x - sr_y) > 1e-6) {
    stop("Sampling rates must match for rbind", call. = FALSE)
  }

  # Check channels
  n_ch_x <- nChannels(x)
  n_ch_y <- nChannels(y)

  if (n_ch_x != n_ch_y) {
    stop("Number of channels must match for rbind", call. = FALSE)
  }

  # Combine assays
  assay_names_x <- SummarizedExperiment::assayNames(x)
  assay_names_y <- SummarizedExperiment::assayNames(y)
  common_assays <- intersect(assay_names_x, assay_names_y)

  new_assays <- list()
  for (name in common_assays) {
    data_x <- SummarizedExperiment::assay(x, name)
    data_y <- SummarizedExperiment::assay(y, name)
    dims_x <- dim(data_x)

    if (length(dims_x) == 2) {
      new_assays[[name]] <- rbind(data_x, data_y)
    } else if (length(dims_x) == 3) {
      new_assays[[name]] <- abind::abind(data_x, data_y, along = 1)
    }
  }

  # Adjust events from y
  meta <- S4Vectors::metadata(x)
  events_x <- meta$events
  events_y <- S4Vectors::metadata(y)$events

  if (!is.null(events_x) && !is.null(events_y)) {
    # Offset y events by duration of x
    duration_x <- length(x) / sr_x
    events_y_df <- events_y@events
    events_y_df$onset <- events_y_df$onset + duration_x

    combined_events <- PhysioEvents(
      onset = c(events_x@events$onset, events_y_df$onset),
      duration = c(events_x@events$duration, events_y_df$duration),
      type = c(events_x@events$type, events_y_df$type),
      value = c(events_x@events$value, events_y_df$value)
    )
    meta$events <- combined_events
  }

  # Combine rowData (time points)
  row_data <- rbind(
    SummarizedExperiment::rowData(x),
    SummarizedExperiment::rowData(y)
  )

  PhysioExperiment(
    assays = S4Vectors::SimpleList(new_assays),
    rowData = row_data,
    colData = SummarizedExperiment::colData(x),
    metadata = meta,
    samplingRate = sr_x
  )
}

#' Extract time window
#'
#' Extracts a time window from the signal.
#'
#' @param x A PhysioExperiment object.
#' @param tmin Start time in seconds.
#' @param tmax End time in seconds.
#' @return A PhysioExperiment with the extracted time window.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000), nrow = 1000, ncol = 4)),
#'   samplingRate = 100
#' )
#'
#' # Extract 2 to 5 seconds
#' pe_window <- extractWindow(pe, tmin = 2, tmax = 5)
#' duration(pe_window)  # approximately 3 seconds
extractWindow <- function(x, tmin, tmax) {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    stop("Valid sampling rate required", call. = FALSE)
  }

  n <- length(x)
  duration <- n / sr

  if (tmin < 0) tmin <- 0
  if (tmax > duration) tmax <- duration

  start_idx <- max(1, as.integer(round(tmin * sr)) + 1)
  end_idx <- min(n, as.integer(round(tmax * sr)) + 1)

  x[start_idx:end_idx, ]
}

#' Get signal duration
#'
#' @param x A PhysioExperiment object.
#' @return Duration in seconds.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000), nrow = 1000, ncol = 4)),
#'   samplingRate = 100
#' )
#' duration(pe)  # 10 seconds
duration <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  sr <- samplingRate(x)
  if (is.na(sr) || sr <= 0) {
    return(NA_real_)
  }

  length(x) / sr
}

#' Summary statistics for PhysioExperiment
#'
#' Computes summary statistics for each channel.
#'
#' @param object A PhysioExperiment object.
#' @param ... Additional arguments (not used).
#' @return A data.frame with summary statistics.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#' summary(pe)
setMethod("summary", "PhysioExperiment", function(object, ...) {
  assay_name <- defaultAssay(object)
  if (is.na(assay_name)) {
    return(data.frame())
  }

  data <- SummarizedExperiment::assay(object, assay_name)
  dims <- dim(data)

  # Collapse to 2D if needed
  if (length(dims) > 2) {
    data <- apply(data, c(1, 2), mean, na.rm = TRUE)
  }

  n_channels <- ncol(data)
  ch_names <- channelNames(object)
  if (length(ch_names) == 0) {
    ch_names <- paste0("Ch", seq_len(n_channels))
  }

  stats <- data.frame(
    channel = ch_names,
    min = apply(data, 2, min, na.rm = TRUE),
    max = apply(data, 2, max, na.rm = TRUE),
    mean = apply(data, 2, mean, na.rm = TRUE),
    sd = apply(data, 2, stats::sd, na.rm = TRUE),
    median = apply(data, 2, stats::median, na.rm = TRUE)
  )

  stats
})

#' Coerce to data.frame
#'
#' Converts the default assay to a data.frame.
#'
#' @param x A PhysioExperiment object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... Additional arguments.
#' @return A data.frame.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(12), nrow = 3, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#' df <- as.data.frame(pe)
#' head(df)
setMethod("as.data.frame", "PhysioExperiment", function(x, row.names = NULL,
                                                         optional = FALSE, ...) {
  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) {
    return(data.frame())
  }

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  # For 2D data
  if (length(dims) == 2) {
    df <- as.data.frame(data)
    ch_names <- channelNames(x)
    if (length(ch_names) == ncol(df)) {
      names(df) <- ch_names
    }
    df$time <- timeIndex(x)
    df <- df[, c("time", setdiff(names(df), "time"))]
    return(df)
  }

  # For higher dimensions, return first sample
  if (length(dims) >= 3) {
    data_2d <- data[, , 1]
    df <- as.data.frame(data_2d)
    ch_names <- channelNames(x)
    if (length(ch_names) == ncol(df)) {
      names(df) <- ch_names
    }
    df$time <- timeIndex(x)
    df <- df[, c("time", setdiff(names(df), "time"))]
    return(df)
  }

  data.frame()
})
