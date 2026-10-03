#' S4 Methods for PhysioExperiment
#'
#' Standard S4 methods for PhysioExperiment objects including show, subsetting,
#' and combining.

#' Show method for PhysioExperiment
#'
#' Displays a summary of the PhysioExperiment object.
#'
#' @param object A PhysioExperiment object.
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   printing a human-readable summary to the console.
#' @seealso \code{\link{PhysioExperiment}} for the constructor,
#'   \code{\link{summary,PhysioExperiment-method}} for channel-level statistics,
#'   \code{\link{as.data.frame,PhysioExperiment-method}} for conversion to data.frame
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
      paste(utils::head(assay_names, 3), collapse = ", "),
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
        paste(utils::head(ch_names, 5), collapse = ", "),
        if (n_ch > 5) " ..." else "", "\n", sep = "")
  }

  # Row data
  row_data <- SummarizedExperiment::rowData(object)
  if (ncol(row_data) > 0) {
    cat("rowData names(", ncol(row_data), "): ",
        paste(utils::head(names(row_data), 5), collapse = ", "),
        if (ncol(row_data) > 5) " ..." else "", "\n", sep = "")
  }

  # Column data
  col_data <- SummarizedExperiment::colData(object)
  if (ncol(col_data) > 0) {
    cat("colData names(", ncol(col_data), "): ",
        paste(utils::head(names(col_data), 5), collapse = ", "),
        if (ncol(col_data) > 5) " ..." else "", "\n", sep = "")
  }

  # Events
  events <- S4Vectors::metadata(object)$events
  if (!is.null(events) && inherits(events, "PhysioEvents")) {
    n_events <- nEvents(events)
    cat("events:", n_events, "\n")
  }

  # Provenance / audit trail
  prov <- S4Vectors::metadata(object)[["provenance"]]
  if (!is.null(prov) && length(prov) > 0L) {
    cat("provenance:", length(prov), "steps\n")
  }

  # HDF5 status (check if isHDF5Backed is available from PhysioIO)
  if (exists("isHDF5Backed", mode = "function")) {
    hdf5_check <- get("isHDF5Backed", mode = "function")
    if (hdf5_check(object)) {
      cat("backend: HDF5 (out-of-memory)\n")
    }
  }
})

#' Length method for PhysioExperiment
#'
#' Returns the number of time points (rows) in the default assay.
#'
#' @param x A PhysioExperiment object.
#' @return Integer scalar giving the number of time points (rows) in the
#'   default assay, or \code{0L} if no assays are present.
#' @seealso \code{\link{dim,PhysioExperiment-method}} for full dimensions,
#'   \code{\link{nChannels}} for the number of channels,
#'   \code{\link{duration}} for duration in seconds
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
#' Returns the dimensions of the default assay.
#'
#' @param x A PhysioExperiment object.
#' @return An integer vector of dimensions (time points by channels
#'   by samples for 3D data),
#'   or \code{NULL} if no assays are present.
#' @seealso \code{\link{length,PhysioExperiment-method}} for time point count,
#'   \code{\link{nChannels}} for channel count,
#'   \code{\link{defaultAssay}} for the assay being queried
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
#' Extracts a subset of the \code{PhysioExperiment} by row (time) and/or
#' column (channel) indices, preserving metadata and appending a processing-history
#' entry with the supplied indices and input/output dimensions. Missing indices
#' select the full corresponding axis.
#'
#' @param x A PhysioExperiment object.
#' @param i Time indices (rows).
#' @param j Channel indices (columns in first non-time dimension).
#' @param ... Additional arguments (not used).
#' @param drop Logical. If TRUE, drops dimensions of size 1.
#' @return A \code{PhysioExperiment} object containing only the selected
#'   time points and/or channels, with updated rowData and colData.
#' @seealso \code{\link{extractWindow}} for subsetting by time in seconds,
#'   \code{\link{pickChannels}} for subsetting by channel name,
#'   \code{\link{dropChannels}} for removing specific channels
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
    selection <- list(rows = if (missing(i)) NULL else i,
                      columns = if (missing(j)) NULL else j,
                      rows_missing = missing(i), columns_missing = missing(j),
                      drop = drop, input_dim = dim(x))
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
    out <- PhysioExperiment(
      assays = S4Vectors::SimpleList(new_assays),
      rowData = row_data,
      colData = col_data,
      metadata = S4Vectors::metadata(x),
      samplingRate = sr
    )
    # Retain annotation columns even when the selected axis has zero rows.
    SummarizedExperiment::rowData(out) <- row_data
    SummarizedExperiment::colData(out) <- col_data
    selection$output_dim <- dim(out)
    appendProvenance(out, activity = "subset", params = selection,
                     input_assay = paste(assay_names, collapse = ","),
                     output_assay = paste(assay_names, collapse = ","),
                     package = "PhysioExperiment",
                     software_version = as.character(utils::packageVersion("PhysioExperiment")))
  }
)

#' Combine PhysioExperiment objects by channels
#'
#' Combines two PhysioExperiment objects by concatenating along the channel
#' (column) dimension. Both objects must have the same number of time points
#' and matching sampling rates.
#'
#' @param x A PhysioExperiment object.
#' @param y A PhysioExperiment object to combine.
#' @return A \code{PhysioExperiment} object with channels from both \code{x}
#'   and \code{y}, combined colData, and metadata from \code{x}.
#' @seealso \code{\link{rbindPhysio}} for combining along the time axis,
#'   \code{\link{pickChannels}} for selecting specific channels,
#'   \code{\link{dropChannels}} for removing channels
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
      # For 3D arrays, use abind if available
      if (requireNamespace("abind", quietly = TRUE)) {
        new_assays[[name]] <- abind::abind(data_x, data_y, along = 2)
      } else {
        stop("Package 'abind' required for combining 3D assays", call. = FALSE)
      }
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
#' Concatenates two PhysioExperiment objects along the time (row) axis.
#' Both objects must have the same number of channels and matching sampling
#' rates. Event onsets in \code{y} are offset by the duration of \code{x}.
#'
#' @param x A PhysioExperiment object.
#' @param y A PhysioExperiment object to concatenate.
#' @return A \code{PhysioExperiment} object with time points from both
#'   \code{x} and \code{y} concatenated, combined rowData, and merged events.
#' @seealso \code{\link{cbindPhysio}} for combining along the channel axis,
#'   \code{\link{extractWindow}} for extracting a time window,
#'   \code{[} forgeneral subsetting
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
      if (requireNamespace("abind", quietly = TRUE)) {
        new_assays[[name]] <- abind::abind(data_x, data_y, along = 1)
      } else {
        stop("Package 'abind' required for combining 3D assays", call. = FALSE)
      }
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
#' Extracts a time window from the signal based on start and end times
#' in seconds.
#'
#' @param x A PhysioExperiment object.
#' @param tmin Start time in seconds.
#' @param tmax End time in seconds.
#' @return A \code{PhysioExperiment} object containing only the samples
#'   within the specified time window, with preserved channel and event
#'   metadata.
#' @seealso \code{\link{duration}} for total signal duration,
#'   \code{\link{timeIndex}} for the time vector,
#'   \code{[} forindex-based subsetting,
#'   \code{\link{timeToSamples}} for converting times to indices
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
  dur <- n / sr

  if (tmin < 0) tmin <- 0
  if (tmax > dur) tmax <- dur

  start_idx <- max(1, as.integer(round(tmin * sr)) + 1)
  end_idx <- min(n, as.integer(round(tmax * sr)) + 1)

  x[start_idx:end_idx, ]
}

#' Get signal duration
#'
#' Computes the total duration of the signal in seconds from the number of
#' time points and the sampling rate.
#'
#' @param x A PhysioExperiment object.
#' @return Numeric scalar giving the signal duration in seconds, or
#'   \code{NA_real_} if the sampling rate is not set.
#' @seealso \code{\link{samplingRate}} for the sampling rate,
#'   \code{\link{length,PhysioExperiment-method}} for time point count,
#'   \code{\link{timeIndex}} for the time vector,
#'   \code{\link{extractWindow}} for time-based subsetting
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
#' Computes per-channel summary statistics (min, max, mean, sd, median)
#' for the default assay. For 3D arrays, values are first averaged across
#' the third dimension.
#'
#' @param object A PhysioExperiment object.
#' @param ... Additional arguments (not used).
#' @return A \code{data.frame} with columns \code{channel}, \code{min},
#'   \code{max}, \code{mean}, \code{sd}, and \code{median}, with one row
#'   per channel. Returns an empty \code{data.frame} if no assays are present.
#' @seealso \code{\link{PhysioExperiment}} for the constructor,
#'   \code{\link{as.data.frame,PhysioExperiment-method}} for full data export,
#'   \code{\link{channelNames}} for channel labels
#' @references
#' R Core Team (2024). "R: A Language and Environment for Statistical
#' Computing." R Foundation for Statistical Computing, Vienna, Austria.
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
#' Converts the default assay of a \code{PhysioExperiment} to a
#' \code{data.frame} with a \code{time} column (in seconds) followed by
#' one column per channel.
#'
#' @param x A PhysioExperiment object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... Additional arguments.
#' @return A \code{data.frame} with a \code{time} column and one column per
#'   channel. For 3D arrays, only the first sample (third dimension index 1)
#'   is returned. Returns an empty \code{data.frame} if no assays are present.
#' @seealso \code{\link{summary,PhysioExperiment-method}} for summary statistics,
#'   \code{\link{timeIndex}} for the time vector,
#'   \code{\link{channelNames}} for column names
#' @references
#' Huber, W., et al. (2015). "Orchestrating high-throughput genomic analysis
#' with Bioconductor." \emph{Nature Methods}, 12(2), 115-121.
#' \doi{10.1038/nmeth.3252}
#'
#' Morgan, M., et al. (2022). "S4Vectors: Foundation of vector-like and
#' list-like containers in Bioconductor." R package.
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
