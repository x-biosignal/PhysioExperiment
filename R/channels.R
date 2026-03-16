#' Channel information management for PhysioExperiment
#'
#' Functions for managing channel metadata including labels, types, units,
#' and electrode positions.

#' Get channel information
#'
#' Returns channel metadata as a DataFrame.
#' Channel information is stored in colData (columns = channels).
#'
#' @param x A PhysioExperiment object.
#' @return A DataFrame with channel information.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(
#'     label = c("Fz", "Cz", "Pz", "Oz"),
#'     type = rep("EEG", 4)
#'   ),
#'   samplingRate = 100
#' )
#'
#' # Get channel information
#' channelInfo(pe)
#'
#' # Get channel names
#' channelNames(pe)
#'
#' # Get number of channels
#' nChannels(pe)
channelInfo <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  SummarizedExperiment::colData(x)
}

#' Set channel information
#'
#' Updates channel metadata.
#' Channel information is stored in colData (columns = channels).
#'
#' @param x A PhysioExperiment object.
#' @param value A DataFrame with channel information.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#'
#' # Set channel info
#' channelInfo(pe) <- S4Vectors::DataFrame(
#'   label = c("Fz", "Cz", "Pz", "Oz"),
#'   type = rep("EEG", 4)
#' )
`channelInfo<-` <- function(x, value) {
  stopifnot(inherits(x, "PhysioExperiment"))
  SummarizedExperiment::colData(x) <- value
  x
}

#' Get channel names/labels
#'
#' @param x A PhysioExperiment object.
#' @return Character vector of channel names.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#' channelNames(pe)  # c("Fz", "Cz", "Pz", "Oz")
channelNames <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  col_data <- SummarizedExperiment::colData(x)

  if ("label" %in% names(col_data)) {
    as.character(col_data$label)
  } else if (nrow(col_data) > 0) {
    paste0("Ch", seq_len(nrow(col_data)))
  } else {
    # Fall back to number of columns in assay
    n_cols <- ncol(x)
    if (n_cols > 0) {
      paste0("Ch", seq_len(n_cols))
    } else {
      character(0)
    }
  }
}

#' Set channel names/labels
#'
#' @param x A PhysioExperiment object.
#' @param value Character vector of channel names.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#' channelNames(pe) <- c("Fz", "Cz", "Pz", "Oz")
#' channelNames(pe)
`channelNames<-` <- function(x, value) {
  stopifnot(inherits(x, "PhysioExperiment"))

  col_data <- SummarizedExperiment::colData(x)
  col_data$label <- as.character(value)
  SummarizedExperiment::colData(x) <- col_data
  x
}

#' Get number of channels
#'
#' @param x A PhysioExperiment object.
#' @return Integer number of channels.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#' nChannels(pe)  # 4
nChannels <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  assay_name <- defaultAssay(x)
  if (is.na(assay_name)) return(0L)

  data <- SummarizedExperiment::assay(x, assay_name)
  dims <- dim(data)

  if (length(dims) >= 2) dims[2] else 0L
}

#' Set channel types
#'
#' Assigns types (EEG, EMG, EOG, etc.) to channels.
#'
#' @param x A PhysioExperiment object.
#' @param types Named character vector or list mapping channel names/indices to types.
#'   If unnamed, applies types in order.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "EOG1", "EMG1", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Set all channels to same type
#' pe <- setChannelTypes(pe, "EEG")
#'
#' # Set specific channel types by name
#' pe <- setChannelTypes(pe, c(EOG1 = "EOG", EMG1 = "EMG"))
setChannelTypes <- function(x, types) {
  stopifnot(inherits(x, "PhysioExperiment"))

  col_data <- SummarizedExperiment::colData(x)
  n_channels <- nrow(col_data)

  if (is.null(names(types))) {
    # Apply in order
    if (length(types) == 1) {
      col_data$type <- rep(types, n_channels)
    } else if (length(types) == n_channels) {
      col_data$type <- as.character(types)
    } else {
      stop("Length of types must be 1 or match number of channels", call. = FALSE)
    }
  } else {
    # Apply by name
    if (!"type" %in% names(col_data)) {
      col_data$type <- rep(NA_character_, n_channels)
    }

    channel_names <- channelNames(x)

    for (i in seq_along(types)) {
      key <- names(types)[i]
      if (key %in% channel_names) {
        idx <- which(channel_names == key)
        col_data$type[idx] <- types[[i]]
      } else if (suppressWarnings(!is.na(as.integer(key)))) {
        idx <- as.integer(key)
        if (idx >= 1 && idx <= n_channels) {
          col_data$type[idx] <- types[[i]]
        }
      }
    }
  }

  SummarizedExperiment::colData(x) <- col_data
  x
}

#' Set channel units
#'
#' Assigns physical units to channels.
#'
#' @param x A PhysioExperiment object.
#' @param units Character vector or named list of units.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#'
#' # Set all channels to same unit
#' pe <- setChannelUnits(pe, "uV")
setChannelUnits <- function(x, units) {
  stopifnot(inherits(x, "PhysioExperiment"))

  col_data <- SummarizedExperiment::colData(x)
  n_channels <- nrow(col_data)

  if (length(units) == 1) {
    col_data$unit <- rep(units, n_channels)
  } else if (length(units) == n_channels) {
    col_data$unit <- as.character(units)
  } else {
    stop("Length of units must be 1 or match number of channels", call. = FALSE)
  }

  SummarizedExperiment::colData(x) <- col_data
  x
}

#' Get channels by type
#'
#' Returns indices of channels matching specified types.
#'
#' @param x A PhysioExperiment object.
#' @param types Character vector of channel types to match.
#' @return Integer vector of matching channel indices.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(
#'     label = c("Fz", "EOG1", "EMG1", "Oz"),
#'     type = c("EEG", "EOG", "EMG", "EEG")
#'   ),
#'   samplingRate = 100
#' )
#'
#' # Get EEG channels
#' eeg_idx <- getChannelsByType(pe, "EEG")  # c(1, 4)
getChannelsByType <- function(x, types) {
  stopifnot(inherits(x, "PhysioExperiment"))

  col_data <- SummarizedExperiment::colData(x)

  if (!"type" %in% names(col_data)) {
    return(integer(0))
  }

  which(col_data$type %in% types)
}

#' Pick specific channels
#'
#' Creates a new PhysioExperiment with only selected channels.
#'
#' @param x A PhysioExperiment object.
#' @param channels Integer indices or character names of channels to keep.
#' @return A new PhysioExperiment with selected channels.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Pick by index
#' pe_subset <- pickChannels(pe, c(1, 3))
#' nChannels(pe_subset)  # 2
#'
#' # Pick by name
#' pe_frontal <- pickChannels(pe, c("Fz", "Cz"))
pickChannels <- function(x, channels) {
  stopifnot(inherits(x, "PhysioExperiment"))

  n_channels <- nChannels(x)

  # Convert names to indices if needed
  if (is.character(channels)) {
    channel_names <- channelNames(x)
    idx <- match(channels, channel_names)
    if (any(is.na(idx))) {
      missing <- channels[is.na(idx)]
      stop("Channels not found: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    channels <- idx
  } else {
    # Validate numeric indices are within range
    out_of_range <- channels[channels < 1 | channels > n_channels]
    if (length(out_of_range) > 0) {
      stop("Channel indices out of range: ", paste(out_of_range, collapse = ", "),
           " (valid range: 1-", n_channels, ")", call. = FALSE)
    }
  }

  # Subset all assays
  assay_names <- SummarizedExperiment::assayNames(x)
  new_assays <- list()

  for (name in assay_names) {
    data <- SummarizedExperiment::assay(x, name)
    dims <- dim(data)

    if (length(dims) == 2) {
      new_assays[[name]] <- data[, channels, drop = FALSE]
    } else if (length(dims) == 3) {
      new_assays[[name]] <- data[, channels, , drop = FALSE]
    } else if (length(dims) == 4) {
      new_assays[[name]] <- data[, channels, , , drop = FALSE]
    }
  }

  # Subset colData (channels are in columns)
  col_data <- SummarizedExperiment::colData(x)[channels, , drop = FALSE]

  PhysioExperiment(
    assays = S4Vectors::SimpleList(new_assays),
    rowData = SummarizedExperiment::rowData(x),
    colData = col_data,
    metadata = S4Vectors::metadata(x),
    samplingRate = samplingRate(x)
  )
}

#' Drop channels
#'
#' Creates a new PhysioExperiment without specified channels.
#'
#' @param x A PhysioExperiment object.
#' @param channels Integer indices or character names of channels to drop.
#' @return A new PhysioExperiment without specified channels.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Drop by index
#' pe_dropped <- dropChannels(pe, 1)
#' nChannels(pe_dropped)  # 3
#'
#' # Drop by name
#' pe_dropped <- dropChannels(pe, c("Fz", "Oz"))
dropChannels <- function(x, channels) {
  stopifnot(inherits(x, "PhysioExperiment"))

  all_channels <- seq_len(nChannels(x))

  # Convert names to indices if needed
  if (is.character(channels)) {
    channel_names <- channelNames(x)
    idx <- match(channels, channel_names)
    if (any(is.na(idx))) {
      warning("Some channels not found and will be ignored", call. = FALSE)
      idx <- idx[!is.na(idx)]
    }
    channels <- idx
  }

  keep <- setdiff(all_channels, channels)

  if (length(keep) == 0) {
    stop("Cannot drop all channels", call. = FALSE)
  }

  pickChannels(x, keep)
}

#' Rename channels
#'
#' @param x A PhysioExperiment object.
#' @param old_names Character vector of current names.
#' @param new_names Character vector of new names.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Rename channels
#' pe <- renameChannels(pe, c("Fz", "Cz"), c("F3", "C3"))
#' channelNames(pe)
renameChannels <- function(x, old_names, new_names) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (length(old_names) != length(new_names)) {
    stop("old_names and new_names must have same length", call. = FALSE)
  }

  current_names <- channelNames(x)

  for (i in seq_along(old_names)) {
    idx <- which(current_names == old_names[i])
    if (length(idx) > 0) {
      current_names[idx] <- new_names[i]
    }
  }

  channelNames(x) <- current_names
  x
}

#' Set reference electrode
#'
#' Records the reference electrode used for the recording.
#'
#' @param x A PhysioExperiment object.
#' @param reference Character string naming the reference electrode.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#'
#' # Set reference electrode
#' pe <- setReference(pe, "average")
#' getReference(pe)  # "average"
setReference <- function(x, reference) {
  stopifnot(inherits(x, "PhysioExperiment"))

  meta <- S4Vectors::metadata(x)
  meta$reference <- reference
  S4Vectors::metadata(x) <- meta
  x
}

#' Get reference electrode
#'
#' @param x A PhysioExperiment object.
#' @return Character string of reference electrode or NULL.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   samplingRate = 100
#' )
#' pe <- setReference(pe, "Cz")
#' getReference(pe)  # "Cz"
getReference <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  S4Vectors::metadata(x)$reference
}

#' Set electrode positions
#'
#' Assigns 3D electrode positions to channels.
#'
#' @param x A PhysioExperiment object.
#' @param positions A data.frame or matrix with columns x, y, z and rows
#'   matching channels. Row names should match channel names.
#' @return Modified PhysioExperiment object.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(300), nrow = 100, ncol = 3)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz")),
#'   samplingRate = 100
#' )
#'
#' # Set electrode positions
#' positions <- data.frame(
#'   x = c(0, 0, 0),
#'   y = c(0.71, 0, -0.71),
#'   z = c(0.71, 1, 0.71)
#' )
#' pe <- setElectrodePositions(pe, positions)
setElectrodePositions <- function(x, positions) {
  stopifnot(inherits(x, "PhysioExperiment"))

  if (is.matrix(positions)) {
    positions <- as.data.frame(positions)
  }

  if (!all(c("x", "y", "z") %in% names(positions))) {
    stop("positions must have columns x, y, z", call. = FALSE)
  }

  col_data <- SummarizedExperiment::colData(x)
  col_data$pos_x <- positions$x
  col_data$pos_y <- positions$y
  col_data$pos_z <- positions$z

  SummarizedExperiment::colData(x) <- col_data
  x
}

#' Get electrode positions
#'
#' @param x A PhysioExperiment object.
#' @return A data.frame with x, y, z columns or NULL if not set.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(300), nrow = 100, ncol = 3)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz")),
#'   samplingRate = 100
#' )
#' pe <- applyMontage(pe, "10-20")
#' getElectrodePositions(pe)
getElectrodePositions <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))

  col_data <- SummarizedExperiment::colData(x)

  if (!all(c("pos_x", "pos_y", "pos_z") %in% names(col_data))) {
    return(NULL)
  }

  data.frame(
    channel = channelNames(x),
    x = col_data$pos_x,
    y = col_data$pos_y,
    z = col_data$pos_z
  )
}

#' Apply standard montage
#'
#' Applies a standard electrode montage (e.g., 10-20 system).
#'
#' @param x A PhysioExperiment object.
#' @param system Montage system: "10-20", "10-10", or "10-5".
#' @return Modified PhysioExperiment object with electrode positions.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(400), nrow = 100, ncol = 4)),
#'   colData = S4Vectors::DataFrame(label = c("Fz", "Cz", "Pz", "Oz")),
#'   samplingRate = 100
#' )
#'
#' # Apply 10-20 system positions
#' pe <- applyMontage(pe, "10-20")
#' getElectrodePositions(pe)
applyMontage <- function(x, system = c("10-20", "10-10", "10-5")) {
  stopifnot(inherits(x, "PhysioExperiment"))
  system <- match.arg(system)

  # Standard 10-20 positions (simplified spherical coordinates)
  # Keys are uppercase for case-insensitive matching
  montage_10_20 <- list(
    FP1 = c(-0.31, 0.95, 0.0),
    FP2 = c(0.31, 0.95, 0.0),
    F7 = c(-0.81, 0.59, 0.0),
    F3 = c(-0.55, 0.67, 0.45),
    FZ = c(0.0, 0.71, 0.71),
    F4 = c(0.55, 0.67, 0.45),
    F8 = c(0.81, 0.59, 0.0),
    T3 = c(-1.0, 0.0, 0.0),
    C3 = c(-0.71, 0.0, 0.71),
    CZ = c(0.0, 0.0, 1.0),
    C4 = c(0.71, 0.0, 0.71),
    T4 = c(1.0, 0.0, 0.0),
    T5 = c(-0.81, -0.59, 0.0),
    P3 = c(-0.55, -0.67, 0.45),
    PZ = c(0.0, -0.71, 0.71),
    P4 = c(0.55, -0.67, 0.45),
    T6 = c(0.81, -0.59, 0.0),
    O1 = c(-0.31, -0.95, 0.0),
    OZ = c(0.0, -1.0, 0.0),
    O2 = c(0.31, -0.95, 0.0)
  )

  channel_names <- channelNames(x)
  positions <- data.frame(
    x = numeric(length(channel_names)),
    y = numeric(length(channel_names)),
    z = numeric(length(channel_names))
  )

  for (i in seq_along(channel_names)) {
    name <- toupper(channel_names[i])
    if (name %in% names(montage_10_20)) {
      pos <- montage_10_20[[name]]
      positions$x[i] <- pos[1]
      positions$y[i] <- pos[2]
      positions$z[i] <- pos[3]
    } else {
      positions$x[i] <- NA
      positions$y[i] <- NA
      positions$z[i] <- NA
    }
  }

  setElectrodePositions(x, positions)
}
