#' Re-referencing Operations for EEG Data
#'
#' Functions for changing the reference electrode in EEG recordings.
#' Re-referencing is a common preprocessing step that affects the spatial
#' distribution of the signal.

#' Re-reference EEG data
#'
#' Changes the reference electrode for EEG recordings. Supports common
#' re-referencing schemes including average reference, linked mastoids,
#' and single electrode reference.
#'
#' @param x A PhysioExperiment object.
#' @param ref_type Type of re-referencing: "average" (common average reference),
#'   "channel" (single channel), "channels" (average of specified channels),
#'   or "REST" (Reference Electrode Standardization Technique).
#' @param ref_channels For "channel" or "channels" type, the channel name(s)
#'   or index/indices to use as reference.
#' @param exclude Channels to exclude from average reference calculation
#'   (e.g., non-EEG channels like EOG, EMG).
#' @param input_assay Input assay name. If NULL, uses default assay.
#' @param output_assay Output assay name. Default is "rereferenced".
#' @param keep_ref Logical. If TRUE, keeps the original reference channel(s)
#'   in the output (zeroed). If FALSE, removes them.
#' @return A PhysioExperiment object with re-referenced data.
#' @details
#' Re-referencing transforms the data by subtracting a reference signal from
#' each channel. The choice of reference affects the spatial distribution
#' and interpretation of the signal.
#'
#' **Average reference** ("average"): Subtracts the mean of all channels at
#' each time point. This is commonly used for high-density EEG and provides
#' a reference-independent measure, but requires good spatial sampling.
#'
#' **Single channel reference** ("channel"): Subtracts the signal from a
#' specified electrode. Common choices include Cz, linked mastoids (A1+A2)/2,
#' or nose reference.
#'
#' **Multi-channel reference** ("channels"): Subtracts the average of multiple
#' specified channels. Useful for linked mastoids or other custom references.
#'
#' @export
#' @examples
#' # Create example EEG data
#' set.seed(123)
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(1000), nrow = 100, ncol = 10)),
#'   rowData = S4Vectors::DataFrame(
#'     label = c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2"),
#'     type = rep("EEG", 10)
#'   ),
#'   samplingRate = 256
#' )
#'
#' # Apply average reference
#' pe_avg <- rereference(pe, ref_type = "average")
#'
#' # Re-reference to a single channel (Cz)
#' pe_cz <- rereference(pe, ref_type = "channel", ref_channels = "C3")
#'
#' # Re-reference to linked mastoids (if available)
#' # pe_linked <- rereference(pe, ref_type = "channels",
#' #                          ref_channels = c("M1", "M2"))
rereference <- function(x, ref_type = c("average", "channel", "channels", "REST"),
                        ref_channels = NULL, exclude = NULL,
                        input_assay = NULL, output_assay = "rereferenced",
                        keep_ref = TRUE) {
  stopifnot(inherits(x, "PhysioExperiment"))
  ref_type <- match.arg(ref_type)

  # Get input data
  if (is.null(input_assay)) {
    input_assay <- defaultAssay(x)
  }

  if (is.na(input_assay)) {
    stop("No assays available", call. = FALSE)
  }

  data <- SummarizedExperiment::assay(x, input_assay)
  dims <- dim(data)
  ch_names <- channelNames(x)
  n_channels <- length(ch_names)

  # Convert channel names to indices
  if (is.character(exclude)) {
    exclude_idx <- match(exclude, ch_names)
    exclude_idx <- exclude_idx[!is.na(exclude_idx)]
  } else if (is.numeric(exclude)) {
    exclude_idx <- exclude
  } else {
    exclude_idx <- integer(0)
  }

  if (is.character(ref_channels)) {
    ref_idx <- match(ref_channels, ch_names)
    if (any(is.na(ref_idx))) {
      missing <- ref_channels[is.na(ref_idx)]
      stop("Reference channels not found: ", paste(missing, collapse = ", "), call. = FALSE)
    }
  } else if (is.numeric(ref_channels)) {
    ref_idx <- ref_channels
  } else {
    ref_idx <- NULL
  }

  # Validate ref_type and ref_channels
  if (ref_type == "channel") {
    if (is.null(ref_idx) || length(ref_idx) != 1) {
      stop("ref_type 'channel' requires exactly one ref_channels value", call. = FALSE)
    }
  } else if (ref_type == "channels") {
    if (is.null(ref_idx) || length(ref_idx) < 1) {
      stop("ref_type 'channels' requires at least one ref_channels value", call. = FALSE)
    }
  }

  # Apply re-referencing based on data dimensionality
  if (length(dims) == 2) {
    result <- .rereferenceMatrix(data, ref_type, ref_idx, exclude_idx)
  } else if (length(dims) == 3) {
    result <- array(NA_real_, dim = dims)
    for (s in seq_len(dims[3])) {
      result[, , s] <- .rereferenceMatrix(data[, , s], ref_type, ref_idx, exclude_idx)
    }
  } else if (length(dims) == 4) {
    result <- array(NA_real_, dim = dims)
    for (ep in seq_len(dims[3])) {
      for (s in seq_len(dims[4])) {
        result[, , ep, s] <- .rereferenceMatrix(data[, , ep, s], ref_type, ref_idx, exclude_idx)
      }
    }
  } else {
    stop("Data must be 2D, 3D, or 4D", call. = FALSE)
  }

  # Handle reference channel removal
  if (!keep_ref && ref_type %in% c("channel", "channels") && !is.null(ref_idx)) {
    # Remove reference channels from data
    keep_idx <- setdiff(seq_len(n_channels), ref_idx)

    if (length(dims) == 2) {
      result <- result[, keep_idx, drop = FALSE]
    } else if (length(dims) == 3) {
      result <- result[, keep_idx, , drop = FALSE]
    } else if (length(dims) == 4) {
      result <- result[, keep_idx, , , drop = FALSE]
    }

    # Subset colData (channel metadata)
    col_data <- SummarizedExperiment::colData(x)
    col_data <- col_data[keep_idx, , drop = FALSE]

    # Subset all existing assays to match new channel count
    old_assays <- SummarizedExperiment::assays(x)
    new_assays <- list()
    for (nm in names(old_assays)) {
      a <- old_assays[[nm]]
      adims <- dim(a)
      if (length(adims) == 2) {
        new_assays[[nm]] <- a[, keep_idx, drop = FALSE]
      } else if (length(adims) == 3) {
        new_assays[[nm]] <- a[, keep_idx, , drop = FALSE]
      } else if (length(adims) == 4) {
        new_assays[[nm]] <- a[, keep_idx, , , drop = FALSE]
      }
    }
    # Add the rereferenced result
    new_assays[[output_assay]] <- result

    # Create new PhysioExperiment with reduced channels
    x <- PhysioExperiment(
      assays = new_assays,
      rowData = SummarizedExperiment::rowData(x),
      colData = col_data,
      metadata = S4Vectors::metadata(x),
      samplingRate = samplingRate(x)
    )
  } else {
    # Store result without removing channels
    assays <- SummarizedExperiment::assays(x)
    assays[[output_assay]] <- result
    SummarizedExperiment::assays(x) <- assays
  }

  # Update metadata
  meta <- S4Vectors::metadata(x)
  old_ref <- meta$reference
  if (ref_type == "average") {
    new_ref <- "average"
  } else if (ref_type == "channel") {
    new_ref <- ch_names[ref_idx]
  } else if (ref_type == "channels") {
    new_ref <- paste(ch_names[ref_idx], collapse = "+")
  } else {
    new_ref <- ref_type
  }
  meta$reference <- new_ref
  meta$previous_reference <- old_ref
  S4Vectors::metadata(x) <- meta

  x
}

#' Re-reference a 2D matrix
#' @noRd
.rereferenceMatrix <- function(data, ref_type, ref_idx, exclude_idx) {
  n_time <- nrow(data)
  n_channels <- ncol(data)

  result <- data

  if (ref_type == "average") {
    # Calculate average excluding specified channels
    include_idx <- setdiff(seq_len(n_channels), exclude_idx)
    ref_signal <- rowMeans(data[, include_idx, drop = FALSE], na.rm = TRUE)

    # Subtract average from all channels
    for (ch in seq_len(n_channels)) {
      result[, ch] <- data[, ch] - ref_signal
    }
  } else if (ref_type == "channel") {
    # Single channel reference
    ref_signal <- data[, ref_idx]

    for (ch in seq_len(n_channels)) {
      result[, ch] <- data[, ch] - ref_signal
    }
  } else if (ref_type == "channels") {
    # Average of multiple channels
    ref_signal <- rowMeans(data[, ref_idx, drop = FALSE], na.rm = TRUE)

    for (ch in seq_len(n_channels)) {
      result[, ch] <- data[, ch] - ref_signal
    }
  } else if (ref_type == "REST") {
    # REST (Reference Electrode Standardization Technique)
    # This is a simplified version - full REST requires lead field matrix
    warning("Full REST requires lead field matrix. Using average reference instead.",
            call. = FALSE)
    include_idx <- setdiff(seq_len(n_channels), exclude_idx)
    ref_signal <- rowMeans(data[, include_idx, drop = FALSE], na.rm = TRUE)

    for (ch in seq_len(n_channels)) {
      result[, ch] <- data[, ch] - ref_signal
    }
  }

  result
}

#' Get current reference
#'
#' Returns the current reference electrode information.
#'
#' @param x A PhysioExperiment object.
#' @return Character string describing the current reference, or NULL if not set.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(100), nrow = 10, ncol = 10)),
#'   samplingRate = 100
#' )
#' pe <- setReference(pe, "Cz")
#' getCurrentReference(pe)  # "Cz"
getCurrentReference <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  S4Vectors::metadata(x)$reference
}

#' Check if data is average referenced
#'
#' @param x A PhysioExperiment object.
#' @return Logical indicating if data is average referenced.
#' @export
#' @examples
#' pe <- PhysioExperiment(
#'   assays = list(raw = matrix(rnorm(100), nrow = 10, ncol = 10)),
#'   samplingRate = 100
#' )
#' pe <- rereference(pe, ref_type = "average")
#' isAverageReferenced(pe)  # TRUE
isAverageReferenced <- function(x) {
  stopifnot(inherits(x, "PhysioExperiment"))
  ref <- S4Vectors::metadata(x)$reference
  !is.null(ref) && ref == "average"
}
