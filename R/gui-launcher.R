#' Launch PhysioExperiment GUI
#'
#' Starts the PhysioExperiment graphical user interface. This launches a local

#' web server with the Plumber API backend and opens the React-based GUI in
#' either a browser or Electron desktop application.
#'
#' @param port Integer. Port number for the API server. Default is 8000.
#' @param host Character. Host address to bind to. Default is "127.0.0.1" for
#'   local access only. Use "0.0.0.0" to allow external access.
#' @param desktop Logical. If TRUE, attempts to launch the Electron desktop
#'   application. If FALSE (default), opens in the default web browser.
#' @param browser Logical. If TRUE (default), automatically opens the GUI in
#'   the browser. Set to FALSE to start only the API server.
#' @param quiet Logical. If TRUE, suppresses startup messages. Default is FALSE.
#'
#' @return Invisibly returns the plumber object. The function blocks while the
#'   server is running unless run in background mode.
#'
#' @details
#' The GUI provides a modern, responsive interface for:
#' \itemize{
#'   \item Data import and management (EDF, BDF, BrainVision, HDF5, CSV)
#'   \item Signal visualization with multi-channel display
#'   \item Preprocessing (filtering, referencing, resampling)
#'   \item Time-frequency analysis (spectrograms, wavelets)
#'   \item Connectivity analysis (coherence, PLV, PLI)
#'   \item Statistical testing (t-test, ANOVA, cluster permutation)
#'   \item Workflow building for batch processing
#' }
#'
#' The GUI communicates with R through a REST API built on plumber. All
#' computations are performed in R using the PhysioExperiment package.
#'
#' @section Desktop Mode:
#' Desktop mode requires Electron to be installed. The Electron application
#' provides native file access and a more integrated desktop experience.
#' If Electron is not available, the function falls back to browser mode.
#'
#' @section Requirements:
#' The following packages are required and will be loaded automatically:
#' \itemize{
#'   \item plumber - for the REST API server
#'   \item jsonlite - for JSON serialization
#' }
#'
#' @examples
#' \dontrun{
#' # Launch GUI in browser
#' launchGUI()
#'
#' # Launch on a different port
#' launchGUI(port = 3000)
#'
#' # Start API server without opening browser
#' launchGUI(browser = FALSE)
#'
#' # Launch in desktop mode (requires Electron)
#' launchGUI(desktop = TRUE)
#' }
#'
#' @export
launchGUI <- function(port = 8000L,
                      host = "127.0.0.1",
                      desktop = FALSE,
                      browser = TRUE,
                      quiet = FALSE) {
  # Check for required packages
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required for the GUI. ",
         "Install it with: install.packages('plumber')",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for the GUI. ",
         "Install it with: install.packages('jsonlite')",
         call. = FALSE)
  }

  # Locate API file
  api_file <- system.file("plumber", "api.R", package = "PhysioExperiment")
  if (api_file == "") {
    stop("Could not find API definition file. ",
         "Please reinstall PhysioExperiment.",
         call. = FALSE)
  }

  # Locate GUI directory
  gui_dir <- system.file("gui", "dist", package = "PhysioExperiment")
  gui_exists <- dir.exists(gui_dir) && length(list.files(gui_dir)) > 0

  if (!quiet) {
    message("Starting PhysioExperiment GUI...")
    message(sprintf("API Server: http://%s:%d", host, port))
    if (gui_exists) {
      message(sprintf("GUI: http://%s:%d", host, port))
    } else {
      message("Note: Pre-built GUI not found. Running in API-only mode.")
      message("To build the GUI, run: npm run build in inst/gui/")
    }
  }

  # Create plumber API
  pr <- plumber::plumb(api_file)

  # If GUI is built, serve static files
  if (gui_exists) {
    pr <- plumber::pr_static(pr, "/", gui_dir)
  }

  # URL to open
  url <- sprintf("http://%s:%d", host, port)

  # Handle browser/desktop opening
  if (browser && !desktop) {
    # Open in browser after small delay to let server start
    later::later(function() {
      utils::browseURL(url)
    }, delay = 1)
  } else if (desktop) {
    # Try to launch Electron
    electron_dir <- system.file("electron", package = "PhysioExperiment")
    if (dir.exists(electron_dir)) {
      # Check for electron executable
      electron_path <- Sys.which("electron")
      if (electron_path != "") {
        later::later(function() {
          system2(electron_path, c(electron_dir, sprintf("--api-url=%s", url)),
                  wait = FALSE)
        }, delay = 1)
      } else {
        if (!quiet) {
          message("Electron not found. Falling back to browser mode.")
        }
        later::later(function() {
          utils::browseURL(url)
        }, delay = 1)
      }
    } else {
      if (!quiet) {
        message("Electron app not found. Falling back to browser mode.")
      }
      later::later(function() {
        utils::browseURL(url)
      }, delay = 1)
    }
  }

  if (!quiet) {
    message("Press Ctrl+C to stop the server.")
  }

  # Run the API server (blocking)
  pr$run(host = host, port = port, quiet = quiet)

  invisible(pr)
}


#' Start PhysioExperiment API Server (Non-blocking)
#'
#' Starts the API server in the background, allowing R to remain interactive.
#' This is useful for development and testing.
#'
#' @param port Integer. Port number for the API server.
#' @param host Character. Host address to bind to.
#' @param quiet Logical. Suppress startup messages.
#'
#' @return A `plumber` background process handle that can be used to stop
#'   the server with `$kill()`.
#'
#' @examples
#' \dontrun
#' # Start server in background
#' server <- startAPIServer()
#'
#' # Do other work...
#'
#' # Stop server
#' server$kill()
#' }
#'
#' @export
startAPIServer <- function(port = 8000L,
                           host = "127.0.0.1",
                           quiet = FALSE) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required. Install it with: install.packages('plumber')",
         call. = FALSE)
  }

  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required for background server. ",
         "Install it with: install.packages('callr')",
         call. = FALSE)
  }

  api_file <- system.file("plumber", "api.R", package = "PhysioExperiment")
  if (api_file == "") {
    stop("Could not find API definition file.", call. = FALSE)
  }

  gui_dir <- system.file("gui", "dist", package = "PhysioExperiment")
  gui_exists <- dir.exists(gui_dir) && length(list.files(gui_dir)) > 0

  # Start server in background process
  bg <- callr::r_bg(
    function(api_file, gui_dir, gui_exists, host, port, quiet) {
      library(plumber)
      library(PhysioExperiment)

      pr <- plumber::plumb(api_file)
      if (gui_exists) {
        pr <- plumber::pr_static(pr, "/", gui_dir)
      }
      pr$run(host = host, port = port, quiet = quiet)
    },
    args = list(
      api_file = api_file,
      gui_dir = gui_dir,
      gui_exists = gui_exists,
      host = host,
      port = port,
      quiet = quiet
    ),
    supervise = TRUE
  )

  if (!quiet) {
    message(sprintf("API server started at http://%s:%d", host, port))
    message("Use server$kill() to stop.")
  }

  bg
}


#' Check GUI Dependencies
#'
#' Checks if all required dependencies for the GUI are available.
#'
#' @return A list with the status of each dependency.
#'
#' @examples
#' \dontrun{
#' checkGUIDependencies()
#' }
#'
#' @export
checkGUIDependencies <- function() {
  deps <- list(
    plumber = requireNamespace("plumber", quietly = TRUE),
    jsonlite = requireNamespace("jsonlite", quietly = TRUE),
    later = requireNamespace("later", quietly = TRUE),
    callr = requireNamespace("callr", quietly = TRUE)
  )

  # Check for built GUI
  gui_dir <- system.file("gui", "dist", package = "PhysioExperiment")
  deps$gui_built <- dir.exists(gui_dir) && length(list.files(gui_dir)) > 0

  # Check for Electron
  deps$electron <- Sys.which("electron") != ""

  # Check for Node.js (for building)
  deps$nodejs <- Sys.which("node") != ""
  deps$npm <- Sys.which("npm") != ""

  # Print status
  message("PhysioExperiment GUI Dependencies:")
  message("----------------------------------")
  for (name in names(deps)) {
    status <- if (deps[[name]]) "\u2713" else "\u2717"
    message(sprintf("  %s %s", status, name))
  }

  if (!deps$gui_built) {
    message("\nNote: GUI is not built. To build:")
    message("  1. cd inst/gui")
    message("  2. npm install")
    message("  3. npm run build")
  }

  if (!all(deps$plumber, deps$jsonlite, deps$later)) {
    missing <- names(deps)[!unlist(deps[c("plumber", "jsonlite", "later")])]
    message(sprintf("\nInstall missing R packages: install.packages(c('%s'))",
                    paste(missing, collapse = "', '")))
  }

  invisible(deps)
}
