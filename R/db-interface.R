#' Lightweight database interface
#'
#' These functions establish a connection to a DuckDB database using the
#' `DBI` interface. They form a minimal skeleton to be expanded with concrete
#' schema management functions in future iterations.
#'
#' @param path Path to a DuckDB database file.
#' @return A database connection object.
#' @export
#' @examples
#' \dontrun{
#' # Connect to an in-memory database
#' con <- connectDatabase()
#'
#' # Connect to a file-based database
#' con <- connectDatabase("experiments.duckdb")
#'
#' # Always disconnect when done
#' disconnectDatabase(con)
#' }
connectDatabase <- function(path = ":memory:") {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' required for database operations. ",
         "Install with: install.packages('duckdb')", call. = FALSE)
  }
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = FALSE)
}

#' @rdname connectDatabase
#' @param con A database connection produced by `connectDatabase()`.
#' @export
disconnectDatabase <- function(con) {
  if (!inherits(con, "DBIConnection")) {
    stop("Object is not a DBI connection", call. = FALSE)
  }
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}
