#' Emit a Physio-ecosystem deprecation warning or error
#'
#' `r lifecycle::badge("stable")`
#'
#' A thin, ecosystem-wide wrapper over [lifecycle::deprecate_warn()] and
#' [lifecycle::deprecate_stop()] so every Physio package signals deprecations the
#' same way. Use `severity = "warn"` during the deprecation window and switch to
#' `severity = "stop"` once the item is defunct. See the ecosystem
#' `DEPRECATION.md` policy for the lifecycle stages and the minimum window.
#'
#' @param when Version string in which the deprecation was introduced, e.g.
#'   `"0.3.0"`.
#' @param what String naming the deprecated function or argument, e.g.
#'   `"old_fn()"` or `"old_fn(arg)"`. Prefix with the package
#'   (`"PhysioECG::old_fn()"`) to attribute it explicitly.
#' @param with Optional string naming the replacement, e.g. `"new_fn()"`.
#' @param details Optional character vector of extra guidance shown to the user.
#' @param severity `"warn"` (default) to emit a deprecation warning, or `"stop"`
#'   to raise a defunct error.
#' @param id Optional deprecation id used to de-duplicate warnings within a
#'   session; ignored when `severity = "stop"`.
#' @return Called for its side effect (a warning or an error); invisibly returns
#'   `NULL`.
#' @seealso [lifecycle::deprecate_warn()], [lifecycle::deprecate_stop()]
#' @family lifecycle
#' @export
#' @examples
#' old_fn <- function(x) {
#'   deprecate_physio("0.3.0", "old_fn()", with = "new_fn()")
#'   x
#' }
#' # Warnings are shown once per session by default; force them for testing with
#' # options(lifecycle_verbosity = "warning").
deprecate_physio <- function(when, what, with = NULL, details = NULL,
                             severity = c("warn", "stop"), id = NULL) {
  severity <- match.arg(severity)
  stopifnot(is.character(when), length(when) == 1L, !is.na(when),
            is.character(what), length(what) == 1L, !is.na(what))
  if (severity == "warn") {
    lifecycle::deprecate_warn(when = when, what = what, with = with,
                              details = details, id = id,
                              user_env = parent.frame())
  } else {
    lifecycle::deprecate_stop(when = when, what = what, with = with,
                              details = details)
  }
  invisible(NULL)
}
