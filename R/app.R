#' Launch the Shiny dashboard
#'
#' Interactive digital twin: risk calculator (Poisson vs bursty), autoscaling lab and anomaly
#' detector. Requires the **shiny** package.
#' @param ... Passed to [shiny::runApp()] (e.g. `port = 8080`).
#' @return Called for its side effect.
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Install the 'shiny' package first.", call. = FALSE)
  shiny::runApp(system.file("app", package = "poissonCloud"), ...)
}

#' Launch the REST API
#'
#' Exposes `/risk`, `/capacity` and `/health` as JSON so that any front-end (including
#' `web/index.html`) can call the R code. Requires the **plumber** package.
#' @param port Port (default 8000).
#' @param host Host (default `"127.0.0.1"`).
#' @return Called for its side effect.
#' @export
run_api <- function(port = 8000, host = "127.0.0.1") {
  if (!requireNamespace("plumber", quietly = TRUE)) stop("Install the 'plumber' package first.", call. = FALSE)
  pr <- plumber::plumb(system.file("plumber", "api.R", package = "poissonCloud"))
  plumber::pr_run(pr, host = host, port = port)
}
