#' Load a traffic dataset
#'
#' Reads the bundled synthetic dataset (14 days, 10-second bins) or your own CSV / XLSX file
#' with columns `timestamp` and `requests`. XLSX needs the **readxl** package.
#' @param path Path to a `.csv`, `.csv.gz` or `.xlsx` file; `NULL` loads the bundled data.
#' @param sheet Sheet name for XLSX files.
#' @return Data frame with `timestamp` (POSIXct, UTC), `requests`, `is_incident` (if present) and `day` (1, 2, ...).
#' @examples
#' d <- load_traffic()
#' head(d)
#' @export
load_traffic <- function(path = NULL, sheet = "traffic_10s") {
  if (is.null(path)) path <- system.file("extdata", "cloud_traffic_10s.csv.gz", package = "poissonCloud")
  if (!nzchar(path) || !file.exists(path)) stop("Traffic file not found.", call. = FALSE)
  ext <- tolower(tools::file_ext(sub("\\.gz$", "", path)))
  d <- if (ext == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Install the 'readxl' package to read XLSX files.", call. = FALSE)
    as.data.frame(readxl::read_excel(path, sheet = sheet))
  } else utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("timestamp", "requests") %in% names(d))) stop("Need columns `timestamp` and `requests`.", call. = FALSE)
  d$timestamp <- as.POSIXct(d$timestamp, tz = "UTC")
  d$day <- as.integer(floor(as.numeric(difftime(d$timestamp, min(d$timestamp), units = "days")))) + 1L
  d
}
