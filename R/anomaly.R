#' Detect traffic anomalies against a forecast
#'
#' A bin is flagged when the observed count is in the extreme tail of the forecast distribution
#' (per-tail p-value < `alpha`, Poisson if `theta = Inf`, negative binomial otherwise). An alarm
#' needs `min_run` consecutive flagged bins, which removes isolated false positives.
#' @param counts Observed counts per bin.
#' @param mean Expected count per bin (scalar or vector, e.g. `predict(model, time)`).
#' @param theta NB size (`Inf` = Poisson).
#' @param alpha Per-tail threshold (default 1e-4).
#' @param min_run Consecutive flagged bins required for an alarm (default 3).
#' @return Data frame: index, count, expected, p_value, flag, alarm.
#' @examples
#' detect_anomalies(c(40, 38, 45, 200, 210, 220, 41), mean = 40, min_run = 3)
#' @export
detect_anomalies <- function(counts, mean, theta = Inf, alpha = 1e-4, min_run = 3) {
  if (!is.numeric(counts) || anyNA(counts) || any(counts < 0)) stop("`counts` must be non-negative with no NA.", call. = FALSE)
  .check_prob(alpha, "alpha")
  n <- length(counts); mean <- rep_len(mean, n)
  p <- pmin(.cdf_count(counts, mean, theta), .cdf_count(counts - 1, mean, theta, lower.tail = FALSE))
  flag <- p < alpha
  r <- rle(flag); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
  alarm <- rep(FALSE, n)
  for (i in which(r$values & r$lengths >= min_run)) alarm[starts[i]:ends[i]] <- TRUE
  data.frame(index = seq_len(n), count = counts, expected = mean, p_value = p, flag = flag, alarm = alarm)
}
