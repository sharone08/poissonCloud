# Internal validation helpers -------------------------------------------------
.check_lambda <- function(lambda) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) ||
      !is.finite(lambda) || lambda <= 0) {
    stop("`lambda` must be a single finite number > 0.", call. = FALSE)
  }
  invisible(lambda)
}
.check_prob <- function(p, name = "p") {
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || p <= 0 || p >= 1) {
    stop(sprintf("`%s` must be in the open interval (0, 1).", name), call. = FALSE)
  }
  invisible(p)
}
.check_counts <- function(x) {
  if (!is.numeric(x) || length(x) < 2L || anyNA(x) || any(x < 0) || any(x != floor(x))) {
    stop("`x` must be a vector of at least 2 non-negative integers (no NA).", call. = FALSE)
  }
  as.integer(x)
}
# CDF/survival of the count model: Poisson if theta is infinite, else negative binomial
.cdf_count <- function(q, mean, theta = Inf, lower.tail = TRUE) {
  if (is.infinite(theta)) stats::ppois(q, mean, lower.tail = lower.tail)
  else stats::pnbinom(q, size = theta, mu = mean, lower.tail = lower.tail)
}
.q_count <- function(p, mean, theta = Inf) {
  if (is.infinite(theta)) stats::qpois(p, mean) else stats::qnbinom(p, size = theta, mu = mean)
}
