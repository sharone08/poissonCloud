#' Poisson probability mass function
#'
#' Computes \eqn{P(X = k) = e^{-\lambda}\lambda^k/k!} on the log scale (stable for large lambda).
#' @param k Vector of counts. Non-integer or negative values have probability 0.
#' @param lambda Mean number of events (> 0).
#' @param log Return the log-probability?
#' @return Numeric vector.
#' @examples
#' poisson_pmf(0:5, lambda = 2)
#' @export
poisson_pmf <- function(k, lambda, log = FALSE) {
  .check_lambda(lambda)
  k <- as.numeric(k)
  out <- rep(-Inf, length(k))
  valid <- !is.na(k) & k >= 0 & k == floor(k)
  out[valid] <- k[valid] * base::log(lambda) - lambda - lgamma(k[valid] + 1)
  out[is.na(k)] <- NA_real_
  if (log) out else exp(out)
}

#' Poisson cumulative distribution function
#'
#' Uses \eqn{P(X \le k) = Q(k+1, \lambda)} (regularised incomplete gamma). The upper tail
#' is computed directly, so tiny SLA probabilities keep full precision.
#' @param q Vector of quantiles.
#' @param lambda Mean (> 0).
#' @param lower.tail If `TRUE` returns P(X <= q), otherwise P(X > q).
#' @return Numeric vector of probabilities.
#' @examples
#' poisson_cdf(55, 40, lower.tail = FALSE)
#' @export
poisson_cdf <- function(q, lambda, lower.tail = TRUE) {
  .check_lambda(lambda)
  q <- floor(as.numeric(q))
  out <- numeric(length(q))
  neg <- !is.na(q) & q < 0
  pos <- !is.na(q) & q >= 0
  out[neg] <- if (lower.tail) 0 else 1
  out[pos] <- stats::pgamma(lambda, shape = q[pos] + 1, lower.tail = !lower.tail)
  out[is.na(q)] <- NA_real_
  out
}

#' Poisson quantile function
#'
#' Smallest integer k with P(X <= k) >= p, found from a normal start then corrected exactly.
#' @param p Probabilities in [0, 1].
#' @param lambda Mean (> 0).
#' @return Numeric vector of quantiles (`Inf` for p = 1).
#' @examples
#' poisson_quantile(0.999, 40)
#' @export
poisson_quantile <- function(p, lambda) {
  .check_lambda(lambda)
  one <- function(pi) {
    if (is.na(pi) || pi < 0 || pi > 1) return(NA_real_)
    if (pi == 0) return(0)
    if (pi == 1) return(Inf)
    k <- max(0, floor(lambda + sqrt(lambda) * stats::qnorm(pi)))
    while (poisson_cdf(k, lambda) < pi) k <- k + 1
    while (k > 0 && poisson_cdf(k - 1, lambda) >= pi) k <- k - 1
    k
  }
  vapply(as.numeric(p), one, numeric(1))
}

#' Moments of the Poisson distribution
#' @param lambda Mean (> 0).
#' @return Named vector: mean, variance, sd, skewness, excess kurtosis, mode.
#' @examples
#' poisson_moments(40)
#' @export
poisson_moments <- function(lambda) {
  .check_lambda(lambda)
  c(mean = lambda, variance = lambda, sd = sqrt(lambda), skewness = 1 / sqrt(lambda),
    excess_kurtosis = 1 / lambda, mode = floor(lambda))
}
