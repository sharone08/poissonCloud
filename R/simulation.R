.knuth <- function(lambda) {
  L <- exp(-lambda); k <- 0L; p <- 1
  repeat { p <- p * stats::runif(1); if (p <= L) return(k); k <- k + 1L }
}

#' Simulate Poisson random variables
#'
#' `"knuth"` multiplies uniforms until the product drops below exp(-lambda) (exact, educational;
#' large lambda is split into blocks of at most 30 and summed, by additivity). `"base"` calls `stats::rpois`.
#' @param n Number of draws.
#' @param lambda Mean (> 0).
#' @param method `"knuth"` or `"base"`.
#' @return Integer vector of length `n`.
#' @examples
#' set.seed(1); mean(poisson_rng(1000, 40))
#' @export
poisson_rng <- function(n, lambda, method = c("knuth", "base")) {
  .check_lambda(lambda); method <- match.arg(method)
  if (!is.numeric(n) || length(n) != 1L || n < 0) stop("`n` must be an integer >= 0.", call. = FALSE)
  if (method == "base") return(stats::rpois(n, lambda))
  m <- ceiling(lambda / 30); lam_i <- lambda / m
  vapply(seq_len(n), function(i) sum(vapply(seq_len(m), function(j) .knuth(lam_i), numeric(1))), numeric(1))
}

#' Simulate arrival times of a homogeneous Poisson process
#'
#' Inter-arrival times are i.i.d. exponential with mean 1/rate.
#' @param rate Arrivals per second (> 0).
#' @param duration Seconds (> 0).
#' @param seed Optional seed.
#' @return Increasing vector of arrival times in (0, duration].
#' @examples
#' length(simulate_arrivals(40, 10, seed = 42))
#' @export
simulate_arrivals <- function(rate, duration, seed = NULL) {
  .check_lambda(rate); .check_lambda(duration)
  if (!is.null(seed)) set.seed(seed)
  n_draw <- ceiling(rate * duration + 6 * sqrt(rate * duration) + 10)
  t <- cumsum(stats::rexp(n_draw, rate))
  while (t[length(t)] <= duration) t <- c(t, t[length(t)] + cumsum(stats::rexp(n_draw, rate)))
  t[t <= duration]
}

#' Count arrivals per window
#' @param arrivals Arrival times.
#' @param window Window width in seconds.
#' @param duration Total duration.
#' @return Integer vector of length `floor(duration / window)`.
#' @examples
#' count_arrivals(simulate_arrivals(40, 60, seed = 1), window = 1, duration = 60)[1:5]
#' @export
count_arrivals <- function(arrivals, window = 1, duration) {
  .check_lambda(window); .check_lambda(duration)
  n_win <- floor(duration / window)
  idx <- ceiling(arrivals / window); idx[idx < 1] <- 1
  tabulate(idx[idx <= n_win], nbins = n_win)
}

#' Monte-Carlo estimate of the overload probability
#' @param lambda Mean count per window.
#' @param capacity Capacity per window.
#' @param n_sim Number of simulated windows.
#' @param seed Optional seed.
#' @return List: `estimate`, `se`, `ci`, `exact`.
#' @examples
#' mc_overload(40, 55, n_sim = 1e5, seed = 123)
#' @export
mc_overload <- function(lambda, capacity, n_sim = 1e5, seed = NULL) {
  .check_lambda(lambda)
  if (!is.null(seed)) set.seed(seed)
  x <- poisson_rng(n_sim, lambda, method = "base")
  est <- mean(x > capacity); se <- sqrt(est * (1 - est) / n_sim)
  list(estimate = est, se = se, ci = c(lower = max(0, est - 1.96 * se), upper = min(1, est + 1.96 * se)),
       exact = overload_probability(lambda, capacity))
}
