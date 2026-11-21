#' Erlang C: probability that a request has to wait (M/M/c queue)
#'
#' With Poisson arrivals (rate lambda), exponential service (rate mu) and c servers,
#' \deqn{C = \frac{P(c;a)/(1-\rho)}{F(c-1;a)+P(c;a)/(1-\rho)},\ a=\lambda/\mu,\ \rho=a/c}
#' where P and F are the Poisson pmf and cdf with mean a.
#' @param lambda Arrival rate.
#' @param mu Service rate per server.
#' @param c Number of servers.
#' @return Probability of waiting.
#' @examples
#' erlang_c(40, 10, 5)
#' @export
erlang_c <- function(lambda, mu, c) {
  .check_lambda(lambda); .check_lambda(mu)
  if (!is.numeric(c) || length(c) != 1L || c < 1 || c != floor(c)) stop("`c` must be an integer >= 1.", call. = FALSE)
  a <- lambda / mu; rho <- a / c
  if (rho >= 1) stop("Unstable system: need lambda < c * mu.", call. = FALSE)
  t <- poisson_pmf(c, a) / (1 - rho)
  t / (poisson_cdf(c - 1, a) + t)
}

#' M/M/c performance metrics
#' @inheritParams erlang_c
#' @return List: rho, p_wait, Wq, W, Lq, L (Little's law).
#' @examples
#' mmc_metrics(40, 10, 6)
#' @export
mmc_metrics <- function(lambda, mu, c) {
  C <- erlang_c(lambda, mu, c); Wq <- C / (c * mu - lambda); W <- Wq + 1 / mu
  list(rho = lambda / (c * mu), p_wait = C, Wq = Wq, W = W, Lq = lambda * Wq, L = lambda * W)
}

#' P(waiting time > t) in an M/M/c queue
#' @inheritParams erlang_c
#' @param t Threshold in seconds.
#' @return Probability.
#' @examples
#' mmc_wait_exceeds(40, 10, 6, t = 0.05)
#' @export
mmc_wait_exceeds <- function(lambda, mu, c, t) {
  if (!is.numeric(t) || length(t) != 1L || t < 0) stop("`t` must be >= 0.", call. = FALSE)
  erlang_c(lambda, mu, c) * exp(-(c * mu - lambda) * t)
}

#' Fewest servers meeting a latency target
#' @inheritParams erlang_c
#' @param t Wait threshold (s).
#' @param target Maximum P(wait > t).
#' @return Number of servers.
#' @examples
#' min_servers(40, 10, t = 0.05, target = 0.01)
#' @export
min_servers <- function(lambda, mu, t, target) {
  .check_lambda(lambda); .check_lambda(mu); .check_prob(target, "target")
  c <- floor(lambda / mu) + 1
  while (mmc_wait_exceeds(lambda, mu, c, t) > target) c <- c + 1
  c
}
