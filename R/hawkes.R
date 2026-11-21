#' Simulate a Hawkes (self-exciting) process
#'
#' Intensity \eqn{\lambda(t) = \mu + \sum_{t_i<t}\alpha e^{-\beta(t-t_i)}}: each request may
#' trigger follow-up requests (retries, page-load cascades), producing bursts.
#' Simulated with Ogata's thinning algorithm.
#' @param mu Baseline rate (events/s, > 0).
#' @param alpha Jump of the intensity at each event (> 0).
#' @param beta Decay rate (> alpha for stationarity).
#' @param horizon Duration in seconds.
#' @param seed Optional seed.
#' @return Increasing vector of event times.
#' @examples
#' ev <- simulate_hawkes(mu = 5, alpha = 0.6, beta = 1, horizon = 60, seed = 1)
#' length(ev)  # about mu * horizon / (1 - alpha / beta) = 750
#' @export
simulate_hawkes <- function(mu, alpha, beta, horizon, seed = NULL) {
  .check_lambda(mu); .check_lambda(alpha); .check_lambda(beta); .check_lambda(horizon)
  if (alpha >= beta) stop("Need alpha < beta (branching ratio < 1).", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  t <- 0; S <- 0; ev <- numeric(0)
  repeat {
    lam_bar <- mu + S
    w <- stats::rexp(1, lam_bar); t_new <- t + w
    if (t_new > horizon) break
    S_new <- S * exp(-beta * w)
    if (stats::runif(1) * lam_bar <= mu + S_new) { ev <- c(ev, t_new); S <- S_new + alpha } else S <- S_new
    t <- t_new
  }
  ev
}

.hawkes_nll <- function(par, times, horizon) {
  mu <- exp(par[1]); alpha <- exp(par[2]); beta <- exp(par[3])
  n <- length(times); A <- numeric(n)
  if (n > 1) for (i in 2:n) A[i] <- exp(-beta * (times[i] - times[i - 1])) * (1 + A[i - 1])
  -(sum(log(mu + alpha * A)) - mu * horizon - (alpha / beta) * sum(1 - exp(-beta * (horizon - times))))
}

#' Fit a Hawkes process by maximum likelihood
#'
#' Exact O(n) recursive log-likelihood for the exponential kernel.
#' @param times Event times in [0, horizon].
#' @param horizon Observation window length.
#' @return A list with `mu`, `alpha`, `beta`, `branching_ratio` (alpha/beta) and `loglik`.
#' @examples
#' ev <- simulate_hawkes(5, 0.6, 1, 300, seed = 1)
#' fit_hawkes(ev, 300)$branching_ratio
#' @export
fit_hawkes <- function(times, horizon) {
  times <- sort(times)
  start <- log(c(length(times) / horizon * 0.5, 0.5, 1))
  o <- stats::optim(start, .hawkes_nll, times = times, horizon = horizon, method = "Nelder-Mead",
                    control = list(maxit = 2000, reltol = 1e-10))
  p <- exp(o$par)
  list(mu = p[1], alpha = p[2], beta = p[3], branching_ratio = p[2] / p[3], loglik = -o$value)
}

#' Branching ratio of a Hawkes process
#'
#' Average number of "child" events triggered by one event (alpha / beta). The long-run mean
#' rate is `mu / (1 - branching_ratio)`: bursts inflate load by that factor.
#' @param alpha,beta Hawkes parameters.
#' @return A number in (0, 1) when the process is stationary.
#' @export
hawkes_branching_ratio <- function(alpha, beta) alpha / beta
