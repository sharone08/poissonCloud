#' Overload probability of a service
#'
#' P(X > capacity) for the number of requests X in one window. With `theta = Inf` the count is
#' Poisson(mean); with finite `theta` it is negative binomial (Var = mean + mean^2 / theta),
#' which models bursty traffic.
#' @param lambda Mean requests per window.
#' @param capacity Capacity per window.
#' @param theta NB size (`Inf` = Poisson).
#' @return Probability.
#' @examples
#' overload_probability(40, 55)             # Poisson
#' overload_probability(40, 55, theta = 20) # bursty: much larger
#' @export
overload_probability <- function(lambda, capacity, theta = Inf) {
  .check_lambda(lambda)
  if (is.infinite(theta)) poisson_cdf(capacity, lambda, lower.tail = FALSE)
  else stats::pnbinom(capacity, size = theta, mu = lambda, lower.tail = FALSE)
}

#' Smallest capacity that meets an SLA
#' @param lambda Mean requests per window.
#' @param sla Share of windows that must be absorbed (default 0.999).
#' @param theta NB size (`Inf` = Poisson).
#' @return Capacity per window.
#' @examples
#' required_capacity(40, 0.999)
#' required_capacity(40, 0.999, theta = 20)
#' @export
required_capacity <- function(lambda, sla = 0.999, theta = Inf) {
  .check_lambda(lambda); .check_prob(sla, "sla")
  if (is.infinite(theta)) poisson_quantile(sla, lambda) else stats::qnbinom(sla, size = theta, mu = lambda)
}

#' Expected overload time over a period
#' @param lambda,capacity,theta See [overload_probability()].
#' @param period_seconds Period length (default one day).
#' @return Expected seconds in overload (if windows are 1 s long).
#' @examples
#' expected_downtime(40, 55) / 60
#' @export
expected_downtime <- function(lambda, capacity, theta = Inf, period_seconds = 86400) {
  overload_probability(lambda, capacity, theta) * period_seconds
}

#' Superposition of independent Poisson streams
#' @param lambdas Rates (> 0).
#' @return The aggregate rate (sum).
#' @examples
#' poisson_superposition(c(12, 18, 10))
#' @export
poisson_superposition <- function(lambdas) {
  if (!is.numeric(lambdas) || length(lambdas) < 1L || anyNA(lambdas) || any(lambdas <= 0))
    stop("`lambdas` must be strictly positive numbers.", call. = FALSE)
  sum(lambdas)
}

#' Thinning of a Poisson stream (e.g. cache misses)
#' @param lambda Input rate.
#' @param p Keep probability in (0, 1].
#' @return The thinned rate lambda * p.
#' @examples
#' poisson_thinning(40, 0.15)
#' @export
poisson_thinning <- function(lambda, p) {
  .check_lambda(lambda)
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || p <= 0 || p > 1) stop("`p` must be in (0, 1].", call. = FALSE)
  lambda * p
}

#' Hourly static-vs-elastic sizing plan
#' @param lambda Mean requests/s per slot.
#' @param per_instance Capacity of one instance (requests/s).
#' @param sla SLA quantile.
#' @param min_instances Redundancy floor.
#' @param theta NB size (`Inf` = Poisson).
#' @return Data frame: slot, lambda, capacity, instances, static_instances.
#' @examples
#' autoscale_savings(autoscale_plan(example_hourly_profile()$lambda, 10))
#' @export
autoscale_plan <- function(lambda, per_instance, sla = 0.999, min_instances = 2, theta = Inf) {
  if (!is.numeric(lambda) || anyNA(lambda) || any(lambda <= 0)) stop("`lambda` must be strictly positive.", call. = FALSE)
  .check_lambda(per_instance)
  cap <- vapply(lambda, required_capacity, numeric(1), sla = sla, theta = theta)
  inst <- pmax(min_instances, ceiling(cap / per_instance))
  data.frame(slot = seq_along(lambda), lambda = lambda, capacity = cap, instances = inst, static_instances = max(inst))
}

#' Savings of elastic sizing versus static peak sizing
#' @param plan Output of [autoscale_plan()].
#' @return Share of instance-slots saved, in [0, 1].
#' @export
autoscale_savings <- function(plan) 1 - sum(plan$instances) / sum(plan$static_instances)

#' Typical 24-hour load profile (requests/s)
#' @return Data frame: hour, lambda.
#' @examples
#' example_hourly_profile()
#' @export
example_hourly_profile <- function() {
  data.frame(hour = 0:23, lambda = c(12, 9, 7, 6, 6, 8, 14, 25, 38, 48, 55, 60, 62, 58, 56, 57, 60, 64, 58, 45, 35, 26, 19, 14))
}
