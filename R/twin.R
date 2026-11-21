# Digital twin of an autoscaling cluster --------------------------------------

#' Simulate an autoscaling cluster against a traffic trace
#'
#' Discrete-time fluid queue. In each bin the cluster can serve `instances * capacity * bin_seconds`
#' requests; the rest joins a backlog, capped at `buffer_seconds` of capacity (excess is dropped).
#' A bin violates the SLO when the backlog needs more than `latency_slo` seconds to drain.
#' @param counts Requests per bin.
#' @param instances Active instances per bin (same length).
#' @param bin_seconds Bin width (s).
#' @param capacity Requests/s one instance can serve.
#' @param latency_slo Maximum acceptable queueing delay (s).
#' @param buffer_seconds Queue buffer, in seconds of cluster capacity.
#' @param price_per_hour Price of one instance-hour.
#' @return List: cost, instance_hours, violation_rate, dropped_share, avg_instances, violation (logical vector).
#' @examples
#' simulate_autoscaling(rep(500, 100), rep(4, 100))$violation_rate
#' @export
simulate_autoscaling <- function(counts, instances, bin_seconds = 10, capacity = 15, latency_slo = 1,
                                 buffer_seconds = 5, price_per_hour = 0.10) {
  n <- length(counts)
  if (length(instances) != n) stop("`counts` and `instances` must have the same length.", call. = FALSE)
  cap_rate <- instances * capacity; cap <- cap_rate * bin_seconds
  backlog <- 0; dropped <- 0; viol <- logical(n)
  for (t in seq_len(n)) {
    load <- counts[t] + backlog; served <- min(load, cap[t]); backlog <- load - served
    buf <- buffer_seconds * cap_rate[t]
    if (backlog > buf) { dropped <- dropped + backlog - buf; backlog <- buf }
    viol[t] <- backlog / cap_rate[t] > latency_slo
  }
  hours <- sum(instances) * bin_seconds / 3600
  list(cost = hours * price_per_hour, instance_hours = hours, violation_rate = mean(viol),
       dropped_share = dropped / sum(counts), avg_instances = mean(instances), violation = viol)
}

.roll_max <- function(d, back) {
  n <- length(d); out <- numeric(n)
  for (t in seq_len(n)) out[t] <- max(d[max(1, t - back):t])
  out
}
.shift_apply <- function(d, delay_bins, cooldown_bins, min_instances) {
  n <- length(d); a <- rep(min_instances, n); r <- .roll_max(d, cooldown_bins)
  if (n > delay_bins) a[(delay_bins + 1):n] <- r[seq_len(n - delay_bins)]
  a
}

#' Autoscaling policies
#'
#' * `policy_static()`: fixed fleet.
#' * `policy_reactive()`: size from the previous bin's load and a target utilisation; effective after the boot delay.
#' * `policy_predictive()`: size from a forecast quantile of the next bin (pre-provisioned, so no boot delay).
#' * `policy_hybrid()`: predictive floor plus a *surge guard* that scales out when the previous bin exceeded the forecast 99.99% quantile.
#'
#' Scale-in is delayed by `cooldown_bins` in all dynamic policies.
#' @param n Number of bins.
#' @param instances Fleet size for `policy_static()`.
#' @param counts Observed counts per bin.
#' @param util Target utilisation in (0, 1].
#' @param q Forecast count quantile per bin (e.g. from [traffic_quantile()]).
#' @param mean,theta Forecast mean and NB size, for the surge guard.
#' @param p Forecast quantile level for the predictive part.
#' @param guard_util Utilisation target used by the surge guard.
#' @param guard_p Quantile above which a bin counts as a surge.
#' @param bin_seconds Bin width (s).
#' @param capacity Requests/s per instance.
#' @param delay_bins Boot delay in bins (default 6 = 60 s with 10 s bins).
#' @param cooldown_bins Scale-in cooldown in bins (default 18 = 3 min).
#' @param min_instances Redundancy floor.
#' @return Integer-valued vector of active instances per bin.
#' @name policies
NULL

#' @rdname policies
#' @export
policy_static <- function(n, instances) rep(instances, n)

#' @rdname policies
#' @export
policy_reactive <- function(counts, util, bin_seconds = 10, capacity = 15, delay_bins = 6,
                            cooldown_bins = 18, min_instances = 2) {
  n <- length(counts); d <- rep(min_instances, n)
  if (n > 1) d[2:n] <- pmax(min_instances, ceiling(counts[1:(n - 1)] / bin_seconds / (capacity * util)))
  .shift_apply(d, delay_bins, cooldown_bins, min_instances)
}

#' @rdname policies
#' @export
policy_predictive <- function(q, bin_seconds = 10, capacity = 15, cooldown_bins = 18, min_instances = 2) {
  .roll_max(pmax(min_instances, ceiling(q / (capacity * bin_seconds))), cooldown_bins)
}

#' @rdname policies
#' @export
policy_hybrid <- function(counts, mean, theta, p, bin_seconds = 10, capacity = 15, delay_bins = 6,
                          cooldown_bins = 18, min_instances = 2, guard_util = 0.8, guard_p = 0.9999) {
  n <- length(counts); base <- policy_predictive(.q_count(p, mean, theta), bin_seconds, capacity, cooldown_bins, min_instances)
  d <- rep(min_instances, n)
  if (n > 1) {
    flag <- counts[1:(n - 1)] > .q_count(guard_p, mean[1:(n - 1)], theta)
    need <- ceiling(counts[1:(n - 1)] / bin_seconds / (capacity * guard_util))
    d[2:n] <- ifelse(flag, pmax(min_instances, need), min_instances)
  }
  pmax(base, .shift_apply(d, delay_bins, cooldown_bins, min_instances))
}

#' Tune a policy parameter: cheapest setting that meets the SLO with a safety margin
#' @param simulate_fn Function taking a parameter and returning a [simulate_autoscaling()] result.
#' @param grid Candidate parameters.
#' @param slo Maximum acceptable violation rate (default 0.001).
#' @param margin Fraction of the SLO used as safety margin on validation data (default 0.25).
#' @return List: `best` (one-row data frame) and `table` (all candidates).
#' @examples
#' f <- function(u) simulate_autoscaling(rep(500, 50), policy_reactive(rep(500, 50), u))
#' tune_policy(f, c(0.5, 0.8))$best
#' @export
tune_policy <- function(simulate_fn, grid, slo = 1e-3, margin = 0.25) {
  res <- do.call(rbind, lapply(grid, function(p) {
    r <- simulate_fn(p); data.frame(param = p, cost = r$cost, violation_rate = r$violation_rate)
  }))
  ok <- res[res$violation_rate <= slo * margin, , drop = FALSE]
  best <- if (nrow(ok)) ok[which.min(ok$cost), , drop = FALSE] else res[which.min(res$violation_rate), , drop = FALSE]
  list(best = best, table = res)
}

#' Compare autoscaling policies on a train / validation / test split
#'
#' Tunes each policy on `valid`, then evaluates on `test`: static peak, reactive, Poisson
#' predictive, negative-binomial predictive and hybrid (NB + surge guard).
#' @param counts Requests per bin.
#' @param time `POSIXct` bin timestamps.
#' @param model A fitted [fit_traffic_model()] (family `"nb"`), trained on `train`.
#' @param train,valid,test Integer index vectors.
#' @param bin_seconds,capacity,delay_bins,cooldown_bins,min_instances See [policies].
#' @param price Price per instance-hour.
#' @param slo SLO on the violation rate.
#' @param margin Safety margin used when tuning.
#' @param grids Optional named list of parameter grids (`reactive`, `poisson`, `nb`, `hybrid`).
#' @return List: `test` (data frame of tuned policies), `pareto` (all test operating points),
#'   `static_instances`.
#' @export
compare_policies <- function(counts, time, model, train, valid, test, bin_seconds = 10, capacity = 15,
                             price = 0.10, slo = 1e-3, margin = 0.25, delay_bins = 6, cooldown_bins = 18,
                             min_instances = 2, grids = NULL) {
  if (is.null(grids)) grids <- list(reactive = c(.3, .4, .5, .6, .7, .8), poisson = c(.9, .99, .999, .99999, .9999999),
                                    nb = c(.9, .99, .999, .9999, .99999), hybrid = c(.9, .99, .999, .9999, .99999))
  mu <- predict(model, time)
  make <- function(name, par, idx) switch(name,
    reactive = policy_reactive(counts[idx], par, bin_seconds, capacity, delay_bins, cooldown_bins, min_instances),
    poisson = policy_predictive(stats::qpois(par, mu[idx]), bin_seconds, capacity, cooldown_bins, min_instances),
    nb = policy_predictive(stats::qnbinom(par, size = model$theta, mu = mu[idx]), bin_seconds, capacity, cooldown_bins, min_instances),
    hybrid = policy_hybrid(counts[idx], mu[idx], model$theta, par, bin_seconds, capacity, delay_bins, cooldown_bins, min_instances))
  sim <- function(name, par, idx) simulate_autoscaling(counts[idx], make(name, par, idx), bin_seconds, capacity, price_per_hour = price)
  n_static <- ceiling(stats::quantile(counts[train], 0.999) / (capacity * bin_seconds))
  rows <- list(); pareto <- list()
  r <- simulate_autoscaling(counts[test], policy_static(length(test), n_static), bin_seconds, capacity, price_per_hour = price)
  rows[["static_peak"]] <- data.frame(policy = "static_peak", param = NA_real_, cost = r$cost, violation_rate = r$violation_rate,
                                      dropped_share = r$dropped_share, avg_instances = r$avg_instances)
  for (nm in names(grids)) {
    tu <- tune_policy(function(p) sim(nm, p, valid), grids[[nm]], slo, margin)
    r <- sim(nm, tu$best$param, test)
    rows[[nm]] <- data.frame(policy = nm, param = tu$best$param, cost = r$cost, violation_rate = r$violation_rate,
                             dropped_share = r$dropped_share, avg_instances = r$avg_instances)
    pareto[[nm]] <- do.call(rbind, lapply(grids[[nm]], function(p) {
      rr <- sim(nm, p, test); data.frame(policy = nm, param = p, cost = rr$cost, violation_rate = rr$violation_rate)
    }))
  }
  list(test = do.call(rbind, rows), pareto = do.call(rbind, pareto), static_instances = as.numeric(n_static))
}
