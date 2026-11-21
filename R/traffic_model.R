# Seasonal count model: log mu_t = Fourier(hour of day) + weekend effect
.time_features <- function(time) {
  lt <- as.POSIXlt(time, tz = "UTC")
  list(hour = lt$hour + lt$min / 60 + lt$sec / 3600, weekend = as.numeric(lt$wday %in% c(0L, 6L)))
}
.design <- function(hour, weekend, harmonics) {
  X <- matrix(1, nrow = length(hour), ncol = 1)
  for (k in seq_len(harmonics)) X <- cbind(X, sin(2 * pi * k * hour / 24), cos(2 * pi * k * hour / 24))
  cbind(X, weekend)
}

#' Fit a seasonal traffic model (non-homogeneous Poisson or negative binomial)
#'
#' Step 1 fits a Poisson GLM (log link) with Fourier terms for the time of day and a weekend
#' effect: this is a **non-homogeneous Poisson process** with a time-varying rate.
#' Step 2 (`family = "nb"`) estimates the NB2 size `theta` by maximum likelihood given those
#' means, so that Var(Y) = mu + mu^2/theta. Burstiness then shows up as a small `1/theta`.
#'
#' @param counts Requests per time bin (non-negative integers).
#' @param time `POSIXct` timestamps (UTC) of the bins.
#' @param family `"nb"` (default) or `"poisson"`.
#' @param harmonics Number of daily Fourier harmonics (default 6).
#' @return An object of class `traffic_model`.
#' @examples
#' \donttest{
#' d <- load_traffic()
#' m <- fit_traffic_model(d$requests[1:60480], d$timestamp[1:60480])
#' m
#' }
#' @export
fit_traffic_model <- function(counts, time, family = c("nb", "poisson"), harmonics = 6) {
  family <- match.arg(family)
  counts <- as.numeric(counts)
  if (length(counts) != length(time)) stop("`counts` and `time` must have the same length.", call. = FALSE)
  if (anyNA(counts) || any(counts < 0)) stop("`counts` must be non-negative with no NA.", call. = FALSE)
  f <- .time_features(time)
  X <- .design(f$hour, f$weekend, harmonics)
  fit <- stats::glm.fit(X, counts, family = stats::poisson())
  mu <- as.numeric(fit$fitted.values)
  theta <- Inf; loglik <- sum(stats::dpois(counts, mu, log = TRUE)); k <- ncol(X)
  if (family == "nb") {
    nll <- function(lt) -sum(stats::dnbinom(counts, size = exp(lt), mu = mu, log = TRUE))
    theta <- exp(stats::optimize(nll, interval = c(0, 12))$minimum)
    loglik <- -nll(log(theta)); k <- k + 1
  }
  structure(list(coef = as.numeric(fit$coefficients), harmonics = harmonics, family = family,
                 theta = theta, loglik = loglik, aic = 2 * k - 2 * loglik, n = length(counts),
                 pearson_phi = sum((counts - mu)^2 / mu) / (length(counts) - ncol(X))),
            class = "traffic_model")
}

#' Switch a fitted model to the Poisson family (same means, no extra dispersion)
#' @param model A `traffic_model`.
#' @return A `traffic_model` with `theta = Inf`.
#' @export
as_poisson <- function(model) { model$family <- "poisson"; model$theta <- Inf; model }

#' @export
print.traffic_model <- function(x, ...) {
  cat("Seasonal traffic model (", x$family, ")\n", sep = "")
  cat(sprintf("  bins: %d | harmonics: %d | AIC: %.1f\n", x$n, x$harmonics, x$aic))
  cat(sprintf("  Pearson dispersion after seasonality: %.2f (Poisson would give 1)\n", x$pearson_phi))
  if (is.finite(x$theta)) cat(sprintf("  NB size theta: %.2f (variance = mean + mean^2/%.1f)\n", x$theta, x$theta))
  invisible(x)
}

#' Expected counts per bin
#' @param object A `traffic_model`.
#' @param time `POSIXct` timestamps.
#' @param ... Unused.
#' @return Numeric vector of expected counts per bin.
#' @export
predict.traffic_model <- function(object, time, ...) {
  f <- .time_features(time)
  as.numeric(exp(.design(f$hour, f$weekend, object$harmonics) %*% object$coef))
}

#' Forecast quantile of the count per bin
#' @param model A `traffic_model`.
#' @param time `POSIXct` timestamps.
#' @param p Probability (default 0.999).
#' @return Numeric vector: smallest count c with P(Y <= c) >= p.
#' @export
traffic_quantile <- function(model, time, p = 0.999) {
  .check_prob(p); .q_count(p, predict(model, time), model$theta)
}

#' Per-tail p-value of observed counts under a traffic model
#' @param model A `traffic_model`.
#' @param counts Observed counts.
#' @param time `POSIXct` timestamps.
#' @return min(P(Y <= y), P(Y >= y)) for each bin.
#' @export
traffic_pvalue <- function(model, counts, time) {
  mu <- predict(model, time)
  pmin(.cdf_count(counts, mu, model$theta), .cdf_count(counts - 1, mu, model$theta, lower.tail = FALSE))
}
