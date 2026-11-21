#' Maximum-likelihood fit of a (homogeneous) Poisson rate
#'
#' The MLE of lambda is the sample mean; the interval is the exact Garwood interval.
#' @param x At least 2 non-negative integer counts.
#' @param conf_level Confidence level (default 0.95).
#' @return An object of class `poisson_fit`.
#' @examples
#' poisson_fit(c(38, 41, 40, 43, 39, 37))
#' @export
poisson_fit <- function(x, conf_level = 0.95) {
  x <- .check_counts(x); .check_prob(conf_level, "conf_level")
  n <- length(x); s <- sum(x); lam <- s / n; alpha <- 1 - conf_level
  lower <- if (s == 0) 0 else stats::qgamma(alpha / 2, shape = s) / n
  upper <- stats::qgamma(1 - alpha / 2, shape = s + 1) / n
  loglik <- if (lam > 0) sum(poisson_pmf(x, lam, log = TRUE)) else 0
  structure(list(lambda = lam, ci = c(lower = lower, upper = upper), conf_level = conf_level,
                 n = n, loglik = loglik, aic = 2 - 2 * loglik, x = x), class = "poisson_fit")
}

#' @export
print.poisson_fit <- function(x, ...) {
  cat("Poisson fit (maximum likelihood)\n")
  cat(sprintf("  n = %d | lambda = %.4f\n", x$n, x$lambda))
  cat(sprintf("  %.0f%% exact CI: [%.4f ; %.4f]\n", 100 * x$conf_level, x$ci[["lower"]], x$ci[["upper"]]))
  cat(sprintf("  log-likelihood = %.2f | AIC = %.2f\n", x$loglik, x$aic))
  invisible(x)
}

#' Over-dispersion test for Poisson counts
#'
#' Under H0 (variance = mean), \eqn{D = \sum (x_i-\bar x)^2/\bar x \sim \chi^2_{n-1}}. Bursty
#' traffic gives a huge D: Poisson then under-estimates the tail risk.
#' @param x Counts (length >= 2).
#' @param alternative `"greater"` (over-dispersion), `"less"` or `"two.sided"`.
#' @return An object of class `htest`.
#' @examples
#' poisson_dispersion_test(c(10, 70, 12, 66, 9, 71))
#' @export
poisson_dispersion_test <- function(x, alternative = c("greater", "less", "two.sided")) {
  alternative <- match.arg(alternative); dname <- deparse(substitute(x)); x <- .check_counts(x)
  n <- length(x); m <- mean(x)
  if (m == 0) stop("The mean is zero: the test is undefined.", call. = FALSE)
  D <- sum((x - m)^2) / m; df <- n - 1
  p <- switch(alternative, greater = stats::pchisq(D, df, lower.tail = FALSE), less = stats::pchisq(D, df),
              two.sided = 2 * min(stats::pchisq(D, df), stats::pchisq(D, df, lower.tail = FALSE)))
  structure(list(statistic = c(D = D), parameter = c(df = df), p.value = min(1, p),
                 estimate = c(mean = m, variance = stats::var(x)), alternative = alternative,
                 method = "Poisson dispersion test (variance = mean)", data.name = dname), class = "htest")
}

#' Chi-squared goodness-of-fit test against a Poisson distribution
#'
#' Classes are merged from both ends until each expected count is at least `min_expected`;
#' degrees of freedom = classes - 2 because lambda is estimated.
#' @param x Counts (length >= 2).
#' @param min_expected Minimum expected count per class (default 5).
#' @return An object of class `htest`.
#' @examples
#' set.seed(1); poisson_gof_test(rpois(500, 4))
#' @export
poisson_gof_test <- function(x, min_expected = 5) {
  dname <- deparse(substitute(x)); x <- .check_counts(x)
  n <- length(x); lam <- mean(x)
  if (lam == 0) stop("The mean is zero: the test is undefined.", call. = FALSE)
  kmax <- max(x); obs <- tabulate(x + 1L, nbins = kmax + 1L)
  p <- poisson_pmf(0:kmax, lam); p[length(p)] <- 1 - sum(p[-length(p)])
  expd <- n * p
  while (length(expd) > 2 && expd[length(expd)] < min_expected) {
    L <- length(expd); obs[L - 1] <- obs[L - 1] + obs[L]; obs <- obs[-L]
    expd[L - 1] <- expd[L - 1] + expd[L]; expd <- expd[-L]
  }
  while (length(expd) > 2 && expd[1] < min_expected) {
    obs[2] <- obs[2] + obs[1]; obs <- obs[-1]; expd[2] <- expd[2] + expd[1]; expd <- expd[-1]
  }
  df <- length(expd) - 2
  if (df < 1) stop("Not enough classes after merging to run the test.", call. = FALSE)
  stat <- sum((obs - expd)^2 / expd)
  structure(list(statistic = c(`X-squared` = stat), parameter = c(df = df),
                 p.value = stats::pchisq(stat, df, lower.tail = FALSE), estimate = c(lambda = lam),
                 method = "Chi-squared goodness-of-fit test (Poisson)", data.name = dname,
                 observed = obs, expected = expd), class = "htest")
}
