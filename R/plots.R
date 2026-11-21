#' Bar plot of the count distribution
#'
#' Poisson (green) versus negative binomial overlay (line) and the overload region in red.
#' @param lambda Mean count per window.
#' @param capacity Optional capacity; counts above it are drawn in red.
#' @param theta NB size; if finite, its pmf is overlaid as a line.
#' @param main Optional title.
#' @param ... Passed to [graphics::barplot()].
#' @return Invisibly, a data frame (k, p).
#' @examples
#' plot_pmf(40, capacity = 55, theta = 20)
#' @export
plot_pmf <- function(lambda, capacity = NULL, theta = Inf, main = NULL, ...) {
  .check_lambda(lambda)
  kmax <- max(poisson_quantile(1 - 1e-7, lambda), if (is.finite(theta)) stats::qnbinom(1 - 1e-4, size = theta, mu = lambda) else 0)
  k <- 0:kmax; p <- poisson_pmf(k, lambda)
  cols <- rep("#2A9D8F", length(k)); if (!is.null(capacity)) cols[k > capacity] <- "#E63946"
  if (is.null(main)) main <- sprintf("Count distribution (mean = %g)", lambda)
  bp <- graphics::barplot(p, names.arg = k, col = cols, border = NA, xlab = "Requests per window (k)",
                          ylab = "P(X = k)", main = main, ...)
  if (is.finite(theta)) graphics::lines(bp, stats::dnbinom(k, size = theta, mu = lambda), lwd = 2, col = "#0B1D3A")
  invisible(data.frame(k = k, p = p))
}

#' @export
plot.poisson_fit <- function(x, ...) {
  kmax <- max(x$x); k <- 0:kmax
  obs <- tabulate(x$x + 1L, nbins = kmax + 1L) / x$n; expd <- poisson_pmf(k, x$lambda)
  bp <- graphics::barplot(obs, names.arg = k, col = "#BFD7EA", border = NA, ylim = c(0, max(obs, expd) * 1.15),
                          xlab = "k", ylab = "Frequency", main = "Observed vs fitted Poisson", ...)
  graphics::lines(bp, expd, type = "b", pch = 19, lwd = 2, col = "#E63946")
  invisible(x)
}

#' Plot an hourly sizing plan
#' @param plan Output of [autoscale_plan()].
#' @param ... Passed to [graphics::plot()].
#' @return Invisibly, `plan`.
#' @export
plot_autoscale <- function(plan, ...) {
  graphics::plot(plan$slot - 1, plan$static_instances, type = "l", lwd = 2, col = "#E63946",
                 ylim = c(0, max(plan$static_instances) * 1.1), xlab = "Hour", ylab = "Instances",
                 main = "Elastic vs static sizing", ...)
  graphics::lines(plan$slot - 1, plan$instances, type = "s", lwd = 3, col = "#2A9D8F")
  invisible(plan)
}
