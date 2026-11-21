# Reproduces the numbers of the deck with the R package.
# Usage (from the repo root):  Rscript analysis/run_analysis.R
# Compare the printed values with analysis/results.json (produced by the Python reference).
devtools::load_all(".")
d <- load_traffic()
train <- which(d$day <= 7); valid <- which(d$day >= 8 & d$day <= 10); test <- which(d$day >= 11)

m <- fit_traffic_model(d$requests[train], d$timestamp[train], family = "nb")
print(m)                                              # theta ~ 43.26, Pearson phi ~ 12.5
mp <- as_poisson(m)
te <- test[d$is_incident[test] == 0]
cov <- function(p) c(
  homogeneous_poisson = mean(d$requests[te] <= qpois(p, mean(d$requests[train]))),
  nhpp_poisson        = mean(d$requests[te] <= traffic_quantile(mp, d$timestamp[te], p)),
  nhpp_nb             = mean(d$requests[te] <= traffic_quantile(m,  d$timestamp[te], p)))
print(rbind(`0.99` = cov(0.99), `0.999` = cov(0.999)))   # expected: 0.62/0.77/0.987 and 0.64/0.83/0.998

pol <- compare_policies(d$requests, d$timestamp, m, train, valid, test)
print(pol$test)                                       # static 86.4$, hybrid ~53.6$ with ~0.06% violations
dir.create("analysis/r_results", showWarnings = FALSE)
write.csv(pol$test, "analysis/r_results/policies_test.csv", row.names = FALSE)
write.csv(pol$pareto, "analysis/r_results/pareto.csv", row.names = FALSE)

mu_t <- predict(m, d$timestamp[test])
for (mod in c("poisson", "nb")) {
  a <- detect_anomalies(d$requests[test], mu_t, if (mod == "nb") m$theta else Inf)
  cat(mod, ": alarm bins =", sum(a$alarm), "| labelled incident bins =", sum(d$is_incident[test]), "\n")
}
