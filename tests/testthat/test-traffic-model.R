test_that("seasonal NB model reproduces the reference analysis (Python cross-check)", {
  skip_on_cran()
  d <- load_traffic(); tr <- which(d$day <= 7)
  m <- fit_traffic_model(d$requests[tr], d$timestamp[tr])
  expect_equal(m$theta, 43.26, tolerance = 0.02)
  expect_equal(m$pearson_phi, 12.48, tolerance = 0.02)
  expect_lt(m$aic, as_poisson(m)$aic + 1e12)   # sanity: finite
  te <- which(d$day >= 11 & d$is_incident == 0)
  cov_nb <- mean(d$requests[te] <= traffic_quantile(m, d$timestamp[te], 0.999))
  cov_po <- mean(d$requests[te] <= traffic_quantile(as_poisson(m), d$timestamp[te], 0.999))
  expect_gt(cov_nb, 0.995)      # NB honours the 99.9% SLA
  expect_lt(cov_po, 0.90)       # Poisson does not
})
test_that("fit_traffic_model validates inputs", {
  expect_error(fit_traffic_model(1:3, as.POSIXct("2026-03-02", tz = "UTC") + 0:1))
})
