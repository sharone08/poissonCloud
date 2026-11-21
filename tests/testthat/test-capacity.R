test_that("overload probability and capacity", {
  expect_equal(overload_probability(40, 55), 0.009678803, tolerance = 1e-6)
  expect_equal(required_capacity(40, 0.999), 61)
  expect_lte(overload_probability(40, 61), 0.001); expect_gt(overload_probability(40, 60), 0.001)
  expect_gt(overload_probability(40, 55, theta = 20), overload_probability(40, 55))   # burstiness raises risk
  expect_gt(required_capacity(40, 0.999, theta = 20), 61)
  expect_error(required_capacity(40, 1.2))
})
test_that("superposition, thinning, autoscale plan", {
  expect_equal(poisson_superposition(c(12, 18, 10)), 40); expect_equal(poisson_thinning(40, 0.15), 6)
  plan <- autoscale_plan(example_hourly_profile()$lambda, 10)
  expect_gt(autoscale_savings(plan), 0); expect_lt(autoscale_savings(plan), 1)
})
test_that("queueing formulas", {
  expect_equal(erlang_c(40, 10, 5), 0.5541126, tolerance = 1e-6)
  expect_equal(mmc_wait_exceeds(40, 10, 6, 0.05), 0.1047577, tolerance = 1e-6)
  expect_equal(min_servers(40, 10, t = 0.05, target = 0.01), 8)
  m <- mmc_metrics(40, 10, 6); expect_equal(m$L, m$Lq + 4, tolerance = 1e-10)
  expect_error(erlang_c(40, 10, 4))
})
