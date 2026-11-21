test_that("poisson_rng has the right mean and variance", {
  set.seed(1)
  x <- poisson_rng(5000, 10)
  expect_equal(mean(x), 10, tolerance = 0.05); expect_equal(var(x), 10, tolerance = 0.15)
  expect_equal(mean(poisson_rng(3000, 100)), 100, tolerance = 0.02)
})
test_that("arrivals are consistent", {
  arr <- simulate_arrivals(40, 200, seed = 7)
  expect_true(all(diff(arr) > 0))
  counts <- count_arrivals(arr, 1, 200)
  expect_equal(sum(counts), length(arr)); expect_equal(mean(counts), 40, tolerance = 0.05)
})
test_that("mc_overload brackets the exact value", {
  r <- mc_overload(40, 55, n_sim = 2e5, seed = 123)
  expect_lt(abs(r$estimate - r$exact), 0.002); expect_equal(r$exact, 0.009678803, tolerance = 1e-6)
})
test_that("Hawkes simulation and fit recover burstiness", {
  ev <- simulate_hawkes(5, 0.6, 1, 300, seed = 1)
  expect_equal(length(ev), 5 * 300 / (1 - 0.6), tolerance = 0.25)
  fit <- fit_hawkes(ev, 300)
  expect_equal(fit$branching_ratio, 0.6, tolerance = 0.25)
  expect_error(simulate_hawkes(1, 2, 1, 10))
})
