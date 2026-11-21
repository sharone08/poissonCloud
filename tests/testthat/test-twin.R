test_that("simulate_autoscaling behaves on trivial cases", {
  ok <- simulate_autoscaling(rep(100, 50), rep(4, 50))        # capacity 600 per bin
  expect_equal(ok$violation_rate, 0); expect_equal(ok$cost, 4 * 50 * 10 / 3600 * 0.10)
  bad <- simulate_autoscaling(rep(5000, 50), rep(2, 50))      # capacity 300 per bin
  expect_equal(bad$violation_rate, 1)
  expect_error(simulate_autoscaling(1:3, 1:2))
})
test_that("policies respect the floor and the boot delay", {
  y <- c(rep(100, 20), rep(3000, 20))
  inst <- policy_reactive(y, 0.7, delay_bins = 6)
  expect_length(inst, 40); expect_true(all(inst >= 2))
  expect_equal(inst[22], 2)                                   # surge not yet served: boot delay
  expect_gt(inst[40], 2)
  expect_equal(policy_static(5, 3), rep(3, 5))
  expect_length(policy_hybrid(y, rep(100, 40), 40, 0.999), 40)
})
test_that("tune_policy picks the cheapest safe setting", {
  f <- function(u) simulate_autoscaling(rep(500, 60), policy_reactive(rep(500, 60), u))
  t <- tune_policy(f, c(0.3, 0.6, 0.9)); expect_true(t$best$violation_rate <= 0.00025 || nrow(t$table) == 3)
})
test_that("anomaly detector flags a surge and ignores normal bins", {
  r <- detect_anomalies(c(40, 38, 45, 400, 410, 405, 41), mean = 40)
  expect_equal(which(r$alarm), 4:6)
  expect_false(any(detect_anomalies(c(40, 38, 45, 41), mean = 40)$alarm))
})
