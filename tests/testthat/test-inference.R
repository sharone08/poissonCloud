test_that("poisson_fit returns the mean and the Garwood interval", {
  x <- c(2, 3, 4, 3, 3); fit <- poisson_fit(x)
  expect_equal(fit$lambda, 3)
  expect_equal(unname(fit$ci), as.numeric(stats::poisson.test(sum(x), T = length(x))$conf.int), tolerance = 1e-8)
  expect_output(print(fit), "lambda")
})
test_that("dispersion test detects over-dispersion", {
  expect_gt(poisson_dispersion_test(rep(40, 50))$p.value, 0.99)
  expect_lt(poisson_dispersion_test(c(rep(10, 25), rep(70, 25)))$p.value, 1e-10)
})
test_that("chi-squared test accepts Poisson data", {
  set.seed(3); expect_gt(poisson_gof_test(rpois(2000, 6))$p.value, 0.001)
})
