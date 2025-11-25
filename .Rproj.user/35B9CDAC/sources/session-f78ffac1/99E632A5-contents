test_that("expected_lambda is finite", {
  bern <- CityWeather("Bern", temperature = 20, humidity = 60, pressure = 1013)
  lambda <- expected_lambda(bern)
  expect_true(is.finite(lambda))
  expect_gt(lambda, 0)
})
