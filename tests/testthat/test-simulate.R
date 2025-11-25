test_that("simulate_parties returns a data frame with correct columns", {
  glass <- GlassProfile(
    a = 12.6, b = 22.5,
    x_1 = 1.1, x_2 = 12.6, x_3 = 18.8, x_4 = 22.5,
    r_foot = 5.8, r_stem = 0.25, r_bowl = 7.5, r_rim = 7
  )

  city <- CityWeather("TestCity", temperature = 20, humidity = 60, pressure = 1013)

  res <- simulate_parties(city, glass, N = 5, seed = 1)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("guests", "glasses", "total_L", "bottles") %in% colnames(res)))
})
