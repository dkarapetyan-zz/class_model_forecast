set.seed(1)
non.stationary.random.walk <- diffinv(rnorm(100))

# stationarity.test should warn that the time series is not a time series.
expect_that(test <- stationarity.test(non.stationary.random.walk), 
           gives_warning())

# Verify whether stationarity.test returned an object of type 
# "StationarityTest".
expect_that(test, is_a("StationarityTest"))

