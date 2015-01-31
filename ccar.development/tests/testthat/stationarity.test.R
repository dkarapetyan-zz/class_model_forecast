set.seed(1)
non.stationary.random.walk <- diffinv(rnorm(100))

# stationarity.test should warn that the time series is not a time series.
expect_that(test <- stationarity.test(non.stationary.random.walk), 
           gives_warning())

# Verify whether stationarity.test returned an object of type 
# "StationarityTest".
expect_that(test, is_a("StationarityTest"))

# Test whether the ACF yields the acf and pacf that were found when this package
# was initially developed.
acf.for.non.stationary.random.walk.seed.one <-
  structure(list(ACF = c(0.95, 0.9, 0.84, 0.78, 0.73, 0.69, 0.65,  0.62, 0.59,
                         0.56, 0.53, 0.51, 0.49, 0.46, 0.43, 0.41, 0.39, 0.37,
                         0.36, 0.35, 0.35), 
                 PACF = c(0.95, -0.08, -0.06, -0.03, 0.04,  0.04, 0.05, -0.04,
                          0.02, 0.03, -0.07, 0.08, 0.04, -0.09, -0.05,  0.07,
                          0.09, -0.12, 0.07, 0.06, 0.08)),
          .Names = c("ACF", "PACF" ), row.names = c(NA, -21L), 
          class = "data.frame")
expect_that(ACF(test), equals(acf.for.non.stationary.random.walk.seed.one))
