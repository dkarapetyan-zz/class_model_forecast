# Test whether the ACF yields the acf and pacf that were found when this package
# was initially developed.
a <- 2 : 20
a.log.diff <- log.diff(a)
expect_that(log.diffinv(a.log.diff, xi = a[1]), equals(a))

a.log.diff <- log.diff(a, base = 10)
expect_that(log.diffinv(a.log.diff, xi = a[1], base = 10), equals(a))


