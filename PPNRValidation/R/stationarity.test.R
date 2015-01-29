#' Standard tests to verify whether a time series is stationary. 
#' 
#' This function plots the ACF and PACF plots for a time series and performs 
#' the Ljung-Box, Augmented Dickey-Fuller (ADF) and 
#' Kwiatkowski-Phillips-Schmidt-Shin (KPSS) tests. If advanced tests are
#' selected stationaritya.test will also perform the Priestley-Subba Rao test 
#' \url{http://www.maths.bris.ac.uk/~guy/Research/LSTS/TOS.html}.
#'
#' @param time.series Time series that should be tested.
#' @param max.lag Maximum lag.
#' @param advanced Determine whether advanced tests should be performed.
#' @return object of class StationarityTest
#' @export
#' @examples
#'  data(fred.totalunemployment)
#'  result <- stationarity.test(fred.totalunemployment)

stationarity.test <- function(time.series, max.lag = NULL, advanced = FALSE)
{
  if (class(time.series) != "ts") {
    warning("time.series argument was not a time series.")
  }
  
  time.series.name <- deparse(substitute(time.series))

  
  # Load the packages so the tests can be generated.
  acf2.call <- paste("acf2.result <- as.data.frame(acf2(",
                     time.series.name,", max.lag = max.lag))")
  eval(parse(file = "", text = acf2.call))
  
  # Take the same lags as acf2. 
  max.lag <- nrow(acf2.result)

  box.call <- 
	  'Box.test(time.series, type = ("Ljung-Box"), lag = max.lag)'
  box.result <- eval(parse(file = "", text = box.call))
  box.result$data.name <- time.series.name
  
  adf.call <- 
	  'adf.test(time.series, alternative = "stationary", k = max.lag)'
  adf.result <-  eval(parse(file = "", text = adf.call))
  adf.result$data.name <- time.series.name
  
  kpss.call <- 
	  'kpss.test(time.series)'
  kpss.result <- eval(parse(file = "", text = kpss.call))
  kpss.result$data.name <- time.series.name

  stationarity.result <- NULL

  if (advanced) {
    stationarity.call <-
      'stationarity(as.numeric(time.series))'
    stationarity.result <- eval(parse(file = "", text = stationarity.call))
    attr(stationarity.result, "series.name") <- time.series.name
  }
  
  structure(list(series.name = time.series.name,
                 box.call = box.call, box.result = box.result,
                 adf.call = adf.call, adf.result = adf.result,
                 kpss.call = kpss.call, kpss.result = kpss.result, 
                 stationarity.result = stationarity.result,
                 acf = acf2.result), 
            class = "StationarityTest")
}
