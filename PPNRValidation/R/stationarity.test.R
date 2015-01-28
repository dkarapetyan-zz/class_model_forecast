#' Standard tests to verify whether a time series is stationary. 
#' 
#' This function plots the ACF and PACF plots for a time series and performs 
#' the Ljung-Box, Augmented Dickey-Fuller (ADF) and 
#' Kwiatkowski-Phillips-Schmidt-Shin (KPSS) tests. 
#'
#' @param time.series a time series that should be tested.
#' @return object of class StationarityTest
#' @export
#' @examples
#'  data(fred.totalunemployment)
#'  result <- stationarity.test(fred.totalunemployment)

stationarity.test <- function(time.series)
{
  if (class(time.series) != "ts") {
    warning("time.series argument was not a time series.")
  }
  
  # store the old par settings so that we can restore them at the end of the
  # function.
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mfrow = c(2,1))
  
  time.series.name <- deparse(substitute(time.series))
  
  # Load the packages so the tests can be generated.
  Acf(time.series, main = paste(time.series.name))
  Pacf(time.series, main = paste(time.series.name))
  
  
  box.call <- 
	  'Box.test(time.series, type = ("Ljung-Box"), lag = 20)'
  box.result <- eval(parse(file = "", text = box.call))
  box.result$data.name <- time.series.name
  
  adf.call <- 
	  'adf.test(time.series, alternative = "stationary")'
  adf.result <-  eval(parse(file = "", text = adf.call))
  adf.result$data.name <- time.series.name
  
  kpss.call <- 
	  'kpss.test(time.series)'
  kpss.result <- eval(parse(file = "", text = kpss.call))
  kpss.result$data.name <- time.series.name
  
  structure(list(series.name = time.series.name,
                 box.call = box.call, box.result = box.result,
                 adf.call = adf.call, adf.result = adf.result,
                 kpss.call = kpss.call, kpss.result = kpss.result), 
            class = "StationarityTest")
}
