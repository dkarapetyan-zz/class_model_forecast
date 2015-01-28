#' Standard tests to verify whether a time series is stationary. 
#' 
#' This function performs some standard tests to verify whether a time
#' series is stationary.
#' @param ts  a time series that should be tested.
#' @keywords testsuite
#' @export
#' @examples
#'  data(fred.totalunemployment)
#'  stationarity.test(fred.totalunemployment)

stationarity.test <- function(time.series)
{
  if (class(time.series) != "ts") {
    warning("time.series argument was not a time series.")
  }
  
  # store the old par settings so that we can restore them at the end of the function.
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mfrow = c(2,1))
  
  # Load the packages so the tests can be generated.
  require(forecast)
  require(tseries)
  Acf(time.series)
  Pacf(time.series)
  
  results <- structure(list(), class = "StationarityTest")
  attr(results, "box.call") <- 'Box.test(time.series, type = ("Ljung-Box"))'
  attr(results, "box.result") <- eval(parse(file = "", 
                                       text = attr(results, "box.call")))
  attr(results, "adf.call") <- 'adf.test(time.series, alternative = "stationary")'
  attr(results, "adf.result") <-  eval(parse(file = "", 
                                        text = attr(results, "adf.call")))
  attr(results, "kpss.call") <- 'kpss.test(time.series)'
  attr(results, "kpss.result") <- eval(parse(file = "", 
                                        text = attr(results, "kpss.call")))
  
  return(results)
}