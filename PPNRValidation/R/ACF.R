#' Gives the ACF and PACF that were computed as part of the stationarity tests.
#'
#' @param object The stationarity test.
#' @return A data.frame that contains the ACF and PACF at different lags.
#' @examples
#'  data(fred.totalunemployment)
#'  result <- stationarity.test(fred.totalunemployment)
#'  ACF(result)

ACF <- function(object) {
  if (class(object) != "StationarityTest") {
    stop("object is expected to be of class StationarityTest.")
  }

 return(object$acf)
}
