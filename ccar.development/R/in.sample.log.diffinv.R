#' Generate the in sample inverse of a log diff based on another time series.
#' 
#' This function restores a series from which is taken the difference with log.diff.
#'
#' @param series The series that should be used as input.
#' @param xi.series The series that should be used as in sample forecast.
#' @param base The base of the log that should be used.
#' @return the resulting series. If xi.series is a time series the returned
#' object will have the same properties as that time series.
#' @examples
#'  a <- 2 : 20
#'  a.log.diff <- log.diff(a)
#'  in.sample.log.diffinv(a.log.diff, xi = a)
in.sample.log.diffinv <- function(series, xi.series, base = exp(1)) {
  if (length(series) != (length(xi.series) - 1)) {
    stop("series should be one element smaller than xi.series.")
  }

  .series <- cbind(series, xi.series[2 : length(xi.series)])

  .results <- c(xi.series[1], apply(.series, 1, function(x) {
                      (x[2] * base ^ x[1]) 
                    }))

  if (class(xi.series) == "ts") {
    .results <- ts(.results, start = start(xi.series), 
                   frequency = frequency(xi.series))
  }

  return(.results)
}

