#' Prints the results of the stationarity tests.
#' 
#' This function prints the outcome of the various stationarity tests. 
#'
#' @param object the StationarityTest result.
#' @param format format the table in a format given by kable.
#' @param return.statistics return a data.frame that contains all the
#' statistics.
#' @param ... additional parameters passed on to kable.
#' @return return a data.frame with test name, series name, test statistic and
#' p-value for each test performed.
#' @export
#' @examples
#'  data(fred.totalunemployment)
#'  result <- stationarity.test(fred.totalunemployment)
#'  print(result)
#' @seealso knitr::kable
print.StationarityTest <- function(object, format = NULL, 
                                    return.statistics = FALSE, ...)
{
  if (class(object) != "StationarityTest") {
    stop("object is expected to be of class StationarityTest.")
  }
  
  result <- data.frame("Test name" = c("Box-Ljung test",
                                       "Augmented Dickey-Fuller Test",
                                       "KPSS Test for Level Stationarity"),
                       "Series" = rep(object$series.name, 3),
                       "Test statistic" = c(object$box.result$statistic,
                                            object$adf.result$statistic,
                                            object$kpss.result$statistic),
                       "p-value" = c(object$box.result$p.value,
                                     object$adf.result$p.value,
                                     object$kpss.result$p.value)
  )
  
  if (is.null(format)) {
    print(object$box.result)
    print(object$adf.result)
    print(object$kpss.result)
  } else {
    kable(result, format = format)
  }
}