#' Gives the difference between the log of each element.
#' 
#' This function generates a series \eqn{y_i = \log(x_i) - log(x_{i + 1})}
#'
#' @param series The series that should be used as input.
#' @param base The base of the log that should be used.
#' @return the resulting series.
#' @examples
#'  a <- 2 : 20
#'  log.diff(a)
log.diff <- function(series, base = exp(1))
{
  diff(log(series, base = base))
}
