#' Gives the inverse difference between the log of each element.
#' 
#' This function restores a series from which is taken the difference with log.diff.
#'
#' @param series The series that should be used as input.
#' @param xi The start of the series.
#' @param base The base of the log that should be used.
#' @return the resulting series.
#' @examples
#'  a <- 2 : 20
#'  a.log.diff <- log.diff(a)
#'  log.diffinv(a.log.diff, xi = a[1])

log.diffinv <- function(series, xi, base = exp(1)) {
  c(xi, xi * base ^ cumsum(series))
}
