# Author: David Karapetyan This Function outputs a graph of an input forecast 

#' @param position_data  A data frame of a particular bank's present balance sheet
#' @param model_coefficients A data frame of arima calibration coefficients computed from past history
#' @param macro_forecasts A time series of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @param book Portion of trading book from which variables are obtained
#' @param variable Variable that will be plotted, with respect to time
#' @returnType  
#' @return void
#' @author David Karapetyan
#' @export

GraphForecast <- function(position_data, model_coefficients, macro_forecasts, book, variable) {
  if (!(book %in% c("afs", "ppnr", "lll", "capital")))
  {stop("Error: Please input a book value of either afs, ppnr, lll, or capital")
  }
  
  
  library(ggplot2)
  library("zoo")
  load("data/model_coefficients.RData")
  load("data/macro_forecasts.RData")
  load("data/position_data.RData")
  source(paste("src/", as.name(book), "_forecast/", as.name(book), "_forecast.R", sep=""))
  function_to_call <- as.name(paste(toupper(as.name(book)), "Forecast", sep=""))
  book_zoo <- as.zoo(eval(function_to_call)(position_data, model_coefficients, macro_forecasts))
  book_fortified <- fortify(book_zoo)
  
  book_fortified <- na.omit(book_fortified)  #get rid of all rows with NAs, so ggplot doesn't give warning
  
  .environment <- environment() #otherwise, aes doesn't source environment properly
  p <- ggplot(data = book_fortified,
      aes(book_fortified$Index,
          book_fortified[[variable]]),
      environment = .environment) #error with $ sign instead of [[ ]]
  p <- p + ggtitle(paste(toupper(book), "Forecast"))
  p <- p + xlab("Time")
  p <- p + ylab(variable)
  p <- p + geom_line()
  print(p)
}