# Author: David Karapetyan This Function outputs a graph of an input forecast 

#' @param position_data A data frame of a particular 
#'  bank's present balance sheet
#' @param model_coefficients A data frame of arima calibration
#' coefficients computed from past history
#' @param macro_forecasts A time series of either basic, adverse, or severely
#' adverse macroeconomic forecasts
#' @param book Portion of trading book from which variables are obtained
#' @param variable Variable that will be plotted, with respect to time
#' @returnType  
#' @return Object of class ggplot
#' @author David Karapetyan
#' @export
#' @example
#' result_time_series <- GraphForecast(
#' position_data = object_data_frame,
#' model_coefficients = object_data_frame,
#' macro_forecasts = object_time_series,
#' book = "lll", variable = "Provision")

GraphForecast <- function(position_data, model_coefficients, macro_forecasts,
		book, variable) {
	if (!(book %in% c("afs", "ppnr", "lll", "capital", "loss"))) {
		stop("Error: Please input a book value of
						either afs, ppnr, lll, or capital")
	}
	
# if (!(variable %in% colnames(book))) {
#   {stop(paste("Error: Please input a
# variable name of one of the following:", colnames(book) ))}
#   }
	
	library(ggplot2)
	library("zoo")
	library("Hmisc")
	load("data/model_coefficients.RData")
	load("data/macro_forecasts.RData")
	load("data/position_data.RData")
  if (file.exists(paste("src/", as.name(book), "_forecast/",
					as.name(book), "_forecast.R", sep=""))) {
 	source(paste("src/", as.name(book), "_forecast/",
					as.name(book), "_forecast.R", sep=""))
  }
#  if (file.exists(paste("src/", as.name(book), "_forecast/",
#					as.name(book), "_forecast.R", sep=""))) {
# 	source(paste("src/", as.name(book), "_forecast/",
#					as.name(book), "_forecast.R", sep=""))
#  }
  
	source(paste("src/", as.name(book), "_forecast/",
					as.name(book), "_forecast.R", sep=""))
	function_to_call <- as.name(paste(
					as.name(capitalize((book))), "Forecast", sep=""))
	book_zoo <- as.zoo(eval(function_to_call)(
					position_data, 
					model_coefficients, 
					macro_forecasts))
	book_fortified <- fortify(book_zoo)
	
	#get rid of all rows with NAs, so ggplot doesn't give warning
	book_fortified <- na.omit(book_fortified)  
	
	#otherwise, aes doesn't source environment properly
	.environment <- environment() 
	p <- ggplot(
			data = book_fortified,
			aes(book_fortified$Index,
					book_fortified[[variable]]),
			environment = .environment) #error with $ sign instead of [[ ]]
	p <- p + ggtitle(paste(capitalize(book), "Forecast"))
	p <- p + xlab("Time")
	p <- p + ylab(variable)
	p <- p + geom_line()
	print(p)
}