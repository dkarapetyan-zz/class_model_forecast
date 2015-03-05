# Author: David Karapetyan
###############################################################################
#This Function outputs a time series PPNR forecast from time series input files
#for bank position data, macroeconomic forecasts , and arima model coefficients 

#' 
#' @param position.data  A matrix of a particular bank's present balance sheet
#' @param model.coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro.forecasts A matrix of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' @export
#' 
#' 



PPNRForecast <- function(position.data, model.coefficients, macro.forecasts) {
	
source("c:/ppnr.quant.repo/class_model/src/ppnr_forecast/revenue_coefficients_forecast_david.R")
source("c:/ppnr.quant.repo/class_model/src/ppnr_forecast/revenue_forecast_david.R")

#testing for inputs done in subroutines	
	.revenue.forecast.ts <- RevenueForecast(position.data)	
	.revenue.coeffs.forecast.ts <- RevenueCoeffForecast(position.data, model.coefficients, macro.forecasts)
	#create blank ppnr capital forecast time series
	.ppnr.forecast.ts <- ts(matrix(NA, ncol = 7, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.ppnr.forecast.ts) <- c("Net.Interest.Income", "Non.Int...Non.Trade.Income", "Trading.Income", 
			"Compensation.Exp", "Fixed.Asset.Exp", "Other.Exp", "PPNR")

			#first column of our forecast is just our initial input data
	.ppnr.forecast.ts[-1,"Net.Interest.Income"] <- (
			.revenue.forecast.ts[-1,"Interest.Earning.Assets"]/400
				*.revenue.coeffs.forecast.ts[-1,"Net.Interest.Margin"]
				)
	
	.ppnr.forecast.ts[-1,"Non.Int...Non.Trade.Income"] <- (
				.revenue.forecast.ts[-1,"Total.Assets"]/400
				*.revenue.coeffs.forecast.ts[-1,"Noninterest.Nontrading.Income.Ratio"]
				)
	
	.ppnr.forecast.ts[-1,"Trading.Income"] <- (
			.revenue.coeffs.forecast.ts[-1,"Return.on.Trading.Assets"]/400
				*.revenue.forecast.ts[-1,"Trading.Assets"]
				)
	
	.ppnr.forecast.ts[-1,"Compensation.Exp"] <- (
				.revenue.forecast.ts[-1,"Total.Assets"]/400
				*.revenue.coeffs.forecast.ts[-1,"Compensation.Noninterest.Expense.Ratio"]
				)
	
	.ppnr.forecast.ts[-1,"Fixed.Asset.Exp"] <- (
			.revenue.coeffs.forecast.ts[-1,"Fixed.Asset.Noninterest.Expense.Ratio"]/400
				*.revenue.forecast.ts[-1,"Total.Assets"]
				)
	
	.ppnr.forecast.ts[-1,"Other.Exp"] <- (
				.revenue.coeffs.forecast.ts[-1,"Other.Noninterest.Expense.Ratio"]/400
				*.revenue.forecast.ts[-1,"Total.Assets"]
				)

	.cols1 <- c("Net.Interest.Income", "Non.Int...Non.Trade.Income", "Trading.Income") 
	.cols2 <- c("Compensation.Exp", "Fixed.Asset.Exp", "Other.Exp")
			
	.ppnr.forecast.ts[-1,"PPNR"] <- (
				rowSums(.ppnr.forecast.ts[-1,.cols1]) - rowSums(.ppnr.forecast.ts[-1, .cols2])
				)

	return (.ppnr.forecast.ts)
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/ppnr_forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_ppnr.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#PPNRForecast(.revenue.forecast, model.coefficients.ppnr, macro.forecasts)