# Author: David Karapetyan This Function outputs a time series PPNR forecast from
# time series input files for bank position data, macroeconomic forecasts , and
# arima model coefficients

#' 
#' @param position_data  A matrix of a particular bank's present balance sheet
#' @param model_coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro_forecasts A matrix of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' @export
#' 
#' 



PPNRForecast <- function(position_data, model_coefficients, macro_forecasts) {
	
	source("src/ppnr_forecast/asset_coefficients_forecast.R")
	source("src/ppnr_forecast/asset_forecast.R")
	
	# testing for inputs done in subroutines
	.revenue_forecast_ts <- RevenueForecast(position_data)
	.revenue_coeffs_forecast_ts <- RevenueCoeffForecast(position_data, model_coefficients, 
			macro_forecasts)
	# create blank ppnr capital forecast time series
	.ppnr_forecast_ts <- ts(matrix(NA, ncol = 7, nrow = 14), start = c(2014, 3), 
			end = c(2017, 4), frequency = 4)
	
	colnames(.ppnr_forecast_ts) <- c("Net.Interest.Income",
	"Non.Int...Non.Trade.Income", "Trading.Income", "Compensation.Exp",
	"Fixed.Asset.Exp", "Other.Exp", "PPNR")
	
	# first column of our forecast is just our initial input data
	.ppnr_forecast_ts[-1, "Net.Interest.Income"] <- (.revenue_forecast_ts[-1, "Interest.Earning.Assets"]/400
				* .revenue_coeffs_forecast_ts[-1, "Net.Interest.Margin"])
	
	.ppnr_forecast_ts[-1, "Non.Int...Non.Trade.Income"] <- (.revenue_forecast_ts[-1, 
						"Total.Assets"]/400
				* .revenue_coeffs_forecast_ts[-1, "Noninterest.Nontrading.Income.Ratio"])
	
	.ppnr_forecast_ts[-1, "Trading.Income"] <- (.revenue_coeffs_forecast_ts[-1,
						"Return.on.Trading.Assets"]/400
				* .revenue_forecast_ts[-1, "Trading.Assets"])
	
	.ppnr_forecast_ts[-1, "Compensation.Exp"] <- (.revenue_forecast_ts[-1, "Total.Assets"]/400
				* .revenue_coeffs_forecast_ts[-1, "Compensation.Noninterest.Expense.Ratio"])
	
	.ppnr_forecast_ts[-1, "Fixed.Asset.Exp"] <- (.revenue_coeffs_forecast_ts[-1, 
						"Fixed.Asset.Noninterest.Expense.Ratio"]/400
				* .revenue_forecast_ts[-1, "Total.Assets"])
	
	.ppnr_forecast_ts[-1, "Other.Exp"] <- (.revenue_coeffs_forecast_ts[-1,
						"Other.Noninterest.Expense.Ratio"]/400 * .revenue_forecast_ts[-1, "Total.Assets"])
	
	.cols1 <- c("Net.Interest.Income", "Non.Int...Non.Trade.Income", "Trading.Income")
	.cols2 <- c("Compensation.Exp", "Fixed.Asset.Exp", "Other.Exp")
	
	.ppnr_forecast_ts[-1, "PPNR"] <- (rowSums(.ppnr_forecast_ts[-1, .cols1])
				- rowSums(.ppnr_forecast_ts[-1, .cols2]))
	
	return(.ppnr_forecast_ts)
}