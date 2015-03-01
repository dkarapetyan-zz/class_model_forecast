# Author: David Karapetyan
###############################################################################
#This Function outputs a time series ppnr forecast (15 columns) to 2017 Q4 for Leases, Credit Cards,
#and other variables, from a series input files of 2014 Q3 input data for the same variables.
#The function is fed, as default arguments, arima model coefficients, and macroeconomic forecasts
#for 2014Q3.


PPNRForecast <- function(assets.forecast, assets.coefficients.forecast) {
	.required_colnames_assets <- c("Interest.Earning.Assets", "Trading.Assets", "Total.Assets")		
	
	
	if (!all(.required_colnames_assets %in% colnames(assets.forecast))) {
		stop("Not all required colnames were found in assets.forecast")
	}
	
	.required_colnames_coeffs <- c( "Net.Interest.Margin", "Noninterest.Nontrading.Income.Ratio",
	"Compensation.Noninterest.Expense.Ratio", "Fixed.Asset.Noninterest.Expense.Ratio",
	"Other.Noninterest.Expense.Ratio", "Return.on.Trading.Assets")
	
	
	if (!all(.required_colnames_coeffs %in% colnames(assets.coefficients.forecast))) {
		stop("Not all required colnames were found in assets.coefficients.forecast")
	}

	
	#create blank ppnr capital forecast time series
	.ppnr.forecast.ts <- ts(matrix(NA, ncol = 7, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.ppnr.forecast.ts) <- c("Net.Interest.Income", "Non.Int.Non.Trade.Income", "Trading.Income", 
					"Compensation.Exp", "Fixed.Asset.Exp", "Other.Exp", "PPNR")

			#first column of our forecast is just our initial input data
	Lag(.ppnr.forecast.ts)[,"Net.Interest.Income"] <- (Lag(assets.forecast)[,"Interest.Earning.Assets"]/400
				*Lag(assets.coefficients.forecast)[,"Net.Interest.Margin"]) 
	
	Lag(.ppnr.forecast.ts)[,"Non.Int.Non.Trade.Income"] <-
			(Lag(assets.forecast)[,"Total.Assets"]/400
				*Lag(assets.coefficients.forecast)[,"Noninterest.Nontrading.Income.Ratio"])
	
	Lag(.ppnr.forecast.ts)[,"Trading.Income"] <-
			(Lag(assets.coefficients.forecast)[,"Return.on.Trading.Assets"]/400
				*Lag(assets.forecast)[,"Trading.Assets"])
	
	Lag(.ppnr.forecast.ts)[,"Compensation.Exp"] <- (Lag(assets.forecast)[,"Total.Assets"]/400
				*Lag(assets.coefficients.forecast)[,"Compensation.Noninterest.Expense.Ratio"])
	
	Lag(.ppnr.forecast.ts)[,"Fixed.Asset.Exp"] <-
			(Lag(assets.coefficients.forecast)[,"Fixed.Asset.Noninterest.Expense.Ratio"]/400
				*Lag(assets.forecast)[,"Total.Assets"])
	
	Lag(.ppnr.forecast.ts)[,"Other.Exp"] <-
			(Lag(assets.coefficients.forecast)[,"Other.Noninterest.Expense.Ratio"]
				*Lag(assets.forecast)[,"Total.Assets"])
	
	Lag(.ppnr.forecast.ts)[,"PPNR"] <-
			(sum(Lag(.ppnr.forecast.ts[, 1:3])) - sum(Lag(.ppnr.forecast.ts[, 4:6])))

	return (View(.ppnr.forecast.ts))
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/ppnr_forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_ppnr.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#PPNRForecast(assets.forecast, model.coefficients.ppnr, macro.forecasts)