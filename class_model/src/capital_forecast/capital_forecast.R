# Author: David Karapetyan This Function outputs a time series capital forecast
# from time series inputs PPNR, AFS and LLL forecasts


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

CapitalForecast <- function(position_data, model_coefficients, macro_forecasts) {
	source("src/afs_forecast/afs_forecast.R")
	source("src/ppnr_forecast/ppnr_forecast.R")
	source("src/lll_provision_forecast/lll_provision_forecast.R")
	
	
	# testing of inputs done in subroutines called by function
	
	
	# create blank capital forecast time series
	.capital_forecast_ts <- ts(matrix(NA, ncol = 8, nrow = 14), start = c(2014, 3), 
			end = c(2017, 4), frequency = 4)
	colnames(.capital_forecast_ts) <- c("Net.Income.Before.Tax", "Allowed.DTA", "Taxes", 
			"Net.Income", "Dividends", "Capital", "Tier.1.Common.Capital", "Leverage.Ratio")
	
	
	# Fill out row entries of capital forecast using arithmetic operations on ppnr,
	# lll, and afs
	
	.row <- nrow(.capital_forecast_ts)  #efficiency in looping. 
	for (i in 1:.row) {
		if (i == 1) {
			if (is.na(position_data[1, "Net.Deferred.Tax.Asset...000."] - position_data[1, 
							"Disallowed.Deferred.Taxes...000."])) {
				.capital_forecast_ts[1, "Allowed.DTA"] <- position_data[1, "Net.Deferred.Tax.Asset...000."]
			} else {
				.capital_forecast_ts[1, "Allowed.DTA"] <- (position_data[1, "Net.Deferred.Tax.Asset...000."]
							- position_data[1, "Disallowed.Deferred.Taxes...000."])
			}
			
			.capital_forecast_ts[1, "Dividends"] <- sum(position_data[1,
							c("Less.Purchase.of.Treasury.Stck...000.",
									"Dividends.on.Preferred.Stock...000.",
									"Dividends.on.Common.Stock...000.")])
			
			.capital_forecast_ts[1, "Capital"] <- position_data[1, "GRB..Total.Equity.Capital...000."]
			
			.capital_forecast_ts[1, "Tier.1.Common.Capital"] <- position_data[1,
					"B3.GRB..Tier.1.Common.Capital..CET1....000."]
			
			.capital_forecast_ts[1, "Leverage.Ratio"] <- (position_data[1,
								"B3.GRB..Tier.1.Common.Capital..CET1....000."]/position_data[1,
								"Total.Assets...000."])
			
		} else {
			.capital_forecast_ts[i, "Net.Income.Before.Tax"] <- (PPNRForecast(position_data, 
								model_coefficients, macro_forecasts)[i, "PPNR"]
						- LLLForecast(position_data, model_coefficients, macro_forecasts)[i, "Provision"]
						+ AFSForecast(position_data, model_coefficients, macro_forecasts)[i,
								"Gain.AFS.Securities"])
			
			.capital_forecast_ts[i, "Taxes"] <- max(0.35 * .capital_forecast_ts[i, 
							"Net.Income.Before.Tax"], max(0.1 * .capital_forecast_ts[i - 1,
											"Tier.1.Common.Capital"]
									- .capital_forecast_ts[i - 1, "Allowed.DTA"], 0))
			
			.capital_forecast_ts[i, "Allowed.DTA"] <- (min(.capital_forecast_ts[i, 
										"Taxes"], 0) + .capital_forecast_ts[i - 1, "Allowed.DTA"])
			
			.capital_forecast_ts[i, "Net.Income"] <- (.capital_forecast_ts[i, "Net.Income.Before.Tax"] - 
						.capital_forecast_ts[i, "Taxes"])
			
			.capital_forecast_ts[i, "Dividends"] <- (max(0.9 * .capital_forecast_ts[i -
			1, "Dividends"] + (0.1) * (0.45 * .capital_forecast_ts[i, "Net.Income"] -
			.capital_forecast_ts[i - 1 , "Dividends"]), 0))
			
			.capital_forecast_ts[i, "Capital"] <- (.capital_forecast_ts[i - 1, "Capital"]
						+ .capital_forecast_ts[i, "Net.Income"]
						+ .capital_forecast_ts[i, "Dividends"])
			
			.capital_forecast_ts[i, "Tier.1.Common.Capital"] <- (.capital_forecast_ts[i, 
								"Capital"]
						- .capital_forecast_ts[1, "Capital"]
						+ .capital_forecast_ts[1, 
								"Tier.1.Common.Capital"])
			
			.capital_forecast_ts[i, "Leverage.Ratio"] <- (.capital_forecast_ts[i, 
								"Tier.1.Common.Capital"]/(position_data[1, "Total.Assets...000."]
							* (1 + (0.0125 * (i - 1)))))
		}
	}
	return(.capital_forecast_ts)
} 
