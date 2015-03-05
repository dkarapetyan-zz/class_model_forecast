# Author: David Karapetyan
###############################################################################
#This Function outputs a time series capital forecast
#from time series inputs PPNR, AFS and LLL forecasts


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

CapitalForecast <- function(position.data, model.coefficients, macro.forecasts) {
	source("c:/ppnr.quant.repo/class_model/src/afs_forecast/afs_forecast_david.R")
	source("c:/ppnr.quant.repo/class_model/src/ppnr_forecast/ppnr_forecast_david.R")
	source("c:/ppnr.quant.repo/class_model/src/lll_provision_forecast/lll_provision_forecast_david.R")
	
	
#testing of inputs done in subroutines called by function	
	
	
	#create blank capital forecast time series
	.capital.forecast.ts <- ts(matrix(NA, ncol = 8, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.capital.forecast.ts) <- c("Net.Income.Before.Tax", "Allowed.DTA","Taxes", 
			"Net.Income","Dividends", "Capital","Tier.1.Common.Capital", "Leverage.Ratio")
	
	
# Fill out row entries of capital forecast using arithmetic operations on ppnr, lll, and afs	
	
	.row<-nrow(.capital.forecast.ts)  #efficiency in looping. 
	for(i in 1:.row)
	{
		if (i==1)
		{
			if (is.na(position.data[1, "Net.Deferred.Tax.Asset...000."]
							- position.data[1, "Disallowed.Deferred.Taxes...000."])) {
				.capital.forecast.ts[1, "Allowed.DTA"] <-
						position.data[1, "Net.Deferred.Tax.Asset...000."]
			}
			else { 
				.capital.forecast.ts[1, "Allowed.DTA"] <- (
							position.data[1, "Net.Deferred.Tax.Asset...000."]
							- position.data[1, "Disallowed.Deferred.Taxes...000."])
			}			
			
			.capital.forecast.ts[1, "Dividends"] <-
					sum(position.data[1,
									c("Less.Purchase.of.Treasury.Stck...000.",
											"Dividends.on.Preferred.Stock...000.", 
											"Dividends.on.Common.Stock...000.")])
			
			.capital.forecast.ts[1, "Capital"] <-
					position.data[1, "GRB..Total.Equity.Capital...000."]
			
			.capital.forecast.ts[1, "Tier.1.Common.Capital"] <-
					position.data[1, "B3.GRB..Tier.1.Common.Capital..CET1....000."]
			
			.capital.forecast.ts[1, "Leverage.Ratio"] <- (
						position.data[1, "B3.GRB..Tier.1.Common.Capital..CET1....000."]/
						position.data[1, "Total.Assets...000."]
						)	
			
		}
		else {
			.capital.forecast.ts[i, "Net.Income.Before.Tax"] <- (
						PPNRForecast(position.data, model.coefficients, macro.forecasts)[i, "PPNR"]
						- LLLForecast(position.data, model.coefficients, macro.forecasts)[i, "Provision"]
						+ AFSForecast(position.data, model.coefficients, macro.forecasts)[i, "Gain.AFS.Securities"]
						)
			
			.capital.forecast.ts[i, "Taxes"] <- max(0.35 * .capital.forecast.ts[i, "Net.Income.Before.Tax"], 
					max(0.1 * .capital.forecast.ts[i-1, "Tier.1.Common.Capital"] -
									.capital.forecast.ts[i-1, "Allowed.DTA" ], 0))
			
			.capital.forecast.ts[i, "Allowed.DTA"] <- (min(.capital.forecast.ts[i, "Taxes"], 0)
						+ .capital.forecast.ts[i-1, "Allowed.DTA"])
			
			.capital.forecast.ts[i, "Net.Income"] <- (.capital.forecast.ts[i, "Net.Income.Before.Tax"] - 
						.capital.forecast.ts[i, "Taxes"] )	
			
			.capital.forecast.ts[i, "Dividends"] <- (max(0.9* .capital.forecast.ts[i-1, "Dividends"]
										+ (0.1)*(0.45*.capital.forecast.ts[i, "Net.Income"]
											-.capital.forecast.ts[i-1, "Dividends"]),0))
			
			.capital.forecast.ts[i, "Capital"] <-(.capital.forecast.ts[i-1, "Capital"] 
						+ .capital.forecast.ts[i, "Net.Income"] + .capital.forecast.ts[i, "Dividends"])
			
			.capital.forecast.ts[i, "Tier.1.Common.Capital"] <- (.capital.forecast.ts[i, "Capital"]
						-.capital.forecast.ts[1, "Capital"] + .capital.forecast.ts[1, "Tier.1.Common.Capital"] )
			
			.capital.forecast.ts[i, "Leverage.Ratio"] <- (.capital.forecast.ts[i, "Tier.1.Common.Capital"]
						/(position.data[1, "Total.Assets...000."] * (1 + (0.0125*(i-1)))))
		}	
	}	
	return(.capital.forecast.ts)
}