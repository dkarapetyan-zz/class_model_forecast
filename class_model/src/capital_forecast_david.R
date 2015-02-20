# Author: David Karapetyan
###############################################################################
#This Function outputs a time series capital forecast
#from a time series input file of PPNR, AFS and LLL

CapitalForecast <- function(individual.forecasts)
{
	.required_colnames <- c("Provision", "Gain.AFS.Securities", "PPNR") 
	
	if (!all(.required_colnames %in% colnames(individual.forecasts))) {
		stop("Not all required colnames were found in individual.forecasts")
	}
	
	
	# TODO write capital forecast.
	
	#create blank capital forecast time series
	.capital.forecast.ts <- ts(matrix(NA, ncol = 8, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.capital.forecast.ts) <- c("Net.Income.Before.Tax", "Allowed.DTA","Taxes", 
			"Net.Income","Dividends", "Capital","Tier.1.Common.Capital", "Leverage.Ratio")
	
	
# Fill out row entries of capital forecast using arithmetic operations on ppnr, lll, and afs	
	
#capital forecast initial data	
	.capital.forecast.ts[1,] <- c(NA, 29676000, NA, NA, 765000,238681000, 152444000, 0.0717)
	
	.capital.forecast.ts[,"Net.Income.Before.Tax"] <- (individual.forecasts[, "PPNR"]
				- individual.forecasts[, "Provision" ] + individual.forecasts[, "Gain.AFS.Securities"])
	
	
	.row<-nrow(.capital.forecast.ts)  #efficiency in looping. 
	.total.assets<-2126138000 #needed position variable
	for(i in 2:.row)
	{
		
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
				/(.total.assets * (1 + (0.0125*(i-1)))))
		
	}	
View(.capital.forecast.ts)
}

# Load the class_model_input time series.
setwd("c:\\ppnr.quant.repo\\class_model\\data\\")
load("class_model_input.RData")

CapitalForecast(class_model_input)
