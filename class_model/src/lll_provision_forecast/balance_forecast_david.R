# Author: David Karapetyan
###############################################################################
#This Function outputs a time series balance forecast (15 columns) to 2017 Q4 for Leases, Credit Cards,
#and other variables, from a time series input file of 2014 Q3 input data for the same variables.

BalanceForecast <- function(individual.forecasts) {
	.required_colnames <- c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
			"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
			"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
			"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
			"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
			"Other")		
	#	
	if (!all(.required_colnames %in% colnames(individual.forecasts))) {
		stop("Not all required colnames were found in individual.forecasts")
	}
	
	#create blank capital forecast time series
	.lll.forecast.ts <- ts(matrix(NA, ncol = 15, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.lll.forecast.ts) <-
			c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
					"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
					"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
					"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
					"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
					"Other")		
	#first column of our forecast is just our initial input data
	.lll.forecast.ts[1, ] <- individual.forecasts[1,]
	.nrows<-nrow(.lll.forecast.ts) #for efficiency in looping
	for(i in 2:.nrows) {
		.lll.forecast.ts[i, ] <- 1.0125 * .lll.forecast.ts[i-1,]	
	}
	
	return (.lll.forecast.ts)
}
#load("c:/ppnr.quant.repo/class_model/data/balance_forecast_input.RData")
#BalanceForecast(balance.forecast.input)
#for testing