# Author: David Karapetyan
###############################################################################
#This Function outputs a time series lll provision forecast (4 columns)
#from a time series input file of lll provision data, with an additional input
#of total reserve data for the period of one quarter prior to the time series beginning

LLLForecast <- function(individual.forecasts) {
	.required_colnames <- c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
			"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
			"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
			"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
			"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
			"Other", "Total.Reserves...000.", "Provision")
#	
	if (!all(.required_colnames %in% colnames(individual.forecasts))) {
		stop("Not all required colnames were found in individual.forecasts")
	}
	
	
	
	#create blank capital forecast time series
	.lll.forecast.ts <- ts(matrix(NA, ncol = 4, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.lll.forecast.ts) <- 
			c("Total.Net.Charge.offs", "X4.Qrt.Net.Charge.offs", "Total.Reserves...000.", 
					"Provision")
	
	
	.row<-nrow(.lll.forecast.ts)  #efficiency in looping. 
browser()	
	for(i in 1:.row) {
		#sum over all necessary elements. Subset out unnecessary
	.lll.forecast.ts[i,"Total.Net.Charge.offs"] <- sum(individual.forecasts[i,
					c("Total.Net.Charge.offs",
							"Total.Reserves...000.", "Provision", "X4.Qrt.Net.Charge.offs")])	
	.lll.forecast.ts[i,"X4.Qrt.Net.Charge.offs"] <- sum(individual.forecasts[i,"Total.Net.Charge.offs"])	
	}	
	
	View(.lll.forecast.ts)
}

# Load the class_model_input time series.
setwd("c:\\ppnr.quant.repo\\class_model\\data\\")
load("lll_input.RData")

LLLForecast(lll.input)
