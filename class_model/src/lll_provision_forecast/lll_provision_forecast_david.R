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
			"Other", "Total.Reserves...000.")
#	
	if (!all(.required_colnames %in% colnames(individual.forecasts))) {
		stop("Not all required colnames were found in individual.forecasts")
	}
	
	#get initial reserves from data, after which we munge slightly to get rid of pesky NA's
	#only one entry in one quarter had relevant data, all other entries in same quarter were NA
	.total.reserves.initial <- individual.forecasts[1, "Total.Reserves...000."] 		
	.col.ids.to.keep <- !(colnames(individual.forecasts) %in% c("Total.Reserves...000."))
	individual.forecasts <- individual.forecasts[-1, .col.ids.to.keep] 		
	#create blank capital forecast time series
	.lll.forecast.ts <- ts(matrix(NA, ncol = 4, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.lll.forecast.ts) <- 
			c("Total.Net.Charge.offs", "X4.Qrt.Net.Charge.offs", "Total.Reserves...000.", 
					"Provision")
	
	.lll.forecast.ts[1, "Total.Reserves...000."] <- .total.reserves.initial
	.row<-nrow(individual.forecasts)  #efficiency in looping. 
	for(i in 2:(.row)) {
		#sum over all necessary elements. Subset out unnecessary
		.lll.forecast.ts[i,"Total.Net.Charge.offs"] <- sum(individual.forecasts[i,])
	}
	#loop. Variables below end one year before end of input data time series
	for(i in 2:(.row-4)) {
		.lll.forecast.ts[i,"X4.Qrt.Net.Charge.offs"] <-
				sum(.lll.forecast.ts[(i+1):(i+4),"Total.Net.Charge.offs"])	
		.lll.forecast.ts[i, "Total.Reserves...000."] <- 
				if ((.lll.forecast.ts[(i-1), "Total.Reserves...000."] 
							- .lll.forecast.ts[i, "Total.Net.Charge.offs"]) <
						.lll.forecast.ts[i, "X4.Qrt.Net.Charge.offs"]) {
					.lll.forecast.ts[i, "X4.Qrt.Net.Charge.offs"]	
				} 
				else if 
				((.lll.forecast.ts[(i-1), "Total.Reserves...000." ] 
							- .lll.forecast.ts[i, "Total.Net.Charge.offs"]) >
						.lll.forecast.ts[i, "X4.Qrt.Net.Charge.offs"]*2.5) {
					.lll.forecast.ts[i, "X4.Qrt.Net.Charge.offs"]*2.5	
				}
				else {
					.lll.forecast.ts[(i-1), "Total.Reserves...000."] 
				}
		.lll.forecast.ts[i, "Provision"] <- (.lll.forecast.ts[i, "Total.Reserves...000."] + 
					.lll.forecast.ts[i, "Total.Net.Charge.offs"]
					-.lll.forecast.ts[(i-1), "Total.Reserves...000."])
	}
	
	
	return (.lll.forecast.ts)
}

