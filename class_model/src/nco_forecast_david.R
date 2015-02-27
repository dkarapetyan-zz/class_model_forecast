# Author: David Karapetyan
###############################################################################
#This Function outputs a time series nco forecast (15 columns) to 2017 Q4 for Leases, Credit Cards,
#and other variables, from a series input files of 2014 Q3 input data for the same variables.
#The function is fed, as default arguments, arima model coefficients, and macroeconomic forecasts
#for 2014Q3.


NCOForecast <- function(nco.forecast.input, model.coefficients.nco, macro.forecasts) {
	.required_colnames_forecasts <- c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
			"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
			"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
			"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
			"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
			"Other")		
	#	
	.required_colnames_model.coefficients.nco <-
			c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
					"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
					"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
					"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
					"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
					"Other")	
	
	.required_colnames_macro <- c("Home.price.growth", "Commercial.Property.Price.Growth",
			"Home.price.growth.if.growth.is.negative",
			"Commercial.Property.Price.Growth.Negative", "Annualized.Change.in.Unemployment", 
			"Time.trend")
	
	
	if (!all(.required_colnames_forecasts %in% colnames(nco.forecast.input))) {
		stop("Not all required colnames were found in nco.forecast.input")
	}
	if (!all(.required_colnames_model.coefficients.nco %in% colnames(model.coefficients.nco))) {
		stop("Not all required colnames were found in model.coefficients.nco")
	}
	if (!all(.required_colnames_macro %in% colnames(macro.forecasts))) {
		stop("Not all required colnames were found in macro.forecasts")
	}
#TODO write test that checks that inputs have the same columns, arranged in the same order
	
	#create blank capital forecast time series
	.nco.forecast.ts <- ts(matrix(NA, ncol = 15, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.nco.forecast.ts) <-
			c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
					"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
					"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
					"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
					"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
					"Other")	
	#first column of our forecast is just our initial input data
	.nrows<-nrow(.nco.forecast.ts) #for efficiency in looping
	for(i in 1:.nrows) {
		if (i==1) { #initial data for nco assigned here
				.nco.forecast.ts[i,] <- nco.forecast.input[i,]
			}
		else { # arithmetic
			
			.nco.forecast.ts[i, ] <- (model.coefficients.nco["Intercept", ] +
					model.coefficients.nco["Lagged.dependent.variable",]
						*.nco.forecast.ts[(i-1), ] +
					model.coefficients.nco["Home.price.growth", ]* macro.forecasts[i,"Home.price.growth"] + 
					model.coefficients.nco["Home.price.growth.if.growth.is.negative",]
						*macro.forecasts[i,"Home.price.growth.if.growth.is.negative"])  
		}
	}
	
	return (View(.nco.forecast.ts))
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/nco_forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_nco.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#NCOForecast(nco.forecast.input, model.coefficients.nco, macro.forecasts)