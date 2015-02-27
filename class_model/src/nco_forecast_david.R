# Author: David Karapetyan
###############################################################################
#This Function outputs a time series balance forecast (15 columns) to 2017 Q4 for Leases, Credit Cards,
#and other variables, from a time series input file of 2014 Q3 input data for the same variables.

NCOForecast <- function(nco.forecast.input, model.coefficients, macro.forecasts) {
	.required_colnames_forecasts <- c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
			"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
			"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
			"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
			"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
			"Other")		
	#	
	.required_colnames_model_coefficients <-
			c("Net.Interest.Margin", "Noninterest.Nontrading.Income.Ratio", 
					"Return.on.Trading.Assets", "Compensation.Noninterest.Expense.Ratio", 
					"Fixed.Asset.Noninterest.Expense.Ratio", "Other.Noninterest.Expense.Ratio", 
					"Return.on.AFS.Securities", "FirstLien.Residential.Real.Estate", 
					"Junior.Lien.Residential.Real.Estate", "HELOC.Residential.Real.Estate", 
					"Construction.Commercial.Real.Estate", "Multifamily.Commercial.Real.Estate", 
					"NonFarm.NonResidential.CRE", "Credit.Card", "Other.Consumer", 
					"CI", "Leases", "Other.Real.Estate", "Loans.to.Foreign.Governments", 
					"Agriculture", "Loans.to.Depository.Institutions", "Other")

	.required_colnames_macro <- c("Home.price.growth", "Commercial.Property.Price.Growth",
			"Home.price.growth.if.growth.is.negative",
			"Commercial.Property.Price.Growth.Negative", "Annualized.Change.in.Unemployment", 
			"Time.trend")
	
	
	if (!all(.required_colnames_forecasts %in% colnames(nco.forecast.input))) {
		stop("Not all required colnames were found in nco.forecast.input")
	}
	if (!all(.required_colnames_model_coefficients %in% colnames(model.coefficients))) {
		stop("Not all required colnames were found in model.coefficients")
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
	.nco.forecast.ts[1,] <- nco.forecast.input[1,]
	#first column of our forecast is just our initial input data
	.nrows<-nrow(.nco.forecast.ts) #for efficiency in looping
	for(i in 2:.nrows) {
		browser()
		.nco.forecast.ts[i, ] <- model.coefficients["Intercept", ] +
				(model.coefficients["Lagged.dependent.variable",]
		*.nco.forecast.ts[(i-1), ]) + (model.coefficients["Home.price.growth",]*
		macro.forecasts[i,"Home.price.growth"]) + 
	(model.coefficients["Home.price.growth.if.growth.is.negative",]*macro.forecasts[i,"Home.price.growth"])  
	}
	
	return (View(.nco.forecast.ts))
}
load("c:/ppnr.quant.repo/class_model/data/nco_forecast_input.RData")
load("c:/ppnr.quant.repo/class_model/data/model_coefficients.RData")
load("c:/ppnr.quant.repo/class_model/data/macro_forecast.RData")

NCOForecast(nco.forecast.input, model.coefficients, macro.forecasts)
#for testing