# Author: David Karapetyan
###############################################################################
#This Function outputs a time series nco forecast (15 columns) to 2017 Q4 for Leases, Credit Cards,
#and other variables, from a series input files of 2014 Q3 input data for the same variables.
#The function is fed, as default arguments, arima model coefficients, and macroeconomic forecasts
#for 2014Q3.


RevenueCoeffForecast<- function(position.data, model.coefficients.ppnr, macro.forecasts ) {
	
	.required_colnames_position <- c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
			"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.", 
			"Total.Assets...000.", "Net.Interest.Income...000.", "Total.Noninterest.Income...000.", 
			"NII..Trading.Revenue...000.", "NIE..Salary...Benefits...000.", 
			"NIE..Premises...Fixed.Assets...000.", "Total.Noninterest.Expense...000.", 
			"U.S..RE..Total.1.4.Fmly...000.", "Con..Total.Real.Estate.Loans...000.", 
			"Con..Tot.Comm...Ind.Loans...000.", "Con..Credit.Cards...Rel.Plans...000.", 
			"Total.Trading.Assets...000.", "Total.Securities...000.")	
	
	#	
	.required_colnames_model.coefficients.ppnr <-
			c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
					"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
					"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
					"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
					"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
					"Other")	
	
	.required_colnames_macro <- c("Annualized.Change.in.Unemployment", 
			"Quarterly.change.in.10.year.Treasury.yield", "Quarterly.change.in.BBB.bond.spread", 
			"Quarterly.change.in.BBB.Spread.if.change.is.positive", "Stock.Market.returns", 
			"Term.Spread", "Time.trend", "X3.Month.Treasury.Yield")
			
			
	
	if (!all(.required_colnames_position %in% colnames(position.data))) {
		stop("Not all required colnames were found in revenue.coeff.forecast.input")
	}
	if (!all(.required_colnames_model.coefficients.nco %in% colnames(model.coefficients.ppnr))) {
		stop("Not all required colnames were found in model.coefficients.nco")
	}
	if (!all(.required_colnames_macro %in% colnames(macro.forecasts))) {
		stop("Not all required colnames were found in macro.forecasts")
	}
	
	
	#We now subset position data to use only the fields needed for ppnr. 
	position.data <- position.data[1, .required_colnames_position]	
	
	
	#create blank capital forecast time series
	.revenue.coeff.forecast.ts <-
			ts(matrix(NA, ncol = 6, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.revenue.coeff.forecast.ts) <-
			c("Noninterest.Nontrading.Income.Ratio", "Compensation.Noninterest.Expense.Ratio", 
					"Fixed.Asset.Noninterest.Expense.Ratio", "Other.Noninterest.Expense.Ratio", 
					"Return.on.Trading.Assets")	
			
	#first column of our forecast is just our initial input data
	.nrows<-nrow(.revenue.coeff.forecast.ts) #for efficiency in looping
	for(i in 1:.nrows) {
		if (i==1) { #initial data for ppnr coefficients. Comes from arithmetic on position data
				.revenue.coeff.forecast.ts[i,] <- revenue.coeff.forecast.input[i,]
			}
		else { # arithmetic
			
			.revenue.coeff.forecast.ts[i, ] <- (model.coefficients.nco["Intercept", ] +
					model.coefficients.nco["Lagged.dependent.variable",]
						*.revenue.coeff.forecast.ts[(i-1), ] +
					model.coefficients.nco["Home.price.growth", ]* macro.forecasts[i,"Home.price.growth"] + 
					model.coefficients.nco["Home.price.growth.if.growth.is.negative",]
						*macro.forecasts[i,"Home.price.growth.if.growth.is.negative"])  
		}
	}
	
	return (View(.revenue.coeff.forecast.ts))
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/nco_forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_nco.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#RevenueCoeffForecastForecast(revenue.coeff.forecast.input, model.coefficients.nco, macro.forecasts)