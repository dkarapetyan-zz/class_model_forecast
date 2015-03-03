# Author: David Karapetyan
###############################################################################
#This Function outputs a time series forecast from 2013Q3 to 2017 Q4 for PPNR revenue adjustment coefficients
#such as Net Interest Margin, Compensation Noninterest Expense Ratio, and others
#The function is fed, as default arguments, position data for 2014Q3, arima model coefficients, and
#macroeconomic forecasts 


RevenueCoeffForecast<- function(position.data, model.coefficients, macro.forecasts ) {
	
	.required_colnames_position <- c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
			"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.", 
			"Total.Assets...000.", "Net.Interest.Income...000.", "Total.Noninterest.Income...000.", 
			"NII..Trading.Revenue...000.", "NIE..Salary...Benefits...000.", 
			"NIE..Premises...Fixed.Assets...000.", "Total.Noninterest.Expense...000.", 
			"U.S..RE..Total.1.4.Fmly...000.", "Con..Total.Real.Estate.Loans...000.", 
			"Con..Tot.Comm...Ind.Loans...000.", "Con..Credit.Cards...Rel.Plans...000.", 
			"Total.Trading.Assets...000.", "Total.Securities...000.", "Asset.Share")	
	
	.required_colnames_model.coefficients <-
			c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
					"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
					"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
					"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
					"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
					"Other")	
	
	.required_colnames_macro <- c("Real.GDP.growth", "Nominal.GDP.growth", "Real.disposable.income.growth", 
			"Nominal.disposable.income.growth", "Unemployment.rate", "CPI.inflation.rate", 
			"X3.Month.Treasury.Yield", "X5.year.Treasury.yield", "X10.year.Treasury.yield", 
			"BBB.corporate.yield", "Mortgage.rate", "Prime.rate", "Dow.Jones.Total.Stock.Market.Index..Level.", 
			"House.Price.Index..Level.", "Commercial.Real.Estate.Price.Index..Level.", 
			"Market.Volatility.Index..Level.", "Term.Spread", "Annualized.Real.GDP.growth", 
			"Quarterly.change.in.10.year.Treasury.yield", "Stock.Market.returns", 
			"Quarterly.change.in.BBB.bond.spread", "Quarterly.change.in.BBB.Spread.if.change.is.positive", 
			"Home.price.growth", "Commercial.Property.Price.Growth", "Home.price.growth.if.growth.is.negative", 
			"Commercial.Property.Price.Growth.Negative", "Annualized.Change.in.Unemployment", 
			"Time.trend")	
	
	if (!all(.required_colnames_position %in% colnames(position.data))) {
		stop("Not all required colnames were found in revenue.coeff.forecast.input")
	}
	if (!all(.required_colnames_model.coefficients %in% colnames(model.coefficients))) {
		stop("Not all required colnames were found in model.coefficients")
	}
	if (!all(.required_colnames_macro %in% colnames(macro.forecasts))) {
		stop("Not all required colnames were found in macro.forecasts")
	}
	
	

	#generate process position data, to use in ppnr forecasting artithmetic
	
.position.data.processed <- matrix(NA, nrow=7, ncol=7)
row.names(.position.data.processed) <- 
		c("Residential.RE.Loans.Ratio", "Commercial.RE.Loans.Ratio", 
				"CI.Loans.Ratio", "Credit.Card.Loans.Ratio", "Trading.Assets.Ratio", 
				"Securities.Ratio", "Asset.Share")	
colnames(.position.data.processed) <-
c("B.S.Ratios", "Net.Interest.Margin", "Noninterest.Nontrading.Income.Ratio", 
		"Compensation.Noninterest.Expense.Ratio", "Fixed.Asset.Noninterest.Expense.Ratio", 
		"Other.Noninterest.Expense.Ratio", "Return.on.Trading.Assets")

#fill in remaining columns
.rownames <-row.names(.position.data.processed) #subset only lpart of table we need
.colnames <- colnames(.position.data.processed) # subset only part of table we need 
.cols<-ncol(.position.data.processed) #for efficiency in looping
for(i in 1:.cols) {
	if (i ==1) {
#assign over all rows except Asset.Share
.pd.subset1 <- c("U.S..RE..Total.1.4.Fmly...000.", "Con..Total.Real.Estate.Loans...000.", 
		"Con..Tot.Comm...Ind.Loans...000.", "Con..Credit.Cards...Rel.Plans...000.", 
		"Total.Trading.Assets...000.", "Total.Securities...000.")
.pd.subset2<- c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
		"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
			
		.position.data.processed[!(row.names(.position.data.processed) %in% c("Asset.Share")),
				"B.S.Ratios"] <- position.data[1,.pd.subset1]/sum(position.data[1,.pd.subset2]) * 100

		#take care of Asset.Share
		.position.data.processed["Asset.Share", "B.S.Ratios"] <- position.data[1,"Asset.Share"]	
	}
	else { #looping portion
		.position.data.processed[,.colnames[i]] <- (model.coefficients[.rownames,.colnames[i]]
		*.position.data.processed[,"B.S.Ratios"])
	}
}

#create blank capital forecast time series
	.revenue.coeff.forecast.ts <-
			ts(matrix(NA, ncol = 6, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	cols <- colnames(.revenue.coeff.forecast.ts) <-
			c("Net.Interest.Margin","Noninterest.Nontrading.Income.Ratio", "Compensation.Noninterest.Expense.Ratio", 
					"Fixed.Asset.Noninterest.Expense.Ratio", "Other.Noninterest.Expense.Ratio", 
					"Return.on.Trading.Assets")	
	
	#first column of our forecast is just our initial input data
	.summing.vec <- c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
			"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
	.nrows<-nrow(.revenue.coeff.forecast.ts) #for efficiency in looping
	for(i in 1:.nrows) { 
		if (i==1) { #initial data for ppnr coefficients. Comes from arithmetic on position data
			.revenue.coeff.forecast.ts[i,"Net.Interest.Margin"] <-
					position.data[1,"Net.Interest.Income...000."]/sum(position.data[1,.summing.vec])*400	
			
			.revenue.coeff.forecast.ts[i,"Noninterest.Nontrading.Income.Ratio"] <- (
						(position.data[1,"Total.Noninterest.Income...000."] -
						position.data[1,"NII..Trading.Revenue...000."])/position.data[1,"Total.Assets...000."] * 400
					)
			
			.revenue.coeff.forecast.ts[i,"Compensation.Noninterest.Expense.Ratio"] <- (
					position.data[1, "NIE..Salary...Benefits...000."]/position.data[1,"Total.Assets...000."] * 400
					)
			
			.revenue.coeff.forecast.ts[i, "Fixed.Asset.Noninterest.Expense.Ratio"] <- (
					position.data[1,"NIE..Premises...Fixed.Assets...000."]/position.data[1,"Total.Assets...000."] * 400
					)
			
			.revenue.coeff.forecast.ts[i, "Other.Noninterest.Expense.Ratio"] <- (
						((position.data[1,"Total.Noninterest.Expense...000."]
						- sum(position.data[1, c("NIE..Salary...Benefits...000.", "NIE..Premises...Fixed.Assets...000.")]))
							/position.data[1, "Total.Assets...000."]) *400)
			
			.revenue.coeff.forecast.ts[i, "Return.on.Trading.Assets"] <-  (
					position.data[1,"NII..Trading.Revenue...000."]/position.data[1,"Total.Trading.Assets...000."] * 400 
					)
		}
		else { # arithmetic
			
			.revenue.coeff.forecast.ts[i, "Net.Interest.Margin" ] <-
					(model.coefficients["Intercept", "Net.Interest.Margin"]
						+ model.coefficients["Lagged.dependent.variable", "Net.Interest.Margin"]
						*.revenue.coeff.forecast.ts[(i-1), "Net.Interest.Margin"]
						+ model.coefficients["Term.Spread", "Net.Interest.Margin"]
						*macro.forecasts[i,"Term.Spread"]
						+ model.coefficients["X3.Month.Treasury.Yield","Net.Interest.Margin"]
						*macro.forecasts[i,"X3.Month.Treasury.Yield"]
						+  model.coefficients["Time.trend", "Net.Interest.Margin"]
						*macro.forecasts[i,"Time.trend"]
						+ sum(.position.data.processed[, "Net.Interest.Margin"]))
			
			.cols<-c("Noninterest.Nontrading.Income.Ratio","Compensation.Noninterest.Expense.Ratio")
			.revenue.coeff.forecast.ts[i, .cols] <- (
						model.coefficients["Intercept", .cols]
						+ model.coefficients["Lagged.dependent.variable", .cols]
						*.revenue.coeff.forecast.ts[(i-1), .cols]
						+ model.coefficients["Stock.Market.returns", .cols]
						*macro.forecasts[i,"Stock.Market.returns"]
						+ c(sum(.position.data.processed[, .cols[1]]), sum(.position.data.processed[, .cols[2]]))
						)
			
			.revenue.coeff.forecast.ts[i, "Fixed.Asset.Noninterest.Expense.Ratio"] <- (
						model.coefficients["Intercept", "Fixed.Asset.Noninterest.Expense.Ratio"]
						+ model.coefficients["Lagged.dependent.variable", "Fixed.Asset.Noninterest.Expense.Ratio"]
						*.revenue.coeff.forecast.ts[(i-1), "Fixed.Asset.Noninterest.Expense.Ratio"]
						+ model.coefficients["Annualized.Change.in.Unemployment", "Fixed.Asset.Noninterest.Expense.Ratio"]
						*macro.forecasts[i,"Annualized.Change.in.Unemployment"]
						+ sum(.position.data.processed[, "Fixed.Asset.Noninterest.Expense.Ratio"])
						)
			
			.revenue.coeff.forecast.ts[i, "Other.Noninterest.Expense.Ratio"] <- (
						model.coefficients["Intercept", "Other.Noninterest.Expense.Ratio"]
						+ model.coefficients["Lagged.dependent.variable", "Other.Noninterest.Expense.Ratio"]
						*.revenue.coeff.forecast.ts[(i-1), "Other.Noninterest.Expense.Ratio"]
						+ model.coefficients["Quarterly.change.in.BBB.bond.spread", "Other.Noninterest.Expense.Ratio"]
						*macro.forecasts[i,"Quarterly.change.in.BBB.bond.spread"]
						+ sum(.position.data.processed[, "Other.Noninterest.Expense.Ratio"])
						)
			
			.revenue.coeff.forecast.ts[i, "Return.on.Trading.Assets"] <- (
						model.coefficients["Intercept", "Return.on.Trading.Assets"]
						+ model.coefficients["Lagged.dependent.variable", "Return.on.Trading.Assets"]
						*.revenue.coeff.forecast.ts[(i-1), "Return.on.Trading.Assets"]
						+ model.coefficients["Quarterly.change.in.BBB.bond.spread", "Return.on.Trading.Assets"]
						*macro.forecasts[i,"Quarterly.change.in.BBB.bond.spread"]
						+ model.coefficients["Quarterly.change.in.BBB.Spread.if.change.is.positive",
								"Return.on.Trading.Assets"]
						*macro.forecasts[i,"Quarterly.change.in.BBB.Spread.if.change.is.positive"]
						+ sum(.position.data.processed[, "Return.on.Trading.Assets"])
						)
		}
	}
	
	return (.revenue.coeff.forecast.ts)
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/position_data.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#RevenueCoeffForecast(position.data, model.coefficients, macro.forecasts)