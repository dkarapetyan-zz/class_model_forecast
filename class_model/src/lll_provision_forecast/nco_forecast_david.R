# Author: David Karapetyan
###############################################################################

#This Function outputs a time series NCO forecast from time series input files
#for bank position data, macroeconomic forecasts , and arima model coefficients 

#' 
#' @param position.data  A matrix of a particular bank's present balance sheet
#' @param model.coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro.forecasts A matrix of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#

NCOForecast <- function(position.data, model.coefficients, macro.forecasts) {
	
	.required_colnames_position <- c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
			"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.", 
			"Total.Assets...000.", "Net.Interest.Income...000.", "Total.Noninterest.Income...000.", 
			"NII..Trading.Revenue...000.", "NIE..Salary...Benefits...000.", 
			"NIE..Premises...Fixed.Assets...000.", "Total.Noninterest.Expense...000.", 
			"U.S..RE..Total.1.4.Fmly...000.", "Con..Total.Real.Estate.Loans...000.", 
			"Con..Tot.Comm...Ind.Loans...000.", "Con..Credit.Cards...Rel.Plans...000.", 
			"Total.Trading.Assets...000.", "Total.Securities...000.", "Asset.Share")	
	
	#	
	.req_cols_mod_coeffs_nco <-
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
	
	
	if (!all(.required_colnames_position %in% colnames(position.data))) {
		stop("Not all required colnames were found in position.data")
	}
	if (!all(.req_cols_mod_coeffs_nco %in% colnames(model.coefficients))) {
		stop("Not all required colnames were found in model.coefficients")
	}
	if (!all(.required_colnames_macro %in% colnames(macro.forecasts))) {
		stop("Not all required colnames were found in macro.forecasts")
	}
#TODO write test that checks that inputs have the same columns, arranged in the same order
	
	#create nco forecast initial data
	
	
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
	#now, we partition set we are looping over--4 groups of arithmetic
	.subset1<-c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
			"HELOC.Residential.Real.Estate")
	.subset2<-c("Credit.Card", "CI", "Leases", "Loans.to.Foreign.Governments", "Agriculture",
			"Loans.to.Depository.Institutions", "Other")
	.subset3<-"Other.Consumer"
	.subset4<-"Other.Real.Estate"
	.subset5<-c("Construction.Commercial.Real.Estate", "Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE")
	for(i in 1:.nrows) {
		if (i==1) { #initial data for nco assigned here
			.nco.forecast.ts[i,"FirstLien.Residential.Real.Estate"] <- (
						(position.data[1,"CO..U.S..RE..Close.end.First.Lien.1.4.Family...000."]
							- position.data[1, "Rec..U.S..RE..Close.end.First.Lien.1.4.Family...000."])/
						position.data[1,"U.S..RE..Cl.end.Frst.Lien.1.4...000."] * 400	
						)
			
			.nco.forecast.ts[i,"Junior.Lien.Residential.Real.Estate"] <- (
						(position.data[1, "CO..U.S..RE..Close.end.Jr.Lien.1.4.Family...000."]
							- position.data[1, "Rec..U.S..RE..Close.end.Jr.Lien.1.4.Family...000."])/
						position.data[1, "U.S..RE..Cl.end.Jr.Lien.1.4...000."] * 400
						)
			.nco.forecast.ts[i, "HELOC.Residential.Real.Estate"]	<- (
						(position.data[1, "CO..U.S..RE..Revolving.1.4.Family..HE.Lines....000."]
							- position.data[1, "Rec..U.S..RE..Revolving.1.4.Family..HE.Lines....000."])/
						position.data[1, "U.S..RE..Rev.1.4.Fam..HE.Lines....000."] * 400
						)
			
			.nco.forecast.ts[i, "Construction.Commercial.Real.Estate"]	<- (
						(position.data[1, "CO..U.S..RE..Construction...Land.Development...000."]
							- position.data[1, "Rec..U.S..RE..Construction...Land.Development...000."])/
						position.data[1, "U.S..RE..Constr...Land.Dev...000."] * 400
						)
			
			.nco.forecast.ts[i, "Multifamily.Commercial.Real.Estate"] <- (
						(position.data[1, "CO..U.S..RE..Multifamily...000."]
							- position.data[1, "Rec..U.S..RE..Multifamily...000."])/
						position.data[1, "U.S..RE..Multifamily.Loans...000."] * 400
						)
			
			.nco.forecast.ts[i, "NonFarm.NonResidential.CRE"]	<- (
						(position.data[1, "CO..U.S..RE..Commercial...000."]
							- position.data[1, "Rec..U.S..RE..Commercial...000."])/
						position.data[1, "U.S..RE..Comm.RE.Nonfarm.NonRes....000."] * 400
						)
			
			.nco.forecast.ts[i, "Credit.Card"]	<- (
						(position.data[1, "CO..Credit.Card.Loans...000."]
							- position.data[1, "Rec..Credit.Card.Loans...000."])/
						position.data[1, "Con..Credit.Cards...Rel.Plans...000."] * 400
						)
			
			.nco.forecast.ts[i, "Other.Consumer"] <- (
						((position.data[1, "CO..Consumer.Loans...000."]
								- position.data[1, "CO..Credit.Card.Loans...000."])
							-(position.data[1, "Rec..Consumer.Loans...000." ] -
								position.data[1, "Rec..Credit.Card.Loans...000."]))/
						(position.data[1,"Con..Tot.Consumer.Loans...000."] -
							position.data[1, "Con..Credit.Cards...Rel.Plans...000."])* 400
						)
			
			.nco.forecast.ts[i, "CI"] <- (
						((position.data[1, "CO..Commercial...Industrial.Lns.U.S..Addressees...000."]
								+ position.data[1, "CO..Commercial...Industrial.Lns.Non.U.S..Address...000."])
							-(position.data[1, "Rec..Commercial...Industrial.Lns.U.S..Addressees...000."]
								+ position.data[1, "Rec..Commercial...Industrial.Lns.Non.U.S..Address...000." ]))/
						position.data[1, "Con..Tot.Comm...Ind.Loans...000."] * 400
						)
			
			
			.nco.forecast.ts[i, "Leases"] <- (
						(position.data[1, "CO..Total.Lease.Financing.Receivables...000."]
							- position.data[1, "Rec..Total.Lease.Financing.Receivables...000."])/
						position.data[1, "Con..Total.Leases...000."] * 400
						)
			
			.cols <- c("U.S..RE..Cl.end.Frst.Lien.1.4...000.", "U.S..RE..Cl.end.Jr.Lien.1.4...000.", 
					"U.S..RE..Rev.1.4.Fam..HE.Lines....000.", "U.S..RE..Constr...Land.Dev...000.", 
					"U.S..RE..Multifamily.Loans...000.", "U.S..RE..Comm.RE.Nonfarm.NonRes....000.")
			
			.nco.forecast.ts[i, "Other.Real.Estate"]	<- (
						((position.data[1, "CO..U.S..RE..Farm.Loans...000."]
								+ position.data[1, "CO..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000."])
							- (position.data[1,"Rec..U.S..RE..Farm.Loans...000."] +
								position.data[1,"Rec..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000."]))/
						(position.data[1, "Con..Total.Real.Estate.Loans...000."]
							-sum(position.data[1, .cols])) * 400
						)
			
			.nco.forecast.ts[i, "Loans.to.Foreign.Governments"]	<- (
						(position.data[1, "CO..Lns.to.non.U.S..Gov...Official.Institutions...000."]
							- position.data[1, "Rec..Lns.to.non.U.S..Gov...Official.Institutions...000."])/
						position.data[1, "Con..non.U.S..Government.Loans...000."] * 400
						)
			
			.nco.forecast.ts[i, "Agriculture"]	<- (
						(position.data[1, "CO..Agricultural.Production.Loans...000."]
							- position.data[1, "Rec..Agricultural.Production.Loans...000."])/
						position.data[1, "Con..Agricultural.Prod.Loans...000."] * 400
						)
			
			.nco.forecast.ts[i, "Loans.to.Depository.Institutions"]	<- (
						(position.data[1, "CO..Total.Lns.to.Dep.Institutions...Acceptances...000."]
							- position.data[1, "Rec..Total.Lns.to.Dep.Institutions...Acceptances...000."])/
						position.data[1, "Con..Loans.to.Depository.Institutions...000."] * 400
						)
			
			.nco.forecast.ts[i, "Other"]	<- (
						(position.data[1, "CO..All.Other.Loans...000."]
							- position.data[1, "Rec..All.Other.Loans...000."])/
						position.data[1, "Other.Loans...000."] * 400
						)
			
		}
		else { # arithmetic
			
			
			.nco.forecast.ts[i, .subset1] <- (
						model.coefficients["Intercept", .subset1]
						+ model.coefficients["Lagged.dependent.variable", .subset1]
						*.nco.forecast.ts[(i-1), .subset1]
						+ model.coefficients["Home.price.growth", .subset1]
						*macro.forecasts[i,"Home.price.growth"]
						+ model.coefficients["Home.price.growth.if.growth.is.negative", .subset1]
						*macro.forecasts[i,"Home.price.growth.if.growth.is.negative"]
						)  
			
			.nco.forecast.ts[i, .subset2] <- (
						model.coefficients["Intercept", .subset2]
						+ model.coefficients["Lagged.dependent.variable", .subset2]
						*.nco.forecast.ts[(i-1), .subset2]
						+ model.coefficients["Annualized.Change.in.Unemployment", .subset2]
						*macro.forecasts[i,"Annualized.Change.in.Unemployment"]
						)
			
			.nco.forecast.ts[i, .subset3] <- (
						model.coefficients["Intercept", .subset3]
						+ model.coefficients["Lagged.dependent.variable", .subset3]
						*.nco.forecast.ts[(i-1), .subset3]
						+ model.coefficients["Annualized.Change.in.Unemployment", .subset3]
						*macro.forecasts[i,"Annualized.Change.in.Unemployment"]
						+ model.coefficients["Time.trend", .subset3]
						*macro.forecasts[i,"Time.trend"]
						)  
			
			.nco.forecast.ts[i, .subset4] <- (
						model.coefficients["Intercept", .subset4]
						+ model.coefficients["Lagged.dependent.variable", .subset4]
						*.nco.forecast.ts[(i-1), .subset4]
						+ model.coefficients["Commercial.Property.Price.Growth", .subset4]
						*macro.forecasts[i,"Commercial.Property.Price.Growth"]
						+ model.coefficients["Commercial.Property.Price.Growth.Negative", .subset4]
						*macro.forecasts[i,"Commercial.Property.Price.Growth.Negative"])  
			
			.nco.forecast.ts[i, .subset5] <- (
						model.coefficients["Intercept", .subset5]
						+ model.coefficients["Lagged.dependent.variable", .subset5]
						*.nco.forecast.ts[(i-1), .subset5]
						+ model.coefficients["Commercial.Property.Price.Growth.Negative", .subset5]
						*macro.forecasts[i,"Commercial.Property.Price.Growth.Negative"])  

		
#TODO write test that checks if model coefficients and macro forecasts names should be identical.
#For BAC, are not for "Commercial.Property.Price.Growth.Negative" and
#"Home.price.growth.if.growth.is.negative"			
			
		}
	}
	
	return (.nco.forecast.ts)
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/nco_forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_nco.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#NCOForecast(nco.forecast.initial, model.coefficients.nco, macro.forecasts)