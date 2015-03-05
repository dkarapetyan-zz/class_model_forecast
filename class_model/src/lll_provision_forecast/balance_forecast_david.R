# Author: David Karapetyan
###############################################################################
#This Function outputs a time series balance forecast
#from a time series input file for bank position data

#' 
#' @param position.data  A matrix of a particular bank's present balance sheet
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' @export
#' 
#' 


BalanceForecast <- function(position.data) {
	.required_colnames <- c("U.S..RE..Cl.end.Frst.Lien.1.4...000.", "U.S..RE..Cl.end.Jr.Lien.1.4...000.", 
			"U.S..RE..Rev.1.4.Fam..HE.Lines....000.", "U.S..RE..Constr...Land.Dev...000.", 
			"U.S..RE..Multifamily.Loans...000.", "U.S..RE..Comm.RE.Nonfarm.NonRes....000.", 
			"Con..Tot.Comm...Ind.Loans...000.", "Con..Credit.Cards...Rel.Plans...000.", 
			"Con..Tot.Consumer.Loans...000.", "Con..Total.Leases...000.", 
			"Con..Total.Real.Estate.Loans...000.", "Con..Loans.to.Depository.Institutions...000.", 
			"Con..Agricultural.Prod.Loans...000.", "Con..non.U.S..Government.Loans...000.", 
			"Other.Loans...000.")
	#	
	if (!all(.required_colnames %in% colnames(position.data))) {
		stop("Not all required colnames were found in position.data")
	}
	
	#create blank capital forecast time series
	.balance.forecast.ts <- ts(matrix(NA, ncol = 15, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.balance.forecast.ts) <-
			c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
					"HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", 
					"Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE", 
					"Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate", 
					"Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
					"Other")		
	#first column of our forecast is just our initial input data
	.subset1 <- c("U.S..RE..Cl.end.Frst.Lien.1.4...000.", "U.S..RE..Cl.end.Jr.Lien.1.4...000.", 
			"U.S..RE..Rev.1.4.Fam..HE.Lines....000.", "U.S..RE..Constr...Land.Dev...000.", 
			"U.S..RE..Multifamily.Loans...000.", "U.S..RE..Comm.RE.Nonfarm.NonRes....000."
	)
	.nrows<-nrow(.balance.forecast.ts) #for efficiency in looping
	for(i in 1:.nrows) {
		if (i ==1) {
			.balance.forecast.ts[1, colnames(.balance.forecast.ts)[1:6]] <- position.data[1,.subset1]
			.balance.forecast.ts[1, "Credit.Card"] <- 
					position.data[1, "Con..Credit.Cards...Rel.Plans...000."]
			.balance.forecast.ts[1, "Other.Consumer"] <- (
						position.data[1, "Con..Tot.Consumer.Loans...000."]
						- position.data[1, "Con..Credit.Cards...Rel.Plans...000."]
						)
			.balance.forecast.ts[1, "CI"] <- position.data[1, "Con..Tot.Comm...Ind.Loans...000."]
			.balance.forecast.ts[1, "Leases"] <- position.data[1, "Con..Total.Leases...000."]
			.balance.forecast.ts[1, "Other.Real.Estate"] <- (
						position.data[1, "Con..Total.Real.Estate.Loans...000."]
						- sum(position.data[1, .subset1])
						)
			
			.balance.forecast.ts[1, "Loans.to.Foreign.Governments"] <- (
						position.data[1, "Con..non.U.S..Government.Loans...000."]
						)
			.balance.forecast.ts[1, "Agriculture"] <- 
					position.data[1,"Con..Agricultural.Prod.Loans...000."]
			
			.balance.forecast.ts[1, "Loans.to.Depository.Institutions"] <-
					position.data[1,"Con..Loans.to.Depository.Institutions...000."]
			
			.balance.forecast.ts[1, "Other"] <- position.data[1,"Other.Loans...000."]
		}
		
		else {
			.balance.forecast.ts[i, ] <- 1.0125 * .balance.forecast.ts[i-1,]	
		}
	}
	
	return (.balance.forecast.ts)
}
#load("c:/ppnr.quant.repo/class_model/data/balance_forecast_input.RData")
#BalanceForecast(balance.forecast.input)
#for testing