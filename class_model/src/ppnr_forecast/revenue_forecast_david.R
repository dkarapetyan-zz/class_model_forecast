# Author: David Karapetyan
###############################################################################
#This Function outputs a time series revenue.forecast (15 columns) to 2017 Q4 for Leases, Credit Cards,
#and other variables, from a series input files of 2014 Q3 input data for the same variables.
#The function is fed, as default arguments, arima model coefficients, and macroeconomic forecasts
#for 2014Q3.


RevenueForecast <- function(position.data) {
	.required_colnames_position_data <- 
			c("CO..U.S..RE..Close.end.First.Lien.1.4.Family...000.",
			"CO..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.",
			"CO..U.S..RE..Total.Close.end.1.4.Family...000.",
			"CO..U.S..RE..Revolving.1.4.Family..HE.Lines....000.",
			"CO..U.S..RE..Construction...Land.Development...000.",
			"CO..U.S..RE..Multifamily...000.", "CO..U.S..RE..Commercial...000.",
			"CO..Commercial...Industrial.Lns.U.S..Addressees...000.",
			"CO..Commercial...Industrial.Lns.Non.U.S..Address...000.",
			"CO..Credit.Card.Loans...000.", "CO..Consumer.Loans...000.",
			"CO..Total.Lease.Financing.Receivables...000.",
			"CO..U.S..RE..Farm.Loans...000.",
			"CO..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000.",
			"CO..Total.Lns.to.Dep.Institutions...Acceptances...000.",
			"CO..Agricultural.Production.Loans...000.",
			"CO..Lns.to.non.U.S..Gov...Official.Institutions...000.",
			"CO..All.Other.Loans...000.",
			"Rec..U.S..RE..Close.end.First.Lien.1.4.Family...000.",
			"Rec..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.",
			"Rec..U.S..RE..Total.Close.end.1.4.Family...000.",
			"Rec..U.S..RE..Revolving.1.4.Family..HE.Lines....000.",
			"Rec..U.S..RE..Construction...Land.Development...000.",
			"Rec..U.S..RE..Multifamily...000.", "Rec..U.S..RE..Commercial...000.",
			"Rec..Commercial...Industrial.Lns.U.S..Addressees...000.",
			"Rec..Commercial...Industrial.Lns.Non.U.S..Address...000.",
			"Rec..Credit.Card.Loans...000.", "Rec..Consumer.Loans...000.",
			"Rec..Total.Lease.Financing.Receivables...000.",
			"Rec..U.S..RE..Farm.Loans...000.",
			"Rec..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000.",
			"Rec..Total.Lns.to.Dep.Institutions...Acceptances...000.",
			"Rec..Agricultural.Production.Loans...000.",
			"Rec..Lns.to.non.U.S..Gov...Official.Institutions...000.",
			"Rec..All.Other.Loans...000.", "U.S..RE..Cl.end.Frst.Lien.1.4...000.",
			"U.S..RE..Cl.end.Jr.Lien.1.4...000.",
			"U.S..RE..Tot.Cl.end.1.4.Family...000.",
			"U.S..RE..Rev.1.4.Fam..HE.Lines....000.",
			"U.S..RE..Constr...Land.Dev...000.", "U.S..RE..Multifamily.Loans...000.",
			"U.S..RE..Comm.RE.Nonfarm.NonRes....000.",
			"Con..Tot.Comm...Ind.Loans...000.", "Con..Credit.Cards...Rel.Plans...000.",
			"Con..Tot.Consumer.Loans...000.", "Con..Total.Leases...000.",
			"Con..Total.Real.Estate.Loans...000.",
			"Con..Loans.to.Depository.Institutions...000.",
			"Con..Agricultural.Prod.Loans...000.",
			"Con..non.U.S..Government.Loans...000.", "Other.Loans...000.",
			"Net.Interest.Income...000.", "Total.Noninterest.Income...000.",
			"NII..Trading.Revenue...000.", "NIE..Salary...Benefits...000.",
			"NIE..Premises...Fixed.Assets...000.", "Total.Noninterest.Expense...000.",
			"Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.",
			"Total.Securities...000.", "Gross.Loans...Leases...000.",
			"Total.Trading.Assets...000.", "Total.Assets...000.",
			"Gain.Realized.Gns.AFS.Secs...000.", "Total.Securities.AFS.BV...000.",
			"Total.AFS.Securities.FV...000.", "U.S..RE..Total.1.4.Fmly...000.",
			"Con..Total.Real.Estate.Loans...000..1",
			"Con..Tot.Comm...Ind.Loans...000..1", "Total.Securities...000..1",
			"Total.AFS.Securities.FV...000..1", "AFS.F..US.Treasury.Secs...000.",
			"AFS.F..Govt.Ag.Secs...000.", "AFS.F..Govt.Spons.Ag...000.",
			"AFS.F...Pass.Through.RMBS..Guar.by.GNMA...000.",
			"AFS.F...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.",
			"AFS.F...Other.RMBS..Issd.or.Guar.by.GSEs...000.",
			"AFS.F..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.F...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.F...Other.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.F..MBS.GNMA..historical....000.",
			"AFS.F..MBS.Sp.Ag..historical....000.",
			"AFS.F..CMOs..FNMA...historical....000.", "Total.Securities.AFS.BV...000..1",
			"AFS.C..US.Treasury.Secs...000.", "AFS.C..Govt.Ag.Secs...000.",
			"AFS.C..Govt.Spons.Ag...000.",
			"AFS.C...Pass.Through.RMBS..Guar.by.GNMA...000.",
			"AFS.C...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.",
			"AFS.C...Other.RMBS..Issd.or.Guar.by.GSEs...000.",
			"AFS.C..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.C...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.C...Other.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.C..MBS.GNMA..historical....000.",
			"AFS.C..MBS.Sp.Ag..historical....000.",
			"AFS.C..CMOs..FNMA...historical....000.", "Total.Reserves...000.",
			"Net.Deferred.Tax.Asset...000.", "Disallowed.Deferred.Taxes...000.",
			"Net.Inc..Noncontrolling.Interests...000.",
			"GRB..Total.Equity.Capital...000.", "Less.Purchase.of.Treasury.Stck...000.",
			"Dividends.on.Preferred.Stock...000.", "Dividends.on.Common.Stock...000.",
			"B3.GRB..Tier.1.Common.Capital..CET1....000.", "Asset.Share")
	
	
	if (!all(.required_colnames_assets %in% colnames(position.data))) {
		stop("Not all required colnames were found in position.data")
	}
	

	
	#create blank revenue forecast time series
	.revenue.forecast.ts <- ts(matrix(NA, ncol = 7, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.revenue.forecast.ts) <- c("Interest.Earning.Assets", "Trading.Assets", "Total.Assets")
	
			#first time series entry of our forecast comes from position data
.revenue.forecast.ts[1, "Interest.Earning.Assets"] <-
		(position.data$Interest.Bearing.Balances...000.
			+ position.data$Tot.Fed.Funds...Reverse.Repos...000.
			+ position.data$Total.Securities...000.
			+ position.data$Gross.Loans...Leases...000.
			+ position.data$Total.Trading.Assets...000.)
.revenue.forecast.ts[1, "Trading.Assets"] <- position.data$Total.Trading.Assets...000.
.revenue.forecast.ts[1, "Total.Assets"] <- position.data$Total.Assets...000.

#remaining entries come from arithmetic on initial time series entry

.nrow = nrow(.revenue.forecast.ts) #for efficient looping
for	(i in 2:.nrow) {
	.revenue.forecast.ts[i,] <- .revenue.forecast.ts[1,]*(1.0125^(i-1))
}


return (.revenue.forecast.ts)
#for testing
#load("c:/ppnr.quant.repo/class_model/data/revenue.forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_ppnr.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#RevenueForecast(assets.forecast, model.coefficients.ppnr, macro.forecasts)