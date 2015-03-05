# Author: David Karapetyan
###############################################################################
#This Function outputs a time series afs forecast for Leases, Credit Cards,
#and other variables, from time series input files for bank position data, macroeconomic forecasts
#, and arima model coefficients 


#' 
#' @param position.data  A matrix of a particular bank's present balance sheet
#' @param model.coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro.forecasts A matrix of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' @export
#' 
#'

AFSForecast <- function(position.data, model.coefficients, macro.forecasts) {
	
	.required_colnames_position <-
			c("Gain.Realized.Gns.AFS.Secs...000.", "Total.Securities.AFS.BV...000.", 
					"Total.AFS.Securities.FV...000.", "AFS.F..US.Treasury.Secs...000.",
					"AFS.F..Govt.Ag.Secs...000.", "AFS.F..Govt.Spons.Ag...000.",
					"AFS.F...Pass.Through.RMBS..Guar.by.GNMA...000.",
					"AFS.F...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.",
					"AFS.F...Other.RMBS..Issd.or.Guar.by.GSEs...000.",
					"AFS.F..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.",
					"AFS.F...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.",
					"AFS.F...Other.CMBS.Issd.or.Guar.by.GSEs...000.",
					"AFS.C..US.Treasury.Secs...000.", "AFS.C..Govt.Ag.Secs...000.",
					"AFS.C..Govt.Spons.Ag...000.", "AFS.C...Pass.Through.RMBS..Guar.by.GNMA...000.",
					"AFS.C...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.",
					"AFS.C...Other.RMBS..Issd.or.Guar.by.GSEs...000.",
					"AFS.C..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.",
					"AFS.C...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.",
					"AFS.C...Other.CMBS.Issd.or.Guar.by.GSEs...000."
			)
	
	.req_cols_mod_coeffs_afs <- c("Return.on.AFS.Securities") 
	
	.required_colnames_macro <-
			c("Quarterly.change.in.10.year.Treasury.yield",
					"Quarterly.change.in.BBB.Spread.if.change.is.positive")
	
	
	
	if (!all(.required_colnames_position %in% colnames(position.data))) {
		stop("Not all required colnames were found in position.data")
	}
	if (!all(.req_cols_mod_coeffs_afs %in% colnames(model.coefficients))) {
		stop("Not all required colnames were found in model.coefficients")
	}
	if (!all(.required_colnames_macro %in% colnames(macro.forecasts))) {
		stop("Not all required colnames were found in macro.forecasts")
	}
	
	#create afs forecast initial data
	
	
	#create blank capital forecast time series
	.afs.forecast.ts <- ts(matrix(NA, ncol = 4, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.afs.forecast.ts) <- c("Risk.AFS.Ratio", "Return.on.AFS.Securities", 
			"Total.AFS.Securities", "Gain.AFS.Securities")
	
	#first column of our forecast is just our initial input data
	.nrows<-nrow(.afs.forecast.ts) #for efficiency in looping
	#now, we partition set we are looping over--4 groups of arithmetic
	.subset1 <- c("AFS.F..US.Treasury.Secs...000.", "AFS.F..Govt.Ag.Secs...000.", 
			"AFS.F..Govt.Spons.Ag...000.",
			"AFS.F...Pass.Through.RMBS..Guar.by.GNMA...000.",
			"AFS.F...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.",
			"AFS.F...Other.RMBS..Issd.or.Guar.by.GSEs...000.",
			"AFS.F..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.F...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.F...Other.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.C..US.Treasury.Secs...000.", "AFS.C..Govt.Ag.Secs...000.",
			"AFS.C..Govt.Spons.Ag...000.",
			"AFS.C...Pass.Through.RMBS..Guar.by.GNMA...000.",
			"AFS.C...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.",
			"AFS.C...Other.RMBS..Issd.or.Guar.by.GSEs...000.",
			"AFS.C..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.C...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.",
			"AFS.C...Other.CMBS.Issd.or.Guar.by.GSEs...000.")
	
	.subset2 <- c("Total.Securities.AFS.BV...000.", "Total.AFS.Securities.FV...000.")
	
	for(i in 1:.nrows) {
		if (i==1) { #initial data for afs assigned here
			.afs.forecast.ts[i,"Risk.AFS.Ratio"] <- (
						1 - sum(position.data[1,.subset1])/sum(position.data[1,.subset2])
						)
			
			.afs.forecast.ts[i, "Return.on.AFS.Securities"] <-
					position.data[1, "Gain.Realized.Gns.AFS.Secs...000."] / sum(position.data[1,.subset2]) * 400
			
			.afs.forecast.ts[i, "Total.AFS.Securities"] <- sum(position.data[1,.subset2])
		}
		else { # arithmetic
			
			
			.afs.forecast.ts[i, "Return.on.AFS.Securities"] <- (
						model.coefficients["Intercept", "Return.on.AFS.Securities"]
						+ model.coefficients["Lagged.dependent.variable", "Return.on.AFS.Securities"]
						*.afs.forecast.ts[(i-1), "Return.on.AFS.Securities"]
						+ model.coefficients["Quarterly.change.in.10.year.Treasury.yield",
								"Return.on.AFS.Securities"]
						*macro.forecasts[i,"Quarterly.change.in.10.year.Treasury.yield"]
						+ model.coefficients["Quarterly.change.in.BBB.Spread.if.change.is.positive",
								"Return.on.AFS.Securities"]
						*macro.forecasts[i,"Quarterly.change.in.BBB.Spread.if.change.is.positive"]
						)  
			
			.afs.forecast.ts[i, "Total.AFS.Securities"] <- (
						1.0125^(i-1) * .afs.forecast.ts[1, "Total.AFS.Securities"] 
						)
			
			.afs.forecast.ts[i, "Gain.AFS.Securities"] <- (
						.afs.forecast.ts[i, "Return.on.AFS.Securities"]/400 * 	
						.afs.forecast.ts[i, "Total.AFS.Securities"]
						)
		}
	}
	
	return (.afs.forecast.ts)
}

#for testing
#load("c:/ppnr.quant.repo/class_model/data/afs_forecasts_input.RData")
#load("c:/ppnr.quant.repo/class_model/data/model_coefficients_afs.RData")
#load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")

#AFSForecast(afs.forecast.initial, model.coefficients.afs, macro.forecasts)