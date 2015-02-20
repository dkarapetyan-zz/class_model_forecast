# TODO: Add comment
# 
# Author: David Karapetyan
###############################################################################


CapitalForecast <- function(ppnr.forecast.data = "ppnr_input.csv", lll.provision.data = "lll_input.csv", afs.securities.data = "afs_input.csv")
{
setwd("c:\\ppnr.quant.repo\\class_model\\data\\")
	#create data variables from inputs
ppnr.data <- read.csv(ppnr.forecast.data, header = TRUE)
lll.data<-read.csv(lll.provision.data, header = TRUE)
afs.data<-read.csv(afs.securities.data, header = TRUE)



#	munging and converting ppnr input into time series, for easy manipulation
	ppnr.breakdown <- ppnr.data$CLASS.Model.Capital.Forecast.Output 
	browser()
	ppnr.data$X <- NULL  
	ppnr.data$CLASS.Model.Capital.Forecast.Output <- NULL
	ppnr.ts <- ts(as.matrix(t(ppnr.data)), start = c(2014,3), end= c(2017,4), frequency=4)
	colnames(ppnr.ts) <- ppnr.breakdown
	
	#	munging and converting LLL provision data input into time series, for easy manipulation
	lll.breakdown <- lll.data$CLASS.Model.Provision.Forecast
	lll.data$X <- NULL  
	lll.data$CLASS.Model.Provision.Forecast <- NULL
	lll.ts<-ts(as.matrix(t(lll.data)), start = c(2014,3), end= c(2016,4), frequency=4)
	colnames(lll.ts) <- lll.breakdown

		#	munging and converting afs provision data input into time series, for easy manipulation
	afs.breakdown <- afs.data$CLASS.Model.Gain.on.AFS.Forecast
	afs.data$X <- NULL  
	afs.data$CLASS.Model.Gain.on.AFS.Forecast <- NULL
	afs.ts<-ts(as.matrix(t(afs.data)), start = c(2014,3), end= c(2017,4), frequency=4)
	colnames(afs.ts) <- afs.breakdown

	#initialize data frame that will contain final output
	capital.forecast.data <- matrix(nrow=8, ncol=8)
	colnames(capital.forecast.data) <- c("Net Income Before Tax", "Allowed DTA","Taxes", "Net.Income","Dividends",
					"Capital","Tier 1 Common Capital", "Leverage Ratio")

	capital.forecast.ts<-ts(as.matrix(capital.forecast.data), start = c(2014,3),
			end = c(2017,4), frequency=4)
	# 

#Fill out row entries of capital forecast using arithmetic operations on ppnr, lll, and afs	
	capital.forecast.ts[,"Net Income Before Tax"] <- ppnr.ts[, "PPNR"]
	- lll.ts[, "Provision" ] + afs.ts[, "Gain AFS Securities"]

browser()


}
CapitalForecast()
