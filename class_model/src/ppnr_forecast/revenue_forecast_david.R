# Author: David Karapetyan
###############################################################################
#This Function outputs a time series ppnr balance forecast (15 columns) to 2017 Q4 for
#Interest Earning Assets, Trading Assets, Total Assets, from a input file of position data

RevenueForecast <- function(position.data) {
	.required_colnames_position_data <- 
c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
			"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.",
			"Total.Assets...000")
	
	#create blank revenue forecast time series
	.revenue.forecast.ts <- ts(matrix(NA, ncol = 3, nrow = 14), start=c(2014,3), end=c(2017,4), frequency=4)
	colnames(.revenue.forecast.ts) <- c("Interest.Earning.Assets", "Trading.Assets", "Total.Assets")

	.summing.vec <- c("Interest.Bearing.Balances...000.", "Tot.Fed.Funds...Reverse.Repos...000.", 
			"Total.Securities...000.", "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
	#first time series entry of our forecast comes from position data
	.revenue.forecast.ts[1, "Interest.Earning.Assets"] <- sum(position.data[1, .summing.vec])
	.revenue.forecast.ts[1, "Trading.Assets"] <- position.data[1, "Total.Trading.Assets...000."]
	.revenue.forecast.ts[1, "Total.Assets"] <- position.data[1, "Total.Assets...000."]
	
#remaining entries come from arithmetic on initial time series entry
	
	.nrow = nrow(.revenue.forecast.ts) #for efficient looping
	for	(i in 2:.nrow) {
		.revenue.forecast.ts[i,] <- .revenue.forecast.ts[1,]*(1.0125^(i-1))
	}
	
	
	return (.revenue.forecast.ts)
}
#for testing
#load("c:/ppnr.quant.repo/class_model/data/position_data.RData")

#RevenueForecast(position.data)