# Author: David Karapetyan This Function outputs a time series ppnr asset
# forecast from time series input files for bank position data.

#' 
#' @param position_data  A matrix of a particular bank's present balance sheet
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' 
#' 

RevenueForecast <- function(position_data) {
	.required_colnames_position_data <- c("Interest.Bearing.Balances...000.",
			"Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
			"Gross.Loans...Leases...000.", "Total.Trading.Assets...000.",
			"Total.Assets...000")
	
	# create blank revenue forecast time series
	.revenue_forecast_ts <- ts(matrix(NA, ncol = 3, nrow = 14), start = c(2014, 3), 
			end = c(2017, 4), frequency = 4)
	
	colnames(.revenue_forecast_ts) <- c("Interest.Earning.Assets",
			"Trading.Assets", "Total.Assets")
	
	.summing_vec <- c("Interest.Bearing.Balances...000.",
			"Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
			"Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
	
	# first time series entry of our forecast comes from position data
	.revenue_forecast_ts[1, "Interest.Earning.Assets"] <- sum(position_data[1, .summing_vec])
	.revenue_forecast_ts[1, "Trading.Assets"] <- position_data[1, "Total.Trading.Assets...000."]
	.revenue_forecast_ts[1, "Total.Assets"] <- position_data[1, "Total.Assets...000."]
	
	# remaining entries come from arithmetic on initial time series entry
	
	.nrow <- nrow(.revenue_forecast_ts)  #for efficient looping
	for (i in 2:.nrow) {
		.revenue_forecast_ts[i, ] <- .revenue_forecast_ts[1, ] * (1.0125^(i - 1))
	}
	
	
	return(.revenue_forecast_ts)
}