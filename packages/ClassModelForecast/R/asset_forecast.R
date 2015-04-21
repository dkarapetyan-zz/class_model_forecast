#' This function outputs a PPNR asset
#' forecast from an input file for bank position data.
#' 
#' @param position_data  A matrix of a particular bank's
#' present balance sheet
#' @param macro_forecasts A matrix of either basic,
#' adverse, or severely adverse macroeconomic forecasts 
#' @return object of class MTS
#' @author David Karapetyan


RevenueForecast <- function(position_data, macro_forecasts) {

	# macro_forecasts input only needed to get appropriate start,
  # end, and frequency for outputted time series
  
  .required_colnames_position_data <- c("Interest.Bearing.Balances...000.", 
      "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
      "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.",
      "Total.Assets...000")
  
  # create blank revenue forecast data frame (easier to work with than time
  # series)
  .cols <- c("Interest.Earning.Assets", "Trading.Assets", "Total.Assets")
  .revenue_forecast_df <- data.frame(ts(
          matrix(NA, ncol = length(.cols),
              dimnames = list(NULL, .cols)),
          start = start(macro_forecasts),
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
  
  .summing_vec <- c("Interest.Bearing.Balances...000.",
      "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
      "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
  # for clarity of inner loop computation
  .nrow <- nrow(.revenue_forecast_df)  #for efficient looping
  
  for (i in 1:.nrow) {
    if (i == 1) {
      # first time series entry of our forecast comes from position
      # data
      .revenue_forecast_df$Interest.Earning.Assets[1] <-
          sum(position_data[.summing_vec])
      .revenue_forecast_df$Trading.Assets[1] <-
          position_data$Total.Trading.Assets...000.
      .revenue_forecast_df$Total.Assets[1] <-
          position_data$Total.Assets...000.
    } else {
      # remaining entries come from arithmetic on initial data
      # frame entry
      .revenue_forecast_df[i, ] <- .revenue_forecast_df[1,] * (1.0125^(i - 1))
      
    }
  }
  
    #convert NA and NaN to 0. 
    .revenue_forecast_df <- replace(
        .revenue_forecast_df, sapply(.revenue_forecast_df,
        is.na),  0)

    .revenue_forecast_df <- replace(
        .revenue_forecast_df, sapply(.revenue_forecast_df,
        is.nan),  0)

  return(ts(
          .revenue_forecast_df,
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
} 
