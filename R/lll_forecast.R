#' This function outputs a time
#' series LLL provision forecast from time series input files
#' for bank position data, macroeconomic forecasts , and Arima
#' model coefficients.
#' 
#' @param position_data  A matrix of a particular bank's
#' present balance sheet
#' @param model_coefficients A matrix of arima calibration
#' coefficients computed from past history
#' @param macro_forecasts A matrix of either basic,
#' adverse, or severely adverse macroeconomic forecasts 
#' @return object of class MTS
#' @author David Karapetyan
#' @export


LLLForecast <- function(position_data, model_coefficients, macro_forecasts) {
  
	# testing of function arguments done in called subroutines
  
  
  # initialize lll forecast
  cols_lll <- c("Total.Net.Charge.offs", "X4.Qrt.Net.Charge.offs", 
      "Total.Reserves...000.", "Provision")
  
  .lll_forecast_df <- data.frame(ts(matrix(NA, ncol = length(cols_lll)), 
          start = start(macro_forecasts),
          end = end(macro_forecasts), 
          frequency = frequency(macro_forecasts)))
  
  colnames(.lll_forecast_df) <- cols_lll
  
  
  
  .lll_forecast_df$Total.Net.Charge.offs[-1] <-
      rowSums(
          LossForecast( position_data, model_coefficients, 
              macro_forecasts)[-1,]) 
  
  
  .row <- nrow(.lll_forecast_df)  #efficiency in looping. 
  # loop. Variables below end one year before end of input data
  # time series
  for (i in 1:(.row - 4)) {
    if (i == 1) {
      .lll_forecast_df$Total.Reserves...000.[i] <-
          position_data$Total.Reserves...000.
    } else {
      .lll_forecast_df$X4.Qrt.Net.Charge.offs[i] <-
          sum(.lll_forecast_df[(i + 1):(i + 4), "Total.Net.Charge.offs"])
      
      .lll_forecast_df$Total.Reserves...000.[i] <-
          if (is.na(
                  .lll_forecast_df$Total.Reserves...000.[i - 1] -
                      .lll_forecast_df$Total.Net.Charge.offs[i] <
              .lll_forecast_df$X4.Qrt.Net.Charge.offs[i]))
          {
            .lll_forecast_df$Total.Reserves...000.[i - 1] 
          }          else if ((.lll_forecast_df$Total.Reserves...000.[i - 1] -
                .lll_forecast_df$Total.Net.Charge.offs[i]) <
              .lll_forecast_df$X4.Qrt.Net.Charge.offs[i]) {
            .lll_forecast_df$X4.Qrt.Net.Charge.offs[i]
          } else if ((.lll_forecast_df$Total.Reserves...000.[i - 1] -
                .lll_forecast_df$Total.Net.Charge.offs[i]) >
              .lll_forecast_df$X4.Qrt.Net.Charge.offs[i] * 2.5) {
            .lll_forecast_df$X4.Qrt.Net.Charge.offs[i] * 2.5          } else {
            .lll_forecast_df$Total.Reserves...000.[i - 1] }
      
      .lll_forecast_df$Provision[i] <-
          (.lll_forecast_df$Total.Reserves...000.[i] +
            .lll_forecast_df$Total.Net.Charge.offs[i] -
            .lll_forecast_df$Total.Reserves...000.[i - 1])    }  }

    #convert NA and NaN to 0. 
    .lll_forecast_df <- replace(.lll_forecast_df, sapply(.lll_forecast_df,
        is.na), 0)

     .lll_forecast_df <- replace(.lll_forecast_df, sapply(.lll_forecast_df,
        is.nan), 0)
 
  return(ts(
          .lll_forecast_df, 
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
} 