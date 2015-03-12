# Author: David Karapetyan This Function outputs a time series lll provision
# forecast from time series input files for bank position data, macroeconomic
# forecasts , and arima model coefficients

#' @param position_data  A matrix of a particular bank's present balance sheet
#' @param model_coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro_forecasts A matrix of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' @export



LllForecast <- function(position_data, model_coefficients, macro_forecasts) {
  source("src/lll_forecast/nco_forecast.R")
  source("src/lll_forecast/balance_forecast.R")
  
  # testing of function arguments done in called subroutines
  
  # initialize model loss forecast time series, which will be used as the input to
  # compute the lll forecast
  cols <- c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
      "HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", "Multifamily.Commercial.Real.Estate", 
      "NonFarm.NonResidential.CRE", "Credit.Card", "Other.Consumer", "CI", "Leases", 
      "Other.Real.Estate", "Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
      "Other")
  .model_loss_forecast_df <- data.frame(ts(
      matrix(NA, ncol = length(cols)),
      start = start(macro_forecasts),
      end = end(macro_forecasts),
      frequency = frequency(macro_forecasts)))
  colnames(.model_loss_forecast_df) <- cols
  
  # populate model loss via nco forecast and balance forecast
  #convert time series to data frame, as data frame easier to work with due to $
  .nco_forecast_df <- data.frame(NCOForecast(position_data, model_coefficients, macro_forecasts))
  .balance_forecast_df <- data.frame(BalanceForecast(position_data, macro_forecasts))
  
  #first quarter of model loss is empty
  .model_loss_forecast_df[-1, ] <- .nco_forecast_df[-1, ]/400 * .balance_forecast_df[-1, ]
  
  # initialize lll forecast
  cols_lll <- c("Total.Net.Charge.offs", "X4.Qrt.Net.Charge.offs", 
      "Total.Reserves...000.", "Provision")
  
  .lll_forecast_df <- data.frame(ts(matrix(
              NA,
              ncol = length(cols_lll)),
          start = start(macro_forecasts),
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
  
  colnames(.lll_forecast_df) <- cols_lll
  
  
  
  .lll_forecast_df$Total.Net.Charge.offs[-1] <- rowSums(.model_loss_forecast_df[-1, ])
  
  
#    for (i in 2:(.row)) {
#            .lll_forecast_df$Total.Net.Charge.offs[i] <- sum(.model_loss_forecast_df[i, ])
#        }
  
  
  
.row <- nrow(.lll_forecast_df)  #efficiency in looping. 
  # loop. Variables below end one year before end of input data time series
  for (i in 1:(.row - 4)) {
    if (i == 1) {
      .lll_forecast_df$Total.Reserves...000.[i] <- position_data$Total.Reserves...000.
    }
    else {
      .lll_forecast_df$X4.Qrt.Net.Charge.offs[i] <-
          sum(.lll_forecast_df[(i + 1):(i + 4), "Total.Net.Charge.offs"])
      
      .lll_forecast_df$Total.Reserves...000.[i] <-
          if ((.lll_forecast_df$Total.Reserves...000.[i-1]
                - .lll_forecast_df$Total.Net.Charge.offs[i])
              < .lll_forecast_df$X4.Qrt.Net.Charge.offs[i]) {
            .lll_forecast_df$X4.Qrt.Net.Charge.offs[i]
          } else if ((.lll_forecast_df$Total.Reserves...000.[i-1]
                - .lll_forecast_df$Total.Net.Charge.offs[i])
              > .lll_forecast_df$X4.Qrt.Net.Charge.offs[i] * 2.5) {
            .lll_forecast_df$X4.Qrt.Net.Charge.offs[i] * 2.5
          } else {
            .lll_forecast_df$Total.Reserves...000.[i-1]
          }
      
      .lll_forecast_df$Provision[i] <- (.lll_forecast_df$Total.Reserves...000.[i]
            + .lll_forecast_df$Total.Net.Charge.offs[i]
            - .lll_forecast_df$Total.Reserves...000.[i-1])
    }
  } 
  
  return(ts(
          .lll_forecast_df,
          start = start(macro_forecasts),
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
} 