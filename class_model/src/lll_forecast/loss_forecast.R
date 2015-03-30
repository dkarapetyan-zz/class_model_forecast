# Author: David Karapetyan This Function outputs an Lll 
# loss forecast from input files for bank position data,
# macroeconomic forecasts , and arima model coefficients

#' @param position_data  A matrix of a particular bank's
#' present balance sheet
#' @param model_coefficients A matrix of arima calibration
#' coefficients computed from past history
#' @param macro_forecasts A matrix of either basic,
#' adverse, or severely adverse macroeconomic forecasts 
#' @return object of class MTS
#' @author David Karapetyan
#' @export 
#' @example
#' result_time_series <- LossForecast(
#' position_data = object_data_frame,
#' model_coefficients = object_data_frame,
#' macro_forecasts = object_time_series)

LossForecast <- function(position_data, model_coefficients, macro_forecasts) {
source("src/lll_forecast/balance_forecast.R")  
source("src/lll_forecast/nco_forecast.R")  
  # initialize model loss forecast time series, which will be
  # used as the input to compute the lll forecast
  cols <- c(
      "FirstLien.Residential.Real.Estate",
      "Junior.Lien.Residential.Real.Estate", "HELOC.Residential.Real.Estate",
      "Construction.Commercial.Real.Estate",
      "Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE",
      "Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate",
      "Loans.to.Foreign.Governments", "Agriculture",
      "Loans.to.Depository.Institutions", "Other")
  
  .model_loss_forecast_df <- data.frame(ts(matrix(NA, ncol = length(cols)), 
          start = start(macro_forecasts), 
          end = end(macro_forecasts), 
          frequency = frequency(macro_forecasts)))
  
  colnames(.model_loss_forecast_df) <- cols
  
  # populate model loss via nco forecast and balance forecast
  # convert time series to data frame, as data frame easier to
  # work with due to $
  .nco_forecast_df <- data.frame(NCOForecast(position_data, 
          model_coefficients, macro_forecasts))
  
  .balance_forecast_df <- data.frame(BalanceForecast(position_data, 
          macro_forecasts))
  
  # first quarter of model loss is empty
  .model_loss_forecast_df[-1, ] <- (
        .nco_forecast_df[-1, ]/400 * .balance_forecast_df[-1, ])

  #convert NA and NaN to 0. 
    .model_loss_forecast_df <- replace(
        .model_loss_forecast_df, sapply(.model_loss_forecast_df,
        is.na), 0)
  
     .model_loss_forecast_df <- replace(
        .model_loss_forecast_df, sapply(.model_loss_forecast_df,
        is.nan), 0)
 
  
  return(ts(
          .model_loss_forecast_df,
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts))) 
}