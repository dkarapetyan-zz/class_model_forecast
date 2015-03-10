# Author: David Karapetyan This Function outputs a PPNR forecast from
# input files for bank position data, macroeconomic forecasts , and
# arima model coefficients

#' @param position_data  A data frame of a particular bank's present balance sheet
#' @param model_coefficients A data frame of arima calibration coefficients computed from past history
#' @param macro_forecasts A time series of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' @export


PPNRForecast <- function(position_data, model_coefficients, macro_forecasts) {
  
  source("src/ppnr_forecast/asset_coefficients_forecast.R")
  source("src/ppnr_forecast/asset_forecast.R")
  
  # testing for inputs done in subroutines
  .revenue_forecast_df <- data.frame(RevenueForecast(position_data, macro_forecasts)) #df easier to work with
  .revenue_coeffs_forecast_df <- data.frame(RevenueCoeffForecast(
          position_data,
          model_coefficients, 
          macro_forecasts
      ))
  
  
  cols <- c("Net.Interest.Income",
      "Non.Int...Non.Trade.Income", "Trading.Income", "Compensation.Exp",
      "Fixed.Asset.Exp", "Other.Exp", "PPNR")
  
  # create blank ppnr capital forecast time series
  .ppnr_forecast_df <- data.frame(ts(
          matrix(NA, ncol = length(cols)),
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
  
  colnames(.ppnr_forecast_df) <- cols
  
  # first column of our forecast is blank. Set up temporary object without first row,
  #populate it, then assign it to ppnr_forecast's 2:end rows
  
  .ppnr_forecast_df_temp <- .ppnr_forecast_df[-1,]
  .revenue_forecast_df_temp <- .revenue_forecast_df[-1,]
  .revenue_coeffs_forecast_df_temp <- .revenue_coeffs_forecast_df[-1,]
  
  .ppnr_forecast_df_temp$Net.Interest.Income <- (
        .revenue_forecast_df_temp$Interest.Earning.Assets/400
        * .revenue_coeffs_forecast_df_temp$Net.Interest.Margin)
  
  .ppnr_forecast_df_temp$Non.Int...Non.Trade.Income <- (.revenue_forecast_df_temp$Total.Assets/400
        * .revenue_coeffs_forecast_df_temp$Noninterest.Nontrading.Income.Ratio)
  
  .ppnr_forecast_df_temp$Trading.Income <- (
        .revenue_coeffs_forecast_df_temp$Return.on.Trading.Assets/400
        * .revenue_forecast_df_temp$Trading.Assets)
  
  .ppnr_forecast_df_temp$Compensation.Exp <- (
        .revenue_forecast_df_temp$Total.Assets/400
        * .revenue_coeffs_forecast_df_temp$Compensation.Noninterest.Expense.Ratio)
  
  .ppnr_forecast_df_temp$Fixed.Asset.Exp <- (
        .revenue_coeffs_forecast_df_temp$Fixed.Asset.Noninterest.Expense.Ratio/400
        * .revenue_forecast_df_temp$Total.Assets)
  
  .ppnr_forecast_df_temp$Other.Exp <- (
        .revenue_coeffs_forecast_df_temp$Other.Noninterest.Expense.Ratio/400
        * .revenue_forecast_df_temp$Total.Assets)
  
  .cols1 <- c("Net.Interest.Income", "Non.Int...Non.Trade.Income", "Trading.Income")
  .cols2 <- c("Compensation.Exp", "Fixed.Asset.Exp", "Other.Exp")
  
  .ppnr_forecast_df_temp$PPNR <- (rowSums(.ppnr_forecast_df_temp[, .cols1])
        - rowSums(.ppnr_forecast_df_temp[, .cols2]))
  
  #assign values from temp variable to .ppnr_forecast_df, modulo first row
  .ppnr_forecast_df[-1,] <- .ppnr_forecast_df_temp
  return(ts(
          .ppnr_forecast_df,
          start = start(macro_forecasts),
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
}