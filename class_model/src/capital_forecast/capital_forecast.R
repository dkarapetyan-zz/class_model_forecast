# Author: David Karapetyan This Function outputs a time
# series capital forecast from time series inputs PPNR, AFS
# and LLL forecasts


#' @param position_data A data frame of a particular 
#' bank's present balance sheet
#' @param model_coefficients A data frame of arima calibration
#' coefficients computed from past history
#' @param macro_forecasts A time series of either basic, adverse, or severely
#' adverse macroeconomic forecasts#'
#' @return object of class MTS
#' @author David Karapetyan
#' @export
#' @example
#' result_time_series <- CapitalForecast(
#' position_data = object_data_frame,
#' model_coefficients = object_data_frame,
#' macro_forecasts = object_time_series)

CapitalForecast <-
    function(position_data, model_coefficients, macro_forecasts) {
  source("src/afs_forecast/afs_forecast.R")
  source("src/ppnr_forecast/ppnr_forecast.R")
  source("src/lll_forecast/lll_forecast.R")
  
  
  # testing of inputs done in subroutines called by function
  
  
  # create blank capital forecast time series
  cols <- c(      "Net.Income.Before.Tax", "Allowed.DTA", "Taxes",       "Net.Income", "Dividends", "Capital", "Tier.1.Common.Capital",
      "Leverage.Ratio")
  
  .capital_forecast_df <- data.frame(ts(
          matrix(NA, ncol = length(cols)), 
          start = start(macro_forecasts),
          end = end(macro_forecasts), 
          frequency = frequency(macro_forecasts)))
  
  colnames(.capital_forecast_df) <- cols
  
  
  # Fill out row entries of capital forecast using arithmetic
  # operations on ppnr, lll, and afs
  ppnr <- data.frame(PPNRForecast(position_data, model_coefficients, 
          macro_forecasts))
  lll <- data.frame(LLLForecast(position_data, model_coefficients, 
          macro_forecasts))
  afs <- data.frame(AFSForecast(position_data, model_coefficients, 
          macro_forecasts))
  .row <- nrow(.capital_forecast_df)  #efficiency in looping. 
  for (i in 1:.row) {
    if (i == 1) {
      if (is.na(position_data$Net.Deferred.Tax.Asset...000. - 
              position_data$Disallowed.Deferred.Taxes...000.)) {
        .capital_forecast_df$Allowed.DTA[i] <-
            position_data$Net.Deferred.Tax.Asset...000.
      } else {
        .capital_forecast_df$Allowed.DTA[i] <-
            (position_data$Net.Deferred.Tax.Asset...000.
              - position_data$Disallowed.Deferred.Taxes...000.)
      }
      
      .capital_forecast_df$Dividends[i] <-
          sum(position_data[c(
                      "Less.Purchase.of.Treasury.Stck...000.", 
                      "Dividends.on.Preferred.Stock...000.",
                      "Dividends.on.Common.Stock...000.")])
      
      .capital_forecast_df$Capital[i] <-
          position_data$GRB..Total.Equity.Capital...000.
      
      .capital_forecast_df$Tier.1.Common.Capital[i] <-
          position_data$B3.GRB..Tier.1.Common.Capital..CET1....000.
      
      .capital_forecast_df$Leverage.Ratio[i] <-
          (position_data$B3.GRB..Tier.1.Common.Capital..CET1....000.
            / position_data$Total.Assets...000.)
      
    } else {
      .capital_forecast_df$Net.Income.Before.Tax[i] <- (
            ppnr$PPNR[i]
            - lll$Provision[i] + afs$Gain.AFS.Securities[i])
      
      .capital_forecast_df$Taxes[i] <-
          max(0.35 * .capital_forecast_df$Net.Income.Before.Tax[i], 
              max(0.1 * .capital_forecast_df$Tier.1.Common.Capital[i - 1]
                      - .capital_forecast_df$Allowed.DTA[i - 1], 0))
      
      .capital_forecast_df$Allowed.DTA[i] <-
          (min(.capital_forecast_df$Taxes[i], 0)
            + .capital_forecast_df$Allowed.DTA[i - 1])
      
      .capital_forecast_df$Net.Income[i] <-
          (.capital_forecast_df$Net.Income.Before.Tax[i]
            - .capital_forecast_df$Taxes[i])
      
      .capital_forecast_df$Dividends[i] <-
          (max(0.9 * .capital_forecast_df$Dividends[i - 1]
                    + (0.1) * (0.45 * .capital_forecast_df$Net.Income[i]
                      - .capital_forecast_df$Dividends[i - 1]), 0))
      
      .capital_forecast_df$Capital[i] <-
          (.capital_forecast_df$Capital[i - 1]
            + .capital_forecast_df$Net.Income[i]
            + .capital_forecast_df$Dividends[i])
      
      .capital_forecast_df$Tier.1.Common.Capital[i] <-
          (.capital_forecast_df$Capital[i]
            - .capital_forecast_df$Capital[1]
            + .capital_forecast_df$Tier.1.Common.Capital[1])
      
      .capital_forecast_df$Leverage.Ratio[i] <-
          (.capital_forecast_df$Tier.1.Common.Capital[i]
            / (position_data$Total.Assets...000.
              * (1 + (0.0125 * (i - 1)))))
    }
  }
  return(ts(
          .capital_forecast_df,
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
} 
