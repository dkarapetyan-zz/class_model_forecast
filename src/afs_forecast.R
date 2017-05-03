#' This function outputs a time
#' series afs forecast for Leases, Credit Cards, and other
#' variables, from time series input files for bank position
#' data, macroeconomic forecasts , and arima model
#' coefficients.
#' 
#' @param position_data  A data frame of a particular bank's present balance sheet
#' @param model_coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro_forecasts An MTS of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @return object of class MTS
#' @author David Karapetyan
#' @export

AFSForecast <- function(position_data, model_coefficients, macro_forecasts) {
  
  .required_colnames_position <- c("Gain.Realized.Gns.AFS.Secs...000.", 
      "Total.Securities.AFS.BV...000.", "Total.AFS.Securities.FV...000.",
      "AFS.F..US.Treasury.Secs...000.", "AFS.F..Govt.Ag.Secs...000.",
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
  
  .req_cols_mod_coeffs_afs <- c("Return.on.AFS.Securities")
  
  .required_colnames_macro <- c("Quarterly.change.in.10.year.Treasury.yield", 
      "Quarterly.change.in.BBB.Spread.if.change.is.positive")
  
  
  
  if (!all(.required_colnames_position %in% colnames(position_data))) {
    stop("Not all required colnames were found in position.data")
  }
  if (!all(.req_cols_mod_coeffs_afs %in% colnames(model_coefficients))) {
    stop("Not all required colnames were found in model.coefficients")
  }
  if (!all(.required_colnames_macro %in% colnames(macro_forecasts))) {
    stop("Not all required colnames were found in macro.forecasts")
  }
  
  # create afs forecast initial data
  
  
  # create blank capital forecast time series
  cols <- c("Risk.AFS.Ratio", "Return.on.AFS.Securities", "Total.AFS.Securities", 
      "Gain.AFS.Securities")
  
  .afs_forecast_df <- data.frame(ts(matrix(NA, ncol = length(cols)), 
          start = start(macro_forecasts),
          end = end(macro_forecasts), 
          frequency = frequency(macro_forecasts)))
  
  colnames(.afs_forecast_df) <- cols
  
  # first column of our forecast is just our initial input data
  # now, we partition set we are looping over--4 groups of
  # arithmetic
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
  
  # data frames easier to use for computations than time
  # series, due to $ operator
  macro_forecasts_df <- data.frame(macro_forecasts)
  .nrows <- nrow(.afs_forecast_df)  #for efficiency in looping
  
  for (i in 1:.nrows) {    if (i == 1) {
      # initial data for afs assigned here
      .afs_forecast_df$Risk.AFS.Ratio[i] <-          (1 - sum(position_data[.subset1]) /
            sum(position_data[.subset2]))
      
      .afs_forecast_df$Return.on.AFS.Securities[i] <-
          (position_data$Gain.Realized.Gns.AFS.Secs...000. /
            sum(position_data[.subset2])
            * 400)
      
      .afs_forecast_df$Total.AFS.Securities <- sum(position_data[.subset2])
    }
    else {
      # arithmetic
      
      .afs_forecast_df$Return.on.AFS.Securities[i] <-
          (model_coefficients["Intercept", "Return.on.AFS.Securities"] +
            model_coefficients["Lagged.dependent.variable",
                "Return.on.AFS.Securities"] *
            .afs_forecast_df$Return.on.AFS.Securities[i - 1] +
            model_coefficients["Quarterly.change.in.10.year.Treasury.yield",
                "Return.on.AFS.Securities"] *
            macro_forecasts_df$Quarterly.change.in.10.year.Treasury.yield[i] +
            model_coefficients[
                "Quarterly.change.in.BBB.Spread.if.change.is.positive",
                "Return.on.AFS.Securities"] *
            macro_forecasts_df$Quarterly.change.in.BBB.Spread.if.change.is.positive[i])
      
      .afs_forecast_df$Total.AFS.Securities[i] <- (1.0125^(i - 1) *
            .afs_forecast_df$Total.AFS.Securities[1])
      
      .afs_forecast_df$Gain.AFS.Securities[i] <-
          (.afs_forecast_df$Return.on.AFS.Securities[i]/400 *
            .afs_forecast_df$Total.AFS.Securities[i]) }  }
  # get rid of Risk.AFS.Ratios column--just position processed
  # data
  .afs_forecast_df <- .afs_forecast_df[-1]
  
      #convert NA and NaN to 0. 
    .afs_forecast_df <- replace(.afs_forecast_df, sapply(.afs_forecast_df,
        is.na), 0)

    .afs_forecast_df <- replace(.afs_forecast_df, sapply(.afs_forecast_df,
        is.nan), 0)

  
  return(ts(
          .afs_forecast_df,
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
} 
