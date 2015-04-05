# Author: David Karapetyan

# This Function outputs a time series NCO forecast from time
# series input files for bank position data, macroeconomic
# forecasts , and arima model coefficients

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
#' result_time_series <- NcOForecast(
#' position_data = object_data_frame,
#' model_coefficients = object_data_frame,
#' macro_forecasts = object_time_series)

NCOForecast <- function(position_data, model_coefficients, macro_forecasts) {
  
  .required_colnames_position <- c("Interest.Bearing.Balances...000.", 
    "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.", 
    "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.", 
    "Total.Assets...000.", "Net.Interest.Income...000.", 
    "Total.Noninterest.Income...000.", "NII..Trading.Revenue...000.", 
    "NIE..Salary...Benefits...000.", "NIE..Premises...Fixed.Assets...000.", 
    "Total.Noninterest.Expense...000.", "U.S..RE..Total.1.4.Fmly...000.", 
    "Con..Total.Real.Estate.Loans...000.", "Con..Tot.Comm...Ind.Loans...000.", 
    "Con..Credit.Cards...Rel.Plans...000.", "Total.Trading.Assets...000.", 
    "Total.Securities...000.", "Asset.Share")
  
  # 
  .req_cols_mod_coeffs_nco <- c("FirstLien.Residential.Real.Estate", 
    "Junior.Lien.Residential.Real.Estate", "HELOC.Residential.Real.Estate", 
    "Construction.Commercial.Real.Estate", "Multifamily.Commercial.Real.Estate", 
    "NonFarm.NonResidential.CRE", "Credit.Card", "Other.Consumer", 
    "CI", "Leases", "Other.Real.Estate", "Loans.to.Foreign.Governments", 
    "Agriculture", "Loans.to.Depository.Institutions", "Other")
  
.required_colnames_macro <- c(
    "Home.price.growth", "Commercial.Property.Price.Growth",
    "Home.price.growth.if.growth.is.negative",
    "Commercial.Property.Price.Growth.Negative",
    "Annualized.Change.in.Unemployment", "Time.trend")
  
  
  if (!all(.required_colnames_position %in% colnames(position_data))) {
    stop("Not all required colnames were found in position.data")
  }
  if (!all(.req_cols_mod_coeffs_nco %in% colnames(model_coefficients))) {
    stop("Not all required colnames were found in model.coefficients")
  }
  if (!all(.required_colnames_macro %in% colnames(macro_forecasts))) {
    stop("Not all required colnames were found in macro.forecasts")
  }
  # TODO write test that checks that inputs have the same
  # columns, arranged in the same order
  
  
  cols <- c(
      "FirstLien.Residential.Real.Estate",
      "Junior.Lien.Residential.Real.Estate", "HELOC.Residential.Real.Estate",
      "Construction.Commercial.Real.Estate",
      "Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE",
      "Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate",
      "Loans.to.Foreign.Governments", "Agriculture",
      "Loans.to.Depository.Institutions", "Other")
  
  
  # create blank capital forecast time series
  .nco_forecast_df <- data.frame(ts(matrix(NA, ncol = length(cols)), 
    start = start(macro_forecasts),
    end = end(macro_forecasts), 
    frequency = frequency(macro_forecasts)))
  
  colnames(.nco_forecast_df) <- cols
  # first column of our forecast is just our initial input data
  
  .nrows <- nrow(.nco_forecast_df)  #for efficiency in looping
  # now, we partition set we are looping over--4 groups of
  # arithmetic
  .subset1 <- c("FirstLien.Residential.Real.Estate",
      "Junior.Lien.Residential.Real.Estate", "HELOC.Residential.Real.Estate")
  .subset2 <- c("Credit.Card", "CI", "Leases", "Loans.to.Foreign.Governments", 
      "Agriculture", "Loans.to.Depository.Institutions", "Other")
  .subset3 <- "Other.Consumer"
  .subset4 <- "Other.Real.Estate"
  .subset5 <- c("Construction.Commercial.Real.Estate",
      "Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE")
  
  for (i in 1:.nrows) {
    if (i == 1) {
      # initial data for nco assigned here
      .nco_forecast_df$FirstLien.Residential.Real.Estate[i] <-
      ((position_data$CO..U.S..RE..Close.end.First.Lien.1.4.Family...000. -
      position_data$Rec..U.S..RE..Close.end.First.Lien.1.4.Family...000.) /
      position_data$U.S..RE..Cl.end.Frst.Lien.1.4...000. * 400)
      
      .nco_forecast_df$Junior.Lien.Residential.Real.Estate[i] <-
      ((position_data$CO..U.S..RE..Close.end.Jr.Lien.1.4.Family...000. -
      position_data$Rec..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.) /
      position_data$U.S..RE..Cl.end.Jr.Lien.1.4...000. * 400)
      
      .nco_forecast_df$HELOC.Residential.Real.Estate[i] <-
      ((position_data$CO..U.S..RE..Revolving.1.4.Family..HE.Lines....000. -
      position_data$Rec..U.S..RE..Revolving.1.4.Family..HE.Lines....000.) /
      position_data$U.S..RE..Rev.1.4.Fam..HE.Lines....000. * 400)
      
      .nco_forecast_df$Construction.Commercial.Real.Estate[i] <-
      ((position_data$CO..U.S..RE..Construction...Land.Development...000. -
      position_data$Rec..U.S..RE..Construction...Land.Development...000.) /
      position_data$U.S..RE..Constr...Land.Dev...000. * 400)
      
      .nco_forecast_df$Multifamily.Commercial.Real.Estate[i] <-
      ((position_data$CO..U.S..RE..Multifamily...000. -
      position_data$Rec..U.S..RE..Multifamily...000.) /
      position_data$U.S..RE..Multifamily.Loans...000. * 400)
      
      .nco_forecast_df$NonFarm.NonResidential.CRE[i] <-
      ((position_data$CO..U.S..RE..Commercial...000. -
      position_data$Rec..U.S..RE..Commercial...000.) /
      position_data$U.S..RE..Comm.RE.Nonfarm.NonRes....000. * 400)
      
      .nco_forecast_df$Credit.Card[i] <-
      ((position_data$CO..Credit.Card.Loans...000. -
      position_data$Rec..Credit.Card.Loans...000.) /
      position_data$Con..Credit.Cards...Rel.Plans...000. * 400)
      
      .nco_forecast_df$Other.Consumer[i] <-
      (((position_data$CO..Consumer.Loans...000. -
      position_data$CO..Credit.Card.Loans...000.) -
      (position_data$Rec..Consumer.Loans...000. -
      position_data$Rec..Credit.Card.Loans...000.)) /
      (position_data$Con..Tot.Consumer.Loans...000. -
      position_data$Con..Credit.Cards...Rel.Plans...000.) * 400)
      
      .nco_forecast_df$CI[i] <-
      (((position_data$CO..Commercial...Industrial.Lns.U.S..Addressees...000. +
      position_data$CO..Commercial...Industrial.Lns.Non.U.S..Address...000.) -
      (position_data$Rec..Commercial...Industrial.Lns.U.S..Addressees...000. +
      position_data$Rec..Commercial...Industrial.Lns.Non.U.S..Address...000.)) /
      position_data$Con..Tot.Comm...Ind.Loans...000. * 400)
      
      
      .nco_forecast_df$Leases[i] <-
      ((position_data$CO..Total.Lease.Financing.Receivables...000. -
      position_data$Rec..Total.Lease.Financing.Receivables...000.) /
      position_data$Con..Total.Leases...000. * 400)
      
.cols <- c("U.S..RE..Cl.end.Frst.Lien.1.4...000.", 
    "U.S..RE..Cl.end.Jr.Lien.1.4...000.",
    "U.S..RE..Rev.1.4.Fam..HE.Lines....000.",
    "U.S..RE..Constr...Land.Dev...000.", "U.S..RE..Multifamily.Loans...000.",
    "U.S..RE..Comm.RE.Nonfarm.NonRes....000.")
      
      .nco_forecast_df$Other.Real.Estate[i] <-
      (((position_data$CO..U.S..RE..Farm.Loans...000. +
      position_data$CO..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000.) -
      (position_data$Rec..U.S..RE..Farm.Loans...000. +
      position_data$Rec..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000.)) /
      (position_data$Con..Total.Real.Estate.Loans...000. -
      sum(position_data[.cols])) * 400)
      
      .nco_forecast_df$Loans.to.Foreign.Governments[i] <-
      ((position_data$CO..Lns.to.non.U.S..Gov...Official.Institutions...000. -
      position_data$Rec..Lns.to.non.U.S..Gov...Official.Institutions...000.) /
      position_data$Con..non.U.S..Government.Loans...000. * 400)
      
      .nco_forecast_df$Agriculture[i] <-
      ((position_data$CO..Agricultural.Production.Loans...000. -
      position_data$Rec..Agricultural.Production.Loans...000.) /
      position_data$Con..Agricultural.Prod.Loans...000. * 400)
      
      .nco_forecast_df$Loans.to.Depository.Institutions[i] <-
      ((position_data$CO..Total.Lns.to.Dep.Institutions...Acceptances...000. -
      position_data$Rec..Total.Lns.to.Dep.Institutions...Acceptances...000.) /
      position_data$Con..Loans.to.Depository.Institutions...000. * 400)
      
      .nco_forecast_df$Other[i] <- ((position_data$CO..All.Other.Loans...000. - 
      position_data$Rec..All.Other.Loans...000.) /
      position_data$Other.Loans...000. * 400)
      
    } else {
      # arithmetic
      
      
      .nco_forecast_df[i, .subset1] <- (model_coefficients["Intercept", 
      .subset1] + model_coefficients["Lagged.dependent.variable", .subset1] *
      .nco_forecast_df[(i - 1), .subset1] +
      model_coefficients["Home.price.growth", .subset1] * macro_forecasts[i,
      "Home.price.growth"] +
      model_coefficients["Home.price.growth.if.growth.is.negative", .subset1] *
      macro_forecasts[i, "Home.price.growth.if.growth.is.negative"])
      
      .nco_forecast_df[i, .subset2] <- (model_coefficients["Intercept", 
      .subset2] + model_coefficients["Lagged.dependent.variable", .subset2] *
      .nco_forecast_df[(i - 1), .subset2] +
      model_coefficients["Annualized.Change.in.Unemployment", .subset2] *
      macro_forecasts[i, "Annualized.Change.in.Unemployment"])
      
      .nco_forecast_df[i, .subset3] <- (model_coefficients["Intercept", 
      .subset3] + model_coefficients["Lagged.dependent.variable", .subset3] *
      .nco_forecast_df[(i - 1), .subset3] +
      model_coefficients["Annualized.Change.in.Unemployment", .subset3] *
      macro_forecasts[i, "Annualized.Change.in.Unemployment"] +
      model_coefficients["Time.trend", .subset3] * macro_forecasts[i,
      "Time.trend"])
      
      .nco_forecast_df[i, .subset4] <- (model_coefficients["Intercept", 
      .subset4] + model_coefficients["Lagged.dependent.variable", .subset4] *
      .nco_forecast_df[(i - 1), .subset4] +
      model_coefficients["Commercial.Property.Price.Growth", .subset4] *
      macro_forecasts[i, "Commercial.Property.Price.Growth"] +
      model_coefficients["Commercial.Property.Price.Growth.Negative", .subset4]
      * macro_forecasts[i, "Commercial.Property.Price.Growth.Negative"])
      
      .nco_forecast_df[i, .subset5] <- (model_coefficients["Intercept", 
      .subset5] + model_coefficients["Lagged.dependent.variable", .subset5] *
      .nco_forecast_df[(i - 1), .subset5] +
      model_coefficients["Commercial.Property.Price.Growth.Negative", .subset5]
      * macro_forecasts[i, "Commercial.Property.Price.Growth.Negative"])
      
      # TODO write test that checks if model coefficients and macro
      # forecasts names should be identical.  For BAC, are not for
      # 'Commercial.Property.Price.Growth.Negative' and
      # 'Home.price.growth.if.growth.is.negative'
      
    }
  }
    #convert NA and NaN to 0. 
    .nco_forecast_df <- replace(.nco_forecast_df, sapply(.nco_forecast_df,
        is.na), 0)

    .nco_forecast_df <- replace(.nco_forecast_df, sapply(.nco_forecast_df,
        is.nan), 0)


  return(ts(.nco_forecast_df,
          start = start(macro_forecasts), 
          end = end(macro_forecasts),
          frequency = frequency(macro_forecasts)))
} 
