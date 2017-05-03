#' This function outputs a time
#' series balance forecast from a time series input file for
#' bank position data.
#' 
#' @param position_data A data frame of a particular 
#' bank's present balance sheet
#' @param macro_forecasts An MTS of either basic, adverse, or severely
#' adverse macroeconomic forecasts
#' @return object of class MTS
#' @author David Karapetyan

BalanceForecast <- function(position_data, macro_forecasts) {
  # macro_forecasts input only needed to get appropriate start,
  # end, and frequency for outputted time series
  
  .required_colnames <- c("U.S..RE..Cl.end.Frst.Lien.1.4...000.", 
  "U.S..RE..Cl.end.Jr.Lien.1.4...000.",
  "U.S..RE..Rev.1.4.Fam..HE.Lines....000.", "U.S..RE..Constr...Land.Dev...000.",
  "U.S..RE..Multifamily.Loans...000.",
  "U.S..RE..Comm.RE.Nonfarm.NonRes....000.", "Con..Tot.Comm...Ind.Loans...000.",
  "Con..Credit.Cards...Rel.Plans...000.", "Con..Tot.Consumer.Loans...000.",
  "Con..Total.Leases...000.", "Con..Total.Real.Estate.Loans...000.",
  "Con..Loans.to.Depository.Institutions...000.",
  "Con..Agricultural.Prod.Loans...000.",
  "Con..non.U.S..Government.Loans...000.", "Other.Loans...000.")
  # 
  if (!all(.required_colnames %in% colnames(position_data))) {
    stop("Not all required colnames were found in position.data")
  }
  
  cols <- c("FirstLien.Residential.Real.Estate",
  "Junior.Lien.Residential.Real.Estate", "HELOC.Residential.Real.Estate",
  "Construction.Commercial.Real.Estate", "Multifamily.Commercial.Real.Estate",
  "NonFarm.NonResidential.CRE", "Credit.Card", "Other.Consumer", "CI", "Leases",
  "Other.Real.Estate", "Loans.to.Foreign.Governments", "Agriculture",
  "Loans.to.Depository.Institutions", "Other")
  
  .balance_forecast_df <- data.frame(ts(matrix(NA, ncol = length(cols)), 
          start = start(macro_forecasts),
          end = end(macro_forecasts), 
          frequency = frequency(macro_forecasts)))
  
  colnames(.balance_forecast_df) <- cols
  
  # first column of our forecast is just our initial input data
  .subset1 <- c("U.S..RE..Cl.end.Frst.Lien.1.4...000.",
  "U.S..RE..Cl.end.Jr.Lien.1.4...000.",
  "U.S..RE..Rev.1.4.Fam..HE.Lines....000.", "U.S..RE..Constr...Land.Dev...000.",
  "U.S..RE..Multifamily.Loans...000.",
  "U.S..RE..Comm.RE.Nonfarm.NonRes....000.")
  
  .nrows <- nrow(.balance_forecast_df)  #for efficiency in looping
  
  for (i in 1:.nrows) {
    if (i == 1) {
      .balance_forecast_df[colnames(.balance_forecast_df)[1:6]][i,] <-
          position_data[.subset1]
      
      .balance_forecast_df$Credit.Card[i] <-
          position_data$Con..Credit.Cards...Rel.Plans...000.
      
      .balance_forecast_df$Other.Consumer[i] <-
          (position_data$Con..Tot.Consumer.Loans...000. -
            position_data$Con..Credit.Cards...Rel.Plans...000.)
      
      .balance_forecast_df$CI[i] <-
          position_data$Con..Tot.Comm...Ind.Loans...000.
      
      .balance_forecast_df$Leases[i] <- position_data$Con..Total.Leases...000.
      
      
      .balance_forecast_df$Other.Real.Estate[i] <-
          (position_data$Con..Total.Real.Estate.Loans...000. -
            sum(position_data[.subset1]))
      
      .balance_forecast_df$Loans.to.Foreign.Governments[i] <-
          (position_data$Con..non.U.S..Government.Loans...000.)
      
      .balance_forecast_df$Agriculture[i] <-
          position_data$Con..Agricultural.Prod.Loans...000.
      
      .balance_forecast_df$Loans.to.Depository.Institutions[i] <-
          position_data$Con..Loans.to.Depository.Institutions...000.
      
      .balance_forecast_df$Other[i] <- position_data$Other.Loans...000. }
    else {
      .balance_forecast_df[i, ] <- 1.0125 * .balance_forecast_df[i - 1, ]
    }
  }
 
    #convert NA and NaN to 0. 
    .balance_forecast_df <- replace(
        .balance_forecast_df, sapply(.balance_forecast_df,
        is.na), 0)

    .balance_forecast_df <- replace(
        .balance_forecast_df, sapply(.balance_forecast_df,
        is.nan), 0)

  
  return(ts(.balance_forecast_df, start = start(macro_forecasts), 
          end = end(macro_forecasts), frequency = frequency(macro_forecasts)))
} 