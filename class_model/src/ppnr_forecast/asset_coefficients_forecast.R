# Author: David Karapetyan This Function outputs a ppnr asset
# coefficient forecast from time series input files for bank
# position data, macroeconomic forecasts , and arima model
# coefficients



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
#' result_time_series <- RevenueCoeffForecast(
#' position_data = object_data_frame,
#' model_coefficients = object_data_frame,
#' macro_forecasts = object_time_series)


RevenueCoeffForecast <- function(
    position_data, model_coefficients, macro_forecasts) {
  
  
  .required_colnames_position <- c( "Interest.Bearing.Balances...000.", 
      "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
      "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.",
      "Total.Assets...000.", "Net.Interest.Income...000.",
      "Total.Noninterest.Income...000.", "NII..Trading.Revenue...000.",
      "NIE..Salary...Benefits...000.", "NIE..Premises...Fixed.Assets...000.",
      "Total.Noninterest.Expense...000.", "U.S..RE..Total.1.4.Fmly...000.",
      "Con..Total.Real.Estate.Loans...000.", "Con..Tot.Comm...Ind.Loans...000.",
      "Con..Credit.Cards...Rel.Plans...000.", "Total.Trading.Assets...000.",
      "Total.Securities...000.", "Asset.Share")
  
  .required_colnames_model_coefficients <- c(
      "FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate",
      "HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate",
      "Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE",
      "Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate",
      "Loans.to.Foreign.Governments", "Agriculture",
      "Loans.to.Depository.Institutions", "Other")
  
  .required_colnames_macro <- c( "Real.GDP.growth", "Nominal.GDP.growth", 
      "Real.disposable.income.growth", "Nominal.disposable.income.growth",
      "Unemployment.rate", "CPI.inflation.rate", "X3.Month.Treasury.Yield",
      "X5.year.Treasury.yield", "X10.year.Treasury.yield", "BBB.corporate.yield",
      "Mortgage.rate", "Prime.rate", "Dow.Jones.Total.Stock.Market.Index..Level.",
      "House.Price.Index..Level.", "Commercial.Real.Estate.Price.Index..Level.",
      "Market.Volatility.Index..Level.", "Term.Spread",
      "Annualized.Real.GDP.growth", "Quarterly.change.in.10.year.Treasury.yield",
      "Stock.Market.returns", "Quarterly.change.in.BBB.bond.spread",
      "Quarterly.change.in.BBB.Spread.if.change.is.positive", "Home.price.growth",
      "Commercial.Property.Price.Growth", "Home.price.growth.if.growth.is.negative",
      "Commercial.Property.Price.Growth.Negative",
      "Annualized.Change.in.Unemployment", "Time.trend")
  
  
  if (!all(.required_colnames_position %in% colnames(position_data))) {
    stop("Not all required colnames were found in position_data") }
  
  if (!all(.required_colnames_model_coefficients %in%
          colnames(model_coefficients))) {
    stop("Not all required colnames were found in model_coefficients")
  }
  
  if (!all(.required_colnames_macro %in% colnames(macro_forecasts))) {
    stop("Not all required colnames were found in macro_forecasts")
  }
  
  
  
  # generate process position data, to use in ppnr forecasting
  # artithmetic
  
  .rownames <- c("Residential.RE.Loans.Ratio", "Commercial.RE.Loans.Ratio", 
      "CI.Loans.Ratio", "Credit.Card.Loans.Ratio", "Trading.Assets.Ratio",
      "Securities.Ratio", "Asset.Share")
  
  .colnames <- c("B.S.Ratios", "Net.Interest.Margin",
      "Noninterest.Nontrading.Income.Ratio",
      "Compensation.Noninterest.Expense.Ratio",
      "Fixed.Asset.Noninterest.Expense.Ratio",
      "Other.Noninterest.Expense.Ratio",
      "Return.on.Trading.Assets")
  
  .position_data_processed <- data.frame(matrix(NA, nrow = length(.rownames), 
          ncol = length(.colnames), dimnames = list(.rownames, .colnames)))
  
  
  # fill in remaining columns
  .cols <- length(.colnames)  #for efficiency in looping
  
  for (str in .colnames) {
    if (str == "B.S.Ratios") {
      # assign over all rows except Asset.Share
      .pd_subset1 <- c("U.S..RE..Total.1.4.Fmly...000.", 
          "Con..Total.Real.Estate.Loans...000.", "Con..Tot.Comm...Ind.Loans...000.", 
          "Con..Credit.Cards...Rel.Plans...000.", "Total.Trading.Assets...000.", 
          "Total.Securities...000.")
      
      .pd_subset2 <- c("Interest.Bearing.Balances...000.", 
          "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.", 
          "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
      
      .position_data_processed[!(row.names(.position_data_processed) %in% 
                c("Asset.Share")), "B.S.Ratios"] <-
          as.numeric(position_data[.pd_subset1]/sum(position_data[.pd_subset2]) * 100)
      
      # take care of Asset.Share
      .position_data_processed["Asset.Share", "B.S.Ratios"] <-
          position_data$Asset.Share
    } else {
      # looping portion
      .position_data_processed[str] <- (model_coefficients[.rownames, 
                str] * .position_data_processed$B.S.Ratios)
    }
  }
  
  
  
  cols <- c(
      "Net.Interest.Margin",
      "Noninterest.Nontrading.Income.Ratio", 
      "Compensation.Noninterest.Expense.Ratio",
      "Fixed.Asset.Noninterest.Expense.Ratio",
      "Other.Noninterest.Expense.Ratio",
      "Return.on.Trading.Assets")
  
  # create blank capital forecast time series
  .revenue_coeffs_forecast_df <- data.frame(ts(
          matrix(NA, ncol = length(cols)), 
          start = start(macro_forecasts),
          end = end(macro_forecasts), 
          frequency = frequency(macro_forecasts)))
  
  colnames(.revenue_coeffs_forecast_df) <- cols
  
  # Extracting time series start end and frequency from
  # macro_forecasts. Now convert to data frame for easier
  # manipulation in ensuing computations
  
  if ("mts" %in% class(macro_forecasts)) {
    macro_forecasts <- data.frame(macro_forecasts)
  }
  # first column of our forecast is just our initial input dat
  
  .summing_vec <- c("Interest.Bearing.Balances...000.",
      "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
      "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
  
  .nrows <- nrow(.revenue_coeffs_forecast_df)  #for efficiency in looping
  
  for (i in 1:.nrows) {
    if (i == 1) {
      # initial data for ppnr coefficients.  Comes from arithmetic
      # on position data
      .revenue_coeffs_forecast_df$Net.Interest.Margin[i] <-
          (position_data$Net.Interest.Income...000. /
            sum(position_data[.summing_vec]) * 400)
      
      .revenue_coeffs_forecast_df$Noninterest.Nontrading.Income.Ratio[i] <-
          ((position_data$Total.Noninterest.Income...000. -
              position_data$NII..Trading.Revenue...000.) /
            position_data$Total.Assets...000. * 400)
      
      .revenue_coeffs_forecast_df$Compensation.Noninterest.Expense.Ratio[i] <-
          (position_data$NIE..Salary...Benefits...000. /
            position_data$Total.Assets...000. * 400)
      
      .revenue_coeffs_forecast_df$Fixed.Asset.Noninterest.Expense.Ratio[i] <-
          (position_data$NIE..Premises...Fixed.Assets...000. /
            position_data$Total.Assets...000. * 400)
      
      .revenue_coeffs_forecast_df$Other.Noninterest.Expense.Ratio[i] <-
          (((position_data$Total.Noninterest.Expense...000. -
                sum(position_data[c("NIE..Salary...Benefits...000.",
                            "NIE..Premises...Fixed.Assets...000.")])) / 
              position_data$Total.Assets...000.) * 400)
      
      .revenue_coeffs_forecast_df$Return.on.Trading.Assets[i] <-
          (position_data$NII..Trading.Revenue...000. /
            position_data$Total.Trading.Assets...000. * 400)
    } else {
      # arithmetic
      
      .revenue_coeffs_forecast_df$Net.Interest.Margin[i] <-
          (model_coefficients["Intercept", "Net.Interest.Margin"] +
            model_coefficients["Lagged.dependent.variable", "Net.Interest.Margin"] *
            .revenue_coeffs_forecast_df$Net.Interest.Margin[i - 1] +
            model_coefficients["Term.Spread", "Net.Interest.Margin"] *
            macro_forecasts$Term.Spread[i] +
            model_coefficients["X3.Month.Treasury.Yield", "Net.Interest.Margin"] *
            macro_forecasts$X3.Month.Treasury.Yield[i] +
            model_coefficients["Time.trend", "Net.Interest.Margin"] *
            macro_forecasts$Time.trend[i] +
            sum(.position_data_processed$Net.Interest.Margin))
      
      .cols <- c("Noninterest.Nontrading.Income.Ratio", 
          "Compensation.Noninterest.Expense.Ratio")
      
      .revenue_coeffs_forecast_df[.cols][i, ] <-
          (model_coefficients["Intercept", .cols] +
            model_coefficients["Lagged.dependent.variable", .cols] *
            .revenue_coeffs_forecast_df[.cols][i - 1, ] +
            model_coefficients["Stock.Market.returns", .cols] *
            macro_forecasts$Stock.Market.returns[i] +
            c(sum(.position_data_processed$Noninterest.Nontrading.Income.Ratio),
                sum(.position_data_processed$Compensation.Noninterest.Expense.Ratio)))
      
      .revenue_coeffs_forecast_df$Fixed.Asset.Noninterest.Expense.Ratio[i] <-
          (model_coefficients["Intercept",
                "Fixed.Asset.Noninterest.Expense.Ratio"]
            + model_coefficients["Lagged.dependent.variable",
                "Fixed.Asset.Noninterest.Expense.Ratio"] *
            .revenue_coeffs_forecast_df$Fixed.Asset.Noninterest.Expense.Ratio[i - 1] +
            model_coefficients["Annualized.Change.in.Unemployment",
                "Fixed.Asset.Noninterest.Expense.Ratio"] *
            macro_forecasts$Annualized.Change.in.Unemployment[i] +
            sum(.position_data_processed$Fixed.Asset.Noninterest.Expense.Ratio))
      
      .revenue_coeffs_forecast_df$Other.Noninterest.Expense.Ratio[i] <-
          (model_coefficients["Intercept", "Other.Noninterest.Expense.Ratio"] +
            model_coefficients["Lagged.dependent.variable",
                "Other.Noninterest.Expense.Ratio"] *
            .revenue_coeffs_forecast_df$Other.Noninterest.Expense.Ratio[i - 1] +
            model_coefficients["Quarterly.change.in.BBB.bond.spread",
                "Other.Noninterest.Expense.Ratio"] *
            macro_forecasts$Quarterly.change.in.BBB.bond.spread[i] +
            sum(.position_data_processed$Other.Noninterest.Expense.Ratio))
      
      .revenue_coeffs_forecast_df$Return.on.Trading.Assets[i] <-
          (model_coefficients["Intercept", "Return.on.Trading.Assets"] +
            model_coefficients["Lagged.dependent.variable",
                "Return.on.Trading.Assets"] *
            .revenue_coeffs_forecast_df$Return.on.Trading.Assets[i - 1] +
            model_coefficients["Quarterly.change.in.BBB.bond.spread",
                "Return.on.Trading.Assets"] *
            macro_forecasts$Quarterly.change.in.BBB.bond.spread[i] +
            model_coefficients["Quarterly.change.in.BBB.Spread.if.change.is.positive",
                "Return.on.Trading.Assets"] *
            macro_forecasts$Quarterly.change.in.BBB.Spread.if.change.is.positive[i] +
            sum(.position_data_processed$Return.on.Trading.Assets))
    }
  }
  
  return(.revenue_coeffs_forecast_df)
} 