#' This function outputs an object suitable for plotting in GGplot.
#' 
#' @param book A character string specifying the trading book from which variables are obtained. This 
#'  can be any of the following: \code{c("LLL", "AFS", "Loss", "NCO", "Capital",
#'  "Balance", "PPNR")}
#' @param variable A character string specifying the variable that will be plotted, with respect to time
#' @param bank A character string specifying the bank
#' @param quarter A character string specifying the year and quarter
#' @param nco_data A data frame aggregating U.S. bank NCO data
#' @param ppnr_data A data frame aggregating U.S. bank PPNR data
#' @param total_assets A data frame aggregating U.S. bank total assets
#' @param capital_data A data frame aggregating U.S. bank capital data
#' @param model_coefficients A data frame of arima calibration coefficients computed from past history
#' @param  macro_forecasts A time series of either basic, adverse, or severely
#' adverse macroeconomic forecasts 
#' @return Object of class data.frame
#' @author David Karapetyan
#' @examples
#'PrepareForPlot(
#'"capital",
#'"Net Income",
#'"Adbanc, Inc.",
#'"2014Q3",
#'nco_data,
#'ppnr_data,
#'total_assets,
#'capital_data,
#'model_coefficients_ey,
#'macro_forecasts
#')
#' @export

PrepareForPlot <- function(
    book, variable, bank, quarter, nco_data, ppnr_data,
    total_assets, capital_data, model_coefficients, macro_forecasts) {
  
#safeguard against user error inputting book name
book <- tolower(book) 
  
  .book_list <- list(
      lll = list(
          name = "LLL",
          variable = c("Total.Reserves", "Provision"),
          func = "LLLForecast"),
      ppnr = list(
          name = "PPNR",
          variable = c("Compensation.Noninterest.Expense.Ratio", 
              "Fixed.Asset.Noninterest.Expense.Ratio",
              "Net.Interest.Margin",
              "Noninterest.Nontrading.Income.Ratio",
              "Other.Noninterest.Expense.Ratio",
              "Return.on.Trading.Assets"),
          file = "src/ppnr_forecast/ppnr_forecast.R",
          func = "PPNRForecast"),
      afs = list(
          name = "AFS",
          variable = c("Return.on.AFS.Securities", "Total.AFS.Securities",
              "Gain.AFS.Securities"),
          func = "AFSForecast"),
      loss = list(
          name = "Loss",
          variable = c(
              "4-Qrt Net Charge-offs", "Agriculture", "CI", 
              "Construction.Commercial.Real.Estate", "Credit.Card",
              "FirstLien.Residential.Real.Estate",
              "HELOC.Residential.Real.Estate",
              "Junior.Lien.Residential.Real.Estate", "Leases",
              "Loans.to.Depository.Institutions",
              "Loans.to.Foreign.Governments",
              "Multifamily.Commercial.Real.Estate",
              "NonFarm.NonResidential.CRE", "Other", "Other.Consumer",
              "Other.Real.Estate", "Total Net Charge-offs"),
          func = "LossForecast"),
      capital = list(
          name = "Capital",
          variable = c(
              "Allowed DTA", "Capital", "Dividends",
              "Leverage Ratio", "Net Income",
              "Net Income Before Tax", "Taxes", "Tier 1 Common Capital"), 
          func = "CapitalForecast"),
      nco = list(
          name = "NCO",
          variable = c(
              "FirstLien.Residential.Real.Estate",
              "Junior.Lien.Residential.Real.Estate",
              "HELOC.Residential.Real.Estate",
              "Construction.Commercial.Real.Estate",
              "Multifamily.Commercial.Real.Estate",
              "NonFarm.NonResidential.CRE",
              "Credit.Card", "Other.Consumer", "CI", 
              "Leases", "Other.Real.Estate",
              "Loans.to.Foreign.Governments", "Agriculture",
              "Loans.to.Depository.Institutions", "Other"),
          func = "NCOForecast"),
      asset_coefficients = list(
          name = "Asset Coefficients",
          variable = c(
              "Net.Interest.Margin",
              "Noninterest.Nontrading.Income.Ratio", 
              "Compensation.Noninterest.Expense.Ratio",
              "Fixed.Asset.Noninterest.Expense.Ratio",
              "Other.Noninterest.Expense.Ratio",
              "Return.on.Trading.Assets"),
          func = "RevenueCoeffForecast"))
  
#TODO Refactor prepare_for_plot to use switch

#retrieve appropriate forecasting function, and prepare to graph

  .book <- .book_list[[book]]
  position_data <- PreparePositionData(
      bank, quarter, nco_data, ppnr_data , total_assets, capital_data)
  
  .func <- as.name(.book[["func"]])
  book_zoo <- as.zoo(eval(.func)(
          position_data,
          model_coefficients,
          macro_forecasts))
  book_fortified <- fortify(book_zoo)
  
#get rid of all rows with NAs, so ggplot doesn't give warning
  return(na.omit(book_fortified))
  
}