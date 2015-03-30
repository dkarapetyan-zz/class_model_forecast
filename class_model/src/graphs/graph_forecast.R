# Author: David Karapetyan This Function outputs a graph of an input forecast 

#' @param book Portion of trading book from which variables are obtained. This 
#'  can be any of the following \code{c("lll", "afs", "loss", "capital",
#'  "balance", "ppnr")}
#' @param variable Variable that will be plotted, with respect to time
#' @param position_data A data frame of a particular 
#'  bank's present balance sheet
#' @param model_coefficients A data frame of arima calibration
#' coefficients computed from past history
#' @param  macro_forecasts A time series of either basic, adverse, or severely
#' adverse macroeconomic forecasts 
#' @return Object of class ggplot
#' @author David Karapetyan
#' @export
#' @examples
library(ggplot2)
library("zoo")
library("Hmisc")
setwd("c:/ppnr.quant.repo/class_model/")
load("data/model_coefficients_ey.RData")
load("data/model_coefficients_frb.RData")
load("data/macro_forecasts.RData")
load("data/nco_data.RData")
load("data/ppnr_data.RData")
load("data/total_assets.RData")
load("data/capital_data.RData")

source(file = "src/lll_forecast/nco_forecast.R")
source(file = "src/lll_forecast/balance_forecast.R")
source(file = "src/capital_forecast/capital_forecast.R")
source(file = "src/lll_forecast/loss_forecast.R")
source(file = "src/afs_forecast/afs_forecast.R")
source(file = "src/lll_forecast/lll_forecast.R")
source(file = "src/prepare_position_data.R")

#' 
GraphForecast(
book = "capital", 
variable = "Net Income",
bank = "Adbanc, Inc.",
quarter = "2014Q3",
nco_data = nco_data,
ppnr_data = ppnr_data,
total_assets = total_assets,
capital_data = capital_data,
model_coefficients = model_coefficients_ey,
macro_forecasts = macro_forecasts)


GraphForecast <- function(
    book, variable, bank, quarter, nco_data, ppnr_data,
    total_assets, capital_data, model_coefficients, macro_forecasts) {
  
#  book <- tolower(book) #make case of input irrelevant 
  
  .book_list <- list(
      lll = list(
          name = "LLL",
          variable = c("Total.Reserves...000.", "Provision"),
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
  
  
#   if (!(book %in% .book_list)) {
#    stop(paste("Error: Please input a book value of:", names(book))) 
#  }
# if (!(variable %in% .book_list[[book]][["variable"]])) {
#    stop(paste("Error: Please input a book value of:",
#            .book_list[[book]][["variable"]])) 
#  }
  
  
  
  
  
  
  
  
  
# TODO Extend function to graph object and variables generated by
# loss_forecast.R file and corresponding LossForecast function. All other
# objects Ryan wants are supported as inputs to GraphForecast
  
#  if (file.exists(paste("src/", as.name(book), "_forecast/",
#					as.name(book), "_forecast.R", sep=""))) {
# 	source(paste("src/", as.name(book), "_forecast/",
#					as.name(book), "_forecast.R", sep=""))
#  }
  
#safeguard against user error inputting book name
book <- tolower(book) 

#retrieve appropriate forecasting function, and prepare to graph

  .book <- .book_list[[book]]
  position_data <- prepare_position_data(
      bank, quarter, nco_data, ppnr_data , total_assets, capital_data)
  
  .func <- as.name(.book[["func"]])
  book_zoo <- as.zoo(eval(.func)(
          position_data,
          model_coefficients,
          macro_forecasts))
  book_fortified <- fortify(book_zoo)
  
#get rid of all rows with NAs, so ggplot doesn't give warning
  book_fortified <- na.omit(book_fortified)  
  
#otherwise, aes doesn't source environment properly
  .environment <- environment() 
  p <- ggplot(
      data = book_fortified,
      aes(book_fortified$Index,
          book_fortified[[make.names(variable)]]),
      environment = .environment) #error with $ sign instead of [[ ]]
  p <- p + ggtitle(paste(.book[["name"]], "Forecast"))
  p <- p + xlab("Time")
  p <- p + ylab(variable)
  p <- p + geom_line()
  return (p)
}