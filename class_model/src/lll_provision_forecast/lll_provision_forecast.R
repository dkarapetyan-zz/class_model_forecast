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



LLLForecast <- function(position_data, model_coefficients, macro_forecasts) {
    source("src/lll_provision_forecast/nco_forecast.R")
    source("src/lll_provision_forecast/balance_forecast.R")
    
    # testing of function arguments done in called subroutines
    
    # initialize model loss forecast time series, which will be used as the input to
    # compute the lll forecast
    
    .model_loss_forecast_ts <- ts(matrix(NA, ncol = 15, nrow = 14), start = c(2014, 
        3), end = c(2017, 4), frequency = 4)
    colnames(.model_loss_forecast_ts) <- c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate", 
        "HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate", "Multifamily.Commercial.Real.Estate", 
        "NonFarm.NonResidential.CRE", "Credit.Card", "Other.Consumer", "CI", "Leases", 
        "Other.Real.Estate", "Loans.to.Foreign.Governments", "Agriculture", "Loans.to.Depository.Institutions", 
        "Other")
    
    # populate model loss via nco forecast and balance forecast
    
    .nco_forecast_ts <- NCOForecast(position_data, model_coefficients, macro_forecasts)
    .balance_forecast_ts <- BalanceForecast(position_data)
    .model_loss_forecast_ts[-1, ] <- .nco_forecast_ts[-1, ]/400 * .balance_forecast_ts[-1, 
        ]
    
    
    # initialize lll forecast
    .lll_forecast_ts <- ts(matrix(NA, ncol = 4, nrow = 14), start = c(2014, 3), end = c(2017, 
        4), frequency = 4)
    colnames(.lll_forecast_ts) <- c("Total.Net.Charge.offs", "X4.Qrt.Net.Charge.offs", 
        "Total.Reserves...000.", "Provision")
    
    
    
    .row <- nrow(.lll_forecast_ts)  #efficiency in looping. 
    for (i in 1:(.row)) {
        if (i == 1) {
            .lll_forecast_ts[1, "Total.Reserves...000."] <- position_data[1, "Total.Reserves...000."]
            
        } else {
            # sum over all necessary elements. Subset out unnecessary
            .lll_forecast_ts[i, "Total.Net.Charge.offs"] <- sum(.model_loss_forecast_ts[i, 
                ])
        }
    }
    # loop. Variables below end one year before end of input data time series
    for (i in 2:(.row - 4)) {
        .lll_forecast_ts[i, "X4.Qrt.Net.Charge.offs"] <- sum(.lll_forecast_ts[(i + 
            1):(i + 4), "Total.Net.Charge.offs"])
        .lll_forecast_ts[i, "Total.Reserves...000."] <- if ((.lll_forecast_ts[(i - 
            1), "Total.Reserves...000."] - .lll_forecast_ts[i, "Total.Net.Charge.offs"]) < 
            .lll_forecast_ts[i, "X4.Qrt.Net.Charge.offs"]) { .lll_forecast_ts[i, "X4.Qrt.Net.Charge.offs"]
        } else if ((.lll_forecast_ts[(i - 1), "Total.Reserves...000."] - .lll_forecast_ts[i, 
            "Total.Net.Charge.offs"]) > .lll_forecast_ts[i, "X4.Qrt.Net.Charge.offs"] * 
            2.5) {
		.lll_forecast_ts[i, "X4.Qrt.Net.Charge.offs"] * 2.5
        } else {
            .lll_forecast_ts[(i - 1), "Total.Reserves...000."]
        }
        
        .lll_forecast_ts[i, "Provision"] <- (.lll_forecast_ts[i, "Total.Reserves...000."] + 
            .lll_forecast_ts[i, "Total.Net.Charge.offs"] - .lll_forecast_ts[(i - 
            1), "Total.Reserves...000."])
    }
    
    
    return(.lll_forecast_ts)
} 
