# Author: David Karapetyan This Function outputs a time series ppnr asset
# coefficient forecast from time series input files for bank position data,
# macroeconomic forecasts , and arima model coefficients

#' 
#' @param position_data  A matrix of a particular bank's present balance sheet
#' @param model_coefficients A matrix of arima calibration coefficients computed from past history
#' @param macro_forecasts A matrix of either basic, adverse, or severely adverse macroeconomic forecasts 
#' @returnType  
#' @return object of class MTS
#' @author David Karapetyan
#' 
#' 

RevenueCoeffForecast <- function(position_data, model_coefficients, macro_forecasts) {
    
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
    
    .required_colnames_model_coefficients <-
  c("FirstLien.Residential.Real.Estate", "Junior.Lien.Residential.Real.Estate",
  "HELOC.Residential.Real.Estate", "Construction.Commercial.Real.Estate",
  "Multifamily.Commercial.Real.Estate", "NonFarm.NonResidential.CRE",
  "Credit.Card", "Other.Consumer", "CI", "Leases", "Other.Real.Estate",
  "Loans.to.Foreign.Governments", "Agriculture",
  "Loans.to.Depository.Institutions", "Other")
    
    .required_colnames_macro <- c("Real.GDP.growth", "Nominal.GDP.growth",
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
        stop("Not all required colnames were found in revenue.coeffs.forecast.input")
    }
    if (!all(.required_colnames_model_coefficients %in% colnames(model_coefficients))) {
        stop("Not all required colnames were found in model.coefficients")
    }
    if (!all(.required_colnames_macro %in% colnames(macro_forecasts))) {
        stop("Not all required colnames were found in macro.forecasts")
    }
    
    
    
    # generate process position data, to use in ppnr forecasting artithmetic
    
    .position_data_processed <- matrix(NA, nrow = 7, ncol = 7)
	
    row.names(.position_data_processed) <- c("Residential.RE.Loans.Ratio",
    "Commercial.RE.Loans.Ratio", "CI.Loans.Ratio", "Credit.Card.Loans.Ratio",
    "Trading.Assets.Ratio", "Securities.Ratio", "Asset.Share")

    colnames(.position_data_processed) <- c("B.S.Ratios", "Net.Interest.Margin",
    "Noninterest.Nontrading.Income.Ratio",
    "Compensation.Noninterest.Expense.Ratio",
    "Fixed.Asset.Noninterest.Expense.Ratio", "Other.Noninterest.Expense.Ratio",
    "Return.on.Trading.Assets")
    
    # fill in remaining columns
    .rownames <- row.names(.position_data_processed)  #subset only lpart of table we need
    .colnames <- colnames(.position_data_processed)  # subset only part of table we need 
    .cols <- ncol(.position_data_processed)  #for efficiency in looping
	
    for (i in 1:.cols) {
        if (i == 1) {
            # assign over all rows except Asset.Share
            .pd_subset1 <- c("U.S..RE..Total.1.4.Fmly...000.",
            "Con..Total.Real.Estate.Loans...000.",
            "Con..Tot.Comm...Ind.Loans...000.",
            "Con..Credit.Cards...Rel.Plans...000.",
            "Total.Trading.Assets...000.", "Total.Securities...000.")
	
            .pd_subset2 <- c("Interest.Bearing.Balances...000.",
            "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
            "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
            
            .position_data_processed[!(row.names(.position_data_processed) %in% c("Asset.Share")), 
                "B.S.Ratios"] <- position_data[1, .pd_subset1]/sum(position_data[1, .pd_subset2]) * 100
            
            # take care of Asset.Share
            .position_data_processed["Asset.Share", "B.S.Ratios"] <- position_data[1, "Asset.Share"]
        } else {
            # looping portion
            .position_data_processed[, .colnames[i]] <- (model_coefficients[.rownames, 
                .colnames[i]] * .position_data_processed[, "B.S.Ratios"])
        }
    }
    
    # create blank capital forecast time series
    .revenue_coeffs_forecast_ts <- ts(matrix(NA, ncol = 6, nrow = 14), start = c(2014, 
        3), end = c(2017, 4), frequency = 4)

    cols <- colnames(.revenue_coeffs_forecast_ts) <- c("Net.Interest.Margin",
    "Noninterest.Nontrading.Income.Ratio",
    "Compensation.Noninterest.Expense.Ratio",
    "Fixed.Asset.Noninterest.Expense.Ratio", "Other.Noninterest.Expense.Ratio",
    "Return.on.Trading.Assets")
    
    # first column of our forecast is just our initial input data
    .summing_vec <- c("Interest.Bearing.Balances...000.",
    "Tot.Fed.Funds...Reverse.Repos...000.", "Total.Securities...000.",
    "Gross.Loans...Leases...000.", "Total.Trading.Assets...000.")
    .nrows <- nrow(.revenue_coeffs_forecast_ts)  #for efficiency in looping
	
    for (i in 1:.nrows) {
        if (i == 1) {
            # initial data for ppnr coefficients. Comes from arithmetic on position data
			.revenue_coeffs_forecast_ts[i, "Net.Interest.Margin"] <- position_data[1, 
							"Net.Interest.Income...000."]/sum(position_data[1,
									.summing_vec]) * 400
			
			.revenue_coeffs_forecast_ts[i, "Noninterest.Nontrading.Income.Ratio"] <- ((position_data[1,
									"Total.Noninterest.Income...000."]
							- position_data[1, "NII..Trading.Revenue...000."])/position_data[1,
								"Total.Assets...000."] * 400)
            
            .revenue_coeffs_forecast_ts[i, "Compensation.Noninterest.Expense.Ratio"] <- (position_data[1,
            "NIE..Salary...Benefits...000."]/position_data[1, "Total.Assets...000."] * 400)
            
            .revenue_coeffs_forecast_ts[i, "Fixed.Asset.Noninterest.Expense.Ratio"] <- (position_data[1, 
                "NIE..Premises...Fixed.Assets...000."]/position_data[1, "Total.Assets...000."] * 400)
            
            .revenue_coeffs_forecast_ts[i, "Other.Noninterest.Expense.Ratio"] <- (((position_data[1, 
                "Total.Noninterest.Expense...000."] - sum(position_data[1, c("NIE..Salary...Benefits...000.", 
                "NIE..Premises...Fixed.Assets...000.")]))/position_data[1, "Total.Assets...000."]) * 
                400)
            
            .revenue_coeffs_forecast_ts[i, "Return.on.Trading.Assets"] <- (position_data[1, 
                "NII..Trading.Revenue...000."]/position_data[1, "Total.Trading.Assets...000."] * 
                400)
        } else {
            # arithmetic
            
            .revenue_coeffs_forecast_ts[i, "Net.Interest.Margin"] <- (model_coefficients["Intercept", 
                "Net.Interest.Margin"] + model_coefficients["Lagged.dependent.variable", 
                "Net.Interest.Margin"] * .revenue_coeffs_forecast_ts[(i - 1), "Net.Interest.Margin"] + 
                model_coefficients["Term.Spread", "Net.Interest.Margin"] * macro_forecasts[i, 
                  "Term.Spread"] + model_coefficients["X3.Month.Treasury.Yield", 
                "Net.Interest.Margin"] * macro_forecasts[i, "X3.Month.Treasury.Yield"] + 
                model_coefficients["Time.trend", "Net.Interest.Margin"] * macro_forecasts[i, 
                  "Time.trend"] + sum(.position_data_processed[, "Net.Interest.Margin"]))
            
            .cols <- c("Noninterest.Nontrading.Income.Ratio", "Compensation.Noninterest.Expense.Ratio")
            .revenue_coeffs_forecast_ts[i, .cols] <- (model_coefficients["Intercept", 
                .cols] + model_coefficients["Lagged.dependent.variable", .cols] * 
                .revenue_coeffs_forecast_ts[(i - 1), .cols] + model_coefficients["Stock.Market.returns", 
                .cols] * macro_forecasts[i, "Stock.Market.returns"] + c(sum(.position_data_processed[, 
                .cols[1]]), sum(.position_data_processed[, .cols[2]])))
            
            .revenue_coeffs_forecast_ts[i, "Fixed.Asset.Noninterest.Expense.Ratio"] <-
					(model_coefficients["Intercept", "Fixed.Asset.Noninterest.Expense.Ratio"]
						+ model_coefficients["Lagged.dependent.variable",
								"Fixed.Asset.Noninterest.Expense.Ratio"]
						* .revenue_coeffs_forecast_ts[(i - 1), "Fixed.Asset.Noninterest.Expense.Ratio"]
						+ model_coefficients["Annualized.Change.in.Unemployment",
								"Fixed.Asset.Noninterest.Expense.Ratio"]
						* macro_forecasts[i, "Annualized.Change.in.Unemployment"]
						+ sum(.position_data_processed[, "Fixed.Asset.Noninterest.Expense.Ratio"]))
            
            .revenue_coeffs_forecast_ts[i, "Other.Noninterest.Expense.Ratio"] <- (model_coefficients["Intercept", 
                "Other.Noninterest.Expense.Ratio"]
		+ model_coefficients["Lagged.dependent.variable", "Other.Noninterest.Expense.Ratio"]
		* .revenue_coeffs_forecast_ts[(i - 1), "Other.Noninterest.Expense.Ratio"]
		+ model_coefficients["Quarterly.change.in.BBB.bond.spread", "Other.Noninterest.Expense.Ratio"]
		* macro_forecasts[i, "Quarterly.change.in.BBB.bond.spread"]
		+ sum(.position_data_processed[, "Other.Noninterest.Expense.Ratio"]))
            
            .revenue_coeffs_forecast_ts[i, "Return.on.Trading.Assets"] <- (model_coefficients["Intercept", 
                "Return.on.Trading.Assets"]
		+ model_coefficients["Lagged.dependent.variable", "Return.on.Trading.Assets"]
		* .revenue_coeffs_forecast_ts[(i - 1), "Return.on.Trading.Assets"]
		+ model_coefficients["Quarterly.change.in.BBB.bond.spread", "Return.on.Trading.Assets"]
		* macro_forecasts[i, "Quarterly.change.in.BBB.bond.spread"]
		+ model_coefficients["Quarterly.change.in.BBB.Spread.if.change.is.positive", 
                  "Return.on.Trading.Assets"]
		  * macro_forecasts[i, "Quarterly.change.in.BBB.Spread.if.change.is.positive"]
		  + sum(.position_data_processed[, "Return.on.Trading.Assets"]))
        }
    }
    
    return(.revenue_coeffs_forecast_ts)
}

