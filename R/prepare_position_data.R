#' This function creates position data for a bank for a given quarter.
#'
#' @param bank  A character string that exists in each of the data files.
#' @param quarter A character string in the format "2013Q1".
#' @param nco_data A data frame object aggregating U.S. bank NCO data
#' @param ppnr_data A data frame object aggregating U.S. bank NCO data
#' @param total_assets A data  Total assets data in data.frame format.
#' @param capital_data  Capital data in data.frame format.
#' @return A data frame of one row that contains all the merged data.

PreparePositionData <- function(bank, quarter, nco_data, ppnr_data,
    total_assets, capital_data) {
 
  
  # Subset the ppnr data.
  .ppnr_bank_info <- ppnr_data[
      ppnr_data$Bank == bank  &
          ppnr_data$Period == quarter
      , ]
  
  # Subset the nco data.
  .nco_bank_info <- nco_data[
      nco_data$Bank == bank  &
          nco_data$Period == quarter
      , ]
  
  # Subset capital data.
  .capital_bank_info <- capital_data[
      capital_data$Bank == bank  &
          capital_data$Period == quarter
      , ]
  
  
  
  #prepare for merge. Drop Bank and Period.
#NCO and PPNR data have 3 variables in common.
#Drop redundancies before merge

  .ppnr_bank_info <- .ppnr_bank_info[,
      !(names(.ppnr_bank_info) %in% c(
                "Bank", 
                "Period",
                "Con..Total.Real.Estate.Loans...000.", 
                "Con..Tot.Comm...Ind.Loans...000.",    
                "Con..Credit.Cards...Rel.Plans...000."
            ))]
  .nco_bank_info <- .nco_bank_info[,
      !(names(.nco_bank_info) %in% c("Bank", "Period"))]
  .capital_bank_info <- .capital_bank_info[,
      !(names(.capital_bank_info) %in% c("Bank", "Period"))]

  #merge
  .final_data <- merge(.nco_bank_info, .ppnr_bank_info,
      by = "SNL.Institution.Key")
  .final_data <- merge(.final_data, .capital_bank_info,
      by = "SNL.Institution.Key")
  .final_data$SNL.Institution.Key <- NULL
  
  #compute Asset Share and add to .final_data
  .final_data$Asset.Share <- (100 * .final_data$Total.Assets...000. /
        total_assets[total_assets$Period == quarter, "Total.Assets"])
  
    # replace NA's in position data with 0's
#  .final_data <- replace(.final_data, is.na(.final_data), 0)


  return(.final_data)
}

