#' This function creates position data for a bank for a given quarter.
#'
#' @param bank  A bank name that exists in each of the data files.
#' @param quarter A quarter in the format "2013Q1".
#' @param nco_data  NCO data in data.frame format.
#' @param ppnr_data  PPNR data in data.frame format.
#' @param total_assets  Total assets data in data.frame format.
#' @param capital_data  Capital data in data.frame format.
#' @return A data frame that consists of one row that contains all the merged data.

prepare_position_data <- function(bank, quarter, nco_data, ppnr_data,
    total_assets, capital_data) {
  if (!inherits(nco_data, "data.frame")) {
    stop("Expected nco_data to be a data.frame object.")
  }
  if (!inherits(total_assets, "data.frame")) {
    stop("Expected total_assets to be a data.frame object.")
  }
  if (!inherits(capital_data, "data.frame")) {
    stop("Expected capital_data to be a data.frame object.")
  }
  if (!inherits(ppnr_data, "data.frame")) {
    stop("Expected ppnr_data to be a data.frame object.")
  }
  if (!is.character(bank)) {
    stop("Expected bank to be a character")
  }
  if (!(quarter %in% nco_data$Period)) {
    stop("Could not find quarter in nco_data")
  }
  if (!(quarter %in% capital_data$Period)) {
    stop("Could not find quarter in capital_data")
  }
  if (!(quarter %in% ppnr_data$Period)) {
    stop("Could not find quarter in ppnr_data")
  }
  if (!(quarter %in% total_assets$Period)) {
    stop("Could not find quarter in total_assets")
  }
  if (!("Bank" %in% names(ppnr_data))) {
    stop("ppnr_data does not contain a Bank column")
  }
  if (!(bank %in% ppnr_data$Bank)) {
    stop("Could not find bank in ppnr_data")
  }
  
  if (!("Bank" %in% names(nco_data))) {
    stop("nco_data does not contain a Bank column")
  }
  if (!(bank %in% nco_data$Bank)) {
    stop("Could not find bank in nco_data")
  }
  
  if (!("Bank" %in% names(capital_data))) {
    stop("capital_data does not contain a Bank column")
  }
  if (!(bank %in% capital_data$Bank)) {
    stop("Could not find bank in capital_data")
  }
  
  

  
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

