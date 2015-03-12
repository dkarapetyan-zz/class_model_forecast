#'
#' Function creates position data for a bank for a given quarter.
#'
#' @param bank  A bank name that exists in each of the data files.
#' @param quarter A quarter in the format "2013 Q1".
#' @param nco_data  NCO data in zoo format.
#' @param ppnr_data  PPNR data in zoo format.
#' @param total_assets  Total assets data in zoo format.
#' @param capital_data  Capital data in zoo format.
#' @return A data frame that consists of one row that contains all the merged
#' data.
#'
#' @examples
#' citi_postion_data <- prepare_position_data("Citigroup Inc.", "2014 Q3",
#'                    nco_data, ppnr_data, total_assets, capital_data)
prepare_position_data <- function(bank, quarter, nco_data, ppnr_data,
                                  total_assets, capital_data) {
  if (!inherits(nco_data, "zoo")) {
    stop("Expected nco_data to be a zoo object.")
  }
  if (!inherits(total_assets, "zoo")) {
    stop("Expected total_assets to be a zoo object.")
  }
  if (!inherits(capital_data, "zoo")) {
    stop("Expected capital_data to be a zoo object.")
  }
  if (!inherits(ppnr_data, "zoo")) {
    stop("Expected ppnr_data to be a zoo object.")
  }
  if (!is.character(bank)) {
    stop("Expected bank to be a character")
  }
  if (!(zoo::as.yearqtr(quarter) %in% index(nco_data))) {
    stop("Could not find quarter in nco_data index")
  }
  if (!(zoo::as.yearqtr(quarter) %in% index(capital_data))) {
    stop("Could not find quarter in capital_data index")
  }
  if (!(zoo::as.yearqtr(quarter) %in% index(ppnr_data))) {
    stop("Could not find quarter in ppnr_data index")
  }
  if (!(zoo::as.yearqtr(quarter) %in% index(total_assets))) {
    stop("Could not find quarter in total_assets index")
  }
  if (!(bank %in% nco_data$Bank)) {
    stop("Could not find bank in nco_data")
  }
  if (!("Bank" %in% names(ppnr_data))) {
    stop("ppnr_data does not contain a Bank column")
  }
  if (!("Bank" %in% names(nco_data))) {
    stop("ppnr_data does not contain a Bank column")
  }
  if (!("Bank" %in% names(capital_data))) {
    stop("ppnr_data does not contain a Bank column")
  }

  .quarter <- zoo::as.yearqtr(quarter)

  # Subset the ppnr data.
  .ppnr_subset <- as.data.frame(ppnr_data[.quarter, ])
  .ppnr_bank_info <- .ppnr_subset[.ppnr_subset$Bank == bank, ]

  # Subset the nco data.
  .nco_subset <- as.data.frame(nco_data[.quarter, ])
  .nco_bank_info <- .nco_subset[.nco_subset$Bank == bank, ]

  # Subset total assets.
  .capital_subset <- as.data.frame(capital_data[.quarter, ])
  .capital_bank_info <- .capital_subset[.capital_subset$Bank == bank, ]

  .final_data <- merge(.ppnr_bank_info, .nco_bank_info,
                       by = "SNL.Institution.Key")
  # Convert the data to numeric because the merge can cause numeric values to
  # be converted to factors. We remove the second entry since that holds the
  # bank name.
  .final_data <- as.data.frame(t(sapply(.final_data[, -2],
                                        function(x) {
                                          as.numeric(levels(x)[x])
                                        })))

  .final_data <- merge(.final_data, .capital_bank_info,
                       by = "SNL.Institution.Key")


  .final_data$Asset.Share <- (100 * .final_data$Total.Assets...000. /
                              as.numeric(total_assets[.quarter]))

  # As a result of merge some columns will have x or y appended to their names.
  # Rename them so that they match the original position data column names.
  names(.final_data) <- gsub("\\.x", "", names(.final_data))
  names(.final_data) <- gsub("\\.y", ".1", names(.final_data))
  # Remove the double entry Bank column.
  .final_data <- .final_data[,!(names(.final_data) %in% "Bank.1")]


  # Convert all factors that can be converted to numeric, which are all columns
  # except Bank, to numeric.
  .to_return <-
    as.data.frame(t(sapply(.final_data[, !(names(.final_data) %in% "Bank")],
                                        function(x) {
                                          # Convert only if the value is a
                                          # factor. Otherwise return x.
                                          if (is.factor(x)) {
                                            as.numeric(levels(x)[x])
                                          } else {
                                            x
                                          }
                                        })))
  # Bind the Bank name with the converted data.
  .to_return <- cbind(.final_data$Bank, .to_return)
  names(.to_return)[1] <- "Bank"

  return(.to_return)
}

