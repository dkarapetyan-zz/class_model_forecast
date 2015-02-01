#' Prints results from multi variate analysis.
#'
#'
print.MultiVariateAnalysis <- function(object, format = NULL,
                                    return.statistics = FALSE, 
                                    pretty.varnames = NULL,
                                    print.results = c('VIF', 'cor', 'prcomp'),
                                    print.all.tables = FALSE,
                                    ...) {
  if (class(object) != "MultiVariateAnalysis") {
    stop("object is expected to be of type MultiVariateAnalysis.")
  }


  .return_statistics <- list()
  .return_tables <- list()
  show_results <- 
    defmacro(object_attribute, 
             test_name, 
             expected_object_class,
             change_class_code,
             change_varnames_code,
             expr = {
               # Verify whether the test must be printed.
               if (test_name %in% print.results) {
                 # Verify whether the attribute has the correct class.
                 if (class(object[[object_attribute]]) == 
                     expected_object_class)
                   # Expose the test name internally so that the code passed
                   # by the caller can use the test_name.
                   .test_result <- object[[object_attribute]]
                 .test_name_internal <- test_name

                 # Convert to data frame or list. 
                 try(eval(parse(file = "", 
                                text = expression(change_class_code))))

                 # Change the variable names if required.
                 if (!is.null(pretty.varnames)) {
                   eval(parse(file = "", text =
                              expression(change_varnames_code)))
                 }
                 if (class(.test_result) == "data.frame") {
                   # Store the tables for later use.
                   .return_statistics[[test_name]] <- .test_result
                   .return_tables[[test_name]] <- 
                     kable(.test_result, format = format, ...)
                 } else if (class(.test_result) == "list") {
                   # Some test yield more than one table. For these tests
                   # each table is exported separately.
                   for (l in .test_result) {
                     .result_name <- paste(test_name, l$result.name, 
                                           sep = "_")
                     .return_statistics[[.result_name]] <- l$results
                     .return_tables[[.result_name]] <-
                       kable(l$results, format = format, ...)
                   }
                 }
                 
                 # If we just want to print to the screen.
                 if (is.null(format)) {
                   cat(paste(test_name, "test result.\n"))
                   print(object[[object_attribute]])
                 } 
               }
             })

  # Show all the results for the VIF analyis.
  show_results("vif.result", "VIF", "data.frame", 
               change_class_code = {
                 # No need for casting so just assign.
                 .test_result <- .test_result
               },
               change_varnames_code = {
                 # Verify whether the pretty.varnames has the right dimension.
                 if (length(.test_result$Variables)
                     != length(pretty.varnames)) {
                   stop("pretty.varnames is not of the right dimension")
                 }
                 # Change the variable names.
                 .test_result$Variables <- pretty.varnames
               }
               )

  # Show all the results for the correlation.
  show_results("cor.result", "cor", "matrix", 
               change_class_code = {
                 # The result of correlation is a matrix so cast it to a
                 # data.frame.
                 .test_result <- as.data.frame(.test_result)
               },
               change_varnames_code = {
                 # Verify whether pretty.varnames has the right dimension.
                 if (length(colnames(.test_result))
                     != length(pretty.varnames)) {
                   stop("pretty.varnames is not of the right dimension")
                 }
                 # Change the variable names.
                 colnames(.test_result) <- pretty.varnames
                 rownames(.test_result) <- pretty.varnames
               }
               )

  # Show all the results for the principal component analysis (PCA).
  show_results("prcomp.result", "prcomp", "prcomp", 
               change_class_code = {
                 # The PCA analysis results in two tables, the standard
                 # deviations and the rotations. Each table is stored
                 # separately in a list.
                 .test_result <- 
                   list(sdev = list(result.name = "sdev", 
                                    results = as.data.frame(.test_result$sdev)),
                        rotation = list(result.name = "rotation",
                                        results =
                                         as.data.frame(.test_result$rotation)))
                 # Give the column of the standard deviation are more sensible
                 # name than the name assigned by as.data.frame.
                 names(.test_result[["sdev"]][["results"]]) <- "sdev"
               },
               change_varnames_code = {
                 # Verify whether the pretty.varnames has the right dimension.
                 if (length(rownames(.test_result[["sdev"]][["results"]]))
                     != length(pretty.varnames)) {
                   stop("pretty.varnames is not of the right dimension")
                 }
                 # Change the names of the rows
                 names(.test_result[["sdev"]][["results"]]) <- "Standard deviation"
                 rownames(.test_result[["sdev"]][["results"]]) <- pretty.varnames
                 rownames(.test_result[["rotation"]][["results"]]) <- pretty.varnames
               }
               )

  if (print.all.tables) {
    for (df in .return_tables) {
      cat(df)
    }
  }

  .to_return <- list()
  if (!is.null(format)) {
    .to_return[["kable.tables"]] <- .return.tables
  }
  if (return.statistics) {
    .to_return[["statistics"]] <- .return_statistics
  }
 
  if (length(.to_return) > 0) {
    return(.to_return)
  }
}
