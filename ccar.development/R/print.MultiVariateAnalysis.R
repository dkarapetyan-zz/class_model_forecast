#' Prints results from multi variate analysis.
#'
#'
print.MultiVariateAnalysis <- function(object) {
  if (class(object) != "multivariateanalysis") {
    stop("object is expected to be of type multivariateanalysis.")
  }

  if (is.null(format)) {
    if (class(object$vif.result) == "data.frame") {
      cat("vif test result\n")
      print(object$vif.result)
    } else {
      warning("could not find vif results.")
    }
 
    if (class(object$cor.result) == "matrix") {
      cat("correlation result\n")
      print(object$cor.result)
    } else {
      warning("could not find correlation results.")
    }
  
    if (class(object$prcomp.result) == "prcomp") {
      cat("principal component analysis result\n")
      print(object$prcomp.result)
    } else {
      warning("could not find principal component results.")
    }
  }
}
