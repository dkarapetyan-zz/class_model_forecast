#' Performs several multi variate analysis on a time series matrix.
#' 
#' Currently the test will run a VIF, correlation and principal component
#' analysis on the data specified.
#' @param data The data that should be used in the analysis.
#' @param ...  Extra arguments passed to vif, cor, prcomp
#' @examples
#'  df <- data.frame(a = 1 : 10, b = 2 * rnorm(10))
#'  res <- multi.variate.analysis(df)#'
#'
#' @seealso \code{\link{usdm::vif}} 
#' @seealso \code{\link{stats::cor}} 
#' @seealso \code{\link{stats::prcomp}} 
#'
multi.variate.analysis <- function(data, ...) {
  if (!(any(class(data) %in% c("matrix", "data.frame")))) {
    warning("Will cast data to matrix.")
    data <- as.matrix(data)
  }

  data_name <- deparse(substitute(data))
  # Perform a colinearity analysis.
  vif.call <- paste("vif(", data_name, ",  ...)")
  vif.result <- try(eval(parse(file = "", text = vif.call)))

  cor.call <- paste("cor(", data_name, ", ...)")
  cor.result <- try(eval(parse(file = "", text = cor.call)))

  prcomp.call <- paste("prcomp(", data_name, ", ...)")
  prcomp.result <- try(eval(parse(file = "", text = prcomp.call)))

  structure(list(vif.call = vif.call,
                 vif.result = vif.result,
                 cor.call = cor.call,
                 cor.result = cor.result,
                 prcomp.call = prcomp.call,
                 prcomp.result = prcomp.result),
            class = "MultiVariateAnalysis")
}
