#' Performs several multi variate analysis on a time series matrix.
#'
#'
multi.variate.analysis <- function(formula, data, ...) {
  if (class(formula) != "formula") {
    stop("Expect formula to be a formula")
  }

  # If the formula contains terms we check whether these are present in the
  # data to give a sensible error message to the user.
  options(show.error.messages = FALSE)
  try(terms_in_formula <- colnames(attr(terms(formula), "factor")))
  options(show.error.messages = TRUE)

  if (exists("terms_in_formula")) {
    # Verify whether all the terms in the formula are present in the data.
    if (!all(terms_in_formula %in% colnames(data))) {
      stop("not all the terms in the formula are present in the data")
    }
  }

  # Deparse the formula and data so that they can be passed to the functions.
  deparsed_formula <- deparse(formula)
  # Perform a colinearity analysis.
  cond.index.call <- paste("cond.index(", deparsed_formula,
                           ", data,  ...)")
  cond.index.result <- eval(parse(file = "", text = cond.index.call))

  structure(list(formula = deparsed_formula,
                 cond.index.call = cond.index.call,
                 cond.index.result = cond.index.result),
            class = "MultiVariateAnalysis")
}
