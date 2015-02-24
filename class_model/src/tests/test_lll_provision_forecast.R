library(testthat)
source("c:\\ppnr.quant.repo\\class_model\\src\\lll_provision_forecast_david.R")
# Load the class_model_input time series.
load("c:/ppnr.quant.repo/class_model/data/lll_input.RData")
test_that("Output is a matrix time series ", {
lll.output <- LLLForecast(lll.input)
expect_that(lll.output, is_a("mts"))	
})
