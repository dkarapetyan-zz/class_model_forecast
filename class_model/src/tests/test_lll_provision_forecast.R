library(testthat)
source("src/lll_provision_forecast/lll_provision_forecast.R")

# Load the class_model_input time series.
load("data/position_data.RData")
load("data/macro_forecasts.RData")
load("data/model_coefficients.RData")


test_that("Output is a matrix time series ", {
  expect_that(LLLForecast(position_data, macro_forecasts, model_coefficients), 
    is_a("mts"))
}) 
