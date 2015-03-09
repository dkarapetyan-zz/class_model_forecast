library(ggplot2)

library("zoo")
load("data/model_coefficients.RData")
load("data/macro_forecasts.RData")
load("data/position_data.RData")
source("src/lll_provision_forecast/lll_provision_forecast.R")

lll_zoo <- as.zoo(LLLForecast(position_data, model_coefficients, macro_forecasts))
lll_fortified <- fortify(lll_zoo)
lll_fortified <- na.omit(lll_fortified)  #get rid of all rows with NAs

p <- ggplot(data = lll_fortified, aes(x = lll_fortified$Index, y = lll_fortified$Provision))
p <- p + labs(title = "LLL Provision Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "Provision")
p <- p + geom_path(data = lll_fortified, aes(x = lll_fortified$Index, y = lll_fortified$Provision))

print(p) 
