library(ggplot2)

library("zoo")
load("data/model_coefficients.RData")
load("data/macro_forecasts.RData")
load("data/position_data.RData")
source("src/capital_forecast/capital_forecast.R")

capital_zoo <- as.zoo(CapitalForecast(position_data, model_coefficients, macro_forecasts))
capital_fortified <- fortify(capital_zoo)
capital_fortified <- na.omit(capital_fortified)  #get rid of all rows with NAs

p <- ggplot(data = capital_fortified, aes(x = capital_fortified$Index, y = capital_fortified$Net.Income))
p <- p + labs(title = "Capital Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "Net Income")
p <- p + geom_path(data = capital_fortified, aes(x = capital_fortified$Index, y = capital_fortified$Net.Income))

print(p) 
