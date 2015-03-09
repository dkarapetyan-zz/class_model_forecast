library(ggplot2)

library("zoo")
load("data/model_coefficients.RData")
load("data/macro_forecasts.RData")
load("data/position_data.RData")
source("src/ppnr_forecast/ppnr_forecast.R")

ppnr_zoo <- as.zoo(PPNRForecast(position_data, model_coefficients, macro_forecasts))
ppnr_fortified <- fortify(ppnr_zoo)
ppnr_fortified <- na.omit(ppnr_fortified)
# get rid of all rows with NAs, otherwise #ggplot flags warning

p <- ggplot(data = ppnr_fortified, aes(x = ppnr_fortified$Index, y = ppnr_fortified$PPNR))
p <- p + labs(title = "PPNR Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "PPNR")
p <- p + geom_path(data = ppnr_fortified, aes(x = ppnr_fortified$Index, y = ppnr_fortified$PPNR))

print(p) 
