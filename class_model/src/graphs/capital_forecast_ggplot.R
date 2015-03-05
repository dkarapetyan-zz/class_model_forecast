library(ggplot2)

load("c:/ppnr.quant.repo/class_model/data/model_coefficients.RData")
load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")
load("c:/ppnr.quant.repo/class_model/data/position_data.RData")
source("c:/ppnr.quant.repo/class_model/src/capital_forecast/capital_forecast_david.R")

capital.zoo <- as.zoo(CapitalForecast(position.data, model.coefficients, macro.forecasts))
capital.fortified<-fortify(capital.zoo)
capital.fortified<-na.omit(capital.fortified) #get rid of all rows with NAs

p <- ggplot(data = capital.fortified, aes(x = capital.fortified$Index, y = capital.fortified$Net.Income))
p <- p + labs(title = "Capital Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "Net Income")
p <- p + geom_path(data = capital.fortified, aes(x = capital.fortified$Index, y = capital.fortified$Net.Income))

print(p)