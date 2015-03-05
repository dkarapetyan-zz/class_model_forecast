library(ggplot2)

load("c:/ppnr.quant.repo/class_model/data/model_coefficients.RData")
load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")
load("c:/ppnr.quant.repo/class_model/data/position_data.RData")
source("c:/ppnr.quant.repo/class_model/src/lll_provision_forecast/lll_provision_forecast_david.R")

lll.zoo <- as.zoo(LLLForecast(position.data, model.coefficients, macro.forecasts))
lll.fortified<-fortify(lll.zoo)
lll.fortified<-na.omit(lll.fortified) #get rid of all rows with NAs

p <- ggplot(data = lll.fortified, aes(x = lll.fortified$Index, y = lll.fortified$Provision))
p <- p + labs(title = "LLL Provision Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "Provision")
p <- p + geom_path(data = lll.fortified, aes(x = lll.fortified$Index, y = lll.fortified$Provision))

print(p)