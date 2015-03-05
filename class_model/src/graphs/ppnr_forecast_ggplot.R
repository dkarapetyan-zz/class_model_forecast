library(ggplot2)

load("c:/ppnr.quant.repo/class_model/data/model_coefficients.RData")
load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")
load("c:/ppnr.quant.repo/class_model/data/position_data.RData")
source("c:/ppnr.quant.repo/class_model/src/ppnr_forecast/ppnr_forecast_david.R")

ppnr.zoo <- as.zoo(PPNRForecast(position.data, model.coefficients, macro.forecasts))
ppnr.fortified<-fortify(ppnr.zoo)
ppnr.fortified<-na.omit(ppnr.fortified)
#get rid of all rows with NAs, otherwise #ggplot flags warning

p <- ggplot(data = ppnr.fortified, aes(x = ppnr.fortified$Index, y = ppnr.fortified$PPNR))
p <- p + labs(title = "PPNR Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "PPNR")
p <- p + geom_path(data = ppnr.fortified, aes(x = ppnr.fortified$Index, y = ppnr.fortified$PPNR))

print(p)