library(ggplot2)

load("c:/ppnr.quant.repo/class_model/data/model_coefficients.RData")
load("c:/ppnr.quant.repo/class_model/data/macro_forecasts.RData")
load("c:/ppnr.quant.repo/class_model/data/position_data.RData")
source("c:/ppnr.quant.repo/class_model/src/afs_forecast/afs_forecast.R")

afs.zoo <- as.zoo(AFSForecast(position.data, model.coefficients, macro.forecasts))
afs.fortified<-fortify(afs.zoo)

afs.fortified$Risk.AFS.Ratio <- NULL #only one row with entry.
afs.fortified<-na.omit(afs.fortified) #get rid of all rows with NAs

p <- ggplot(data = afs.fortified, aes(x = afs.fortified$Index, y = afs.fortified$Gain.AFS.Securities))
p <- p + labs(title = "AFS Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "Gain AFS Securities")
p <- p + geom_path(data = afs.fortified, aes(x = afs.fortified$Index, y = afs.fortified$Gain.AFS.Securities))

print(p)