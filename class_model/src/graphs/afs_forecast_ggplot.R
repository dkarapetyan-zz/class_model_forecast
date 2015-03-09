library(ggplot2)

library("zoo")
load("data/model_coefficients.RData")
load("data/macro_forecasts.RData")
load("data/position_data.RData")
source("src/afs_forecast/afs_forecast.R")

afs_zoo <- as.zoo(AFSForecast(position_data, model_coefficients, macro_forecasts))
afs_fortified <- fortify(afs_zoo)

afs_fortified$Risk.AFS.Ratio <- NULL  #only one row with entry.
afs_fortified <- na.omit(afs_fortified)  #get rid of all rows with NAs

p <- ggplot(data = afs_fortified, aes(x = afs_fortified$Index, y = afs_fortified$Gain.AFS.Securities))
p <- p + labs(title = "AFS Forecast")
p <- p + labs(x = "Time")
p <- p + labs(y = "Gain AFS Securities")
p <- p + geom_path(data = afs_fortified, aes(x = afs_fortified$Index, y = afs_fortified$Gain.AFS.Securities))

print(p) 
