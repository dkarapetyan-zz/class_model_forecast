setwd("c:/ppnr.quant.repo/class_model/data/")

data <- read.csv("class_macro_calibration_data_jan31.csv")
# base_macro=read.csv('base_macro.csv')

names(data)
dim(data)

data$year <- as.numeric(substr(data$period, 1, 4))
data$qt <- as.numeric(substr(data$period, 6, 6))
data$time.trend <- data$year - 1991 + 0.25 * data$qt

data$term.spread <- data$x10.year.treasury.yield - data$x3.month.treasury.yield

data$quarterly.change.in.10.year.treasury.yield <- na
data$quarterly.change.in.10.year.treasury.yield[2:nrow(data)] <- diff(data$x10.year.treasury.yield)

data$stock.market.returns <- na
data$stock.market.returns[-1] <- diff(log(data$dow.jones.total.stock.market.index..level.))

data$quarterly.change.in.bbb.bond.spread <- na
data$quarterly.change.in.bbb.bond.spread[-1] <- diff(data$bbb.corporate.yield)

data$quarterly.change.in.bbb.spread.if.change.is.positive <- ifelse(data$quarterly.change.in.bbb.bond.spread < 
    0, 0, data$quarterly.change.in.bbb.bond.spread)

data$home.price.growth <- na
data$commercial.property.price.growth <- na
for (i in (5:nrow(data))) {
    data$home.price.growth[i] <- 100 * (log(data$house.price.index..level.[i]) - log(data$house.price.index..level.[i - 
        4]))
    data$commercial.property.price.growth[i] <- 100 * (log(data$commercial.real.estate.price.index..level.[i]) - 
        log(data$commercial.real.estate.price.index..level.[i - 4]))
}

data$home.price.growth.if.growth.is.negative <- ifelse(data$home.price.growth < 0, data$home.price.growth, 
    0)

data$commercial.property.price.growth.negative <- ifelse(data$commercial.property.price.growth < 
    0, data$commercial.property.price.growth, 0)

data$annualized.change.in.unemployment <- na
data$annualized.change.in.unemployment[-1] <- diff(data$unemployment.rate) * 4

write.csv(data, file = paste(getwd(), "macro.historical.input.csv", sep = ""))

print(data[1:5, ])


### input data: 'class_macro_calibration_data_jan31.csv' output data: '.../class
### model/calibration/data/input/macro.historical.input.csv' 
