#take sample of many macroeconomic variables, and munge so that we compute some
#new variables that are crucial as inputs for our autoregression model. 



setwd("c:/ppnr.quant.repo/class_model/data/")
base.macro <- read.csv("base.macro.csv")
adverse.macro <- read.csv("adverse.macro.csv")
severely.adverse.macro <- read.csv("severely.adverse.macro.csv")




macro.input.f <- function(scenario = "", data = "", start.y = 2014, start.q = 3) {

  #first, declare all macroeconomic variables we will generate from the input
  #macroeconomic scenario data
 

    data$annualized.real.gdp.growth <- null
    data$quarterly.change.in.10.year.treasury.yield <- null
    data$quarterly.change.in.bbb.bond.spread <- null
    data$home.price.growth <- null
    data$commercial.property.price.growth <- null
    data$stock.market.returns <- null
    data$annualized.change.in.unemployment <- null

    #next, we run our computations for each variable, then append the variable
    #to the end of the input market scenario spreadsheet "data"

    data$term.spread <- (data$x10.year.treasury.yield - data$x3.month.treasury.yield)
    
    for (i in (4:nrow(data))) {
        data$annualized.real.gdp.growth[i] <- sum(data$real.gdp.growth[(i - 3):i])/4
    }
    
    data$quarterly.change.in.10.year.treasury.yield[2:nrow(data)] <- diff(data$x10.year.treasury.yield)
    
    data$stock.market.returns <- diff(log(data$dow.jones.total.stock.market.index..level.))
    
    data$quarterly.change.in.bbb.bond.spread <- diff(data$bbb.corporate.yield)
   
    #returns maximum of quarterly change, and zero
    data$quarterly.change.in.bbb.spread.if.change.is.positive <- ifelse(data$quarterly.change.in.bbb.bond.spread < 
        0, 0, data$quarterly.change.in.bbb.bond.spread)
    
    
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
    
    data$annualized.change.in.unemployment[-1] <- diff(data$unemployment.rate) * 4
    
    data$time.trend <- data$quarter * 0.25 + (data$year - 1991)
    data <- data[which(data$time.trend >= (start.y - 1991) + 0.25 * start.q), ]
    write.csv(data, file = paste(getwd(), "/", scenario, "macro.model.input.csv", sep = ""), 
        row.names = false)
    
    print(data[1:5, ])
}

macro.input.f(scenario = "base", data = base.macro)
macro.input.f(scenario = "adverse", data = adverse.macro)
macro.input.f(scenario = "severely.adverse", data = severely.adverse.macro) 
