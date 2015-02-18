#take sample of many macroeconomic variables, and munge so that we compute some
#new variables that are crucial as inputs for our autoregression model. 









InputMacroMunger <- function(data = "", start.y = 2014, start.q = 3) {
  setwd("c:/ppnr.quant.repo/class_model/data/")
  data <- read.csv(data)  
  colnames(data) <- gsub("\\.{2}.*$", "", colnames(data))   #delete ellipses and everything after from import
  colnames(data) <- lapply(colnames(data), tolower)      #change all variables to lowercase 
data <- head(data, -1) #delete last row, which has gibberish
  
  #first, declare all macroeconomic variables we will generate from the input
  #macroeconomic scenario data

  munged.macro.data<-list()
  munged.macro.data$date <- NULL
  munged.macro.data$quarter <- NULL
  munged.macro.data$year <- NULL
  munged.macro.data$annualized.real.gdp.growth <- NULL
  munged.macro.data$quarterly.change.in.10.year.treasury.yield <- NULL
  munged.macro.data$quarterly.change.in.bbb.bond.spread <- NULL
  munged.macro.data$home.price.growth <- NULL
  munged.macro.data$commercial.property.price.growth <- NULL
  munged.macro.data$stock.market.returns <- NULL
  munged.macro.data$annualized.change.in.unemployment <- NULL

  #next, we run our computations for each variable, then append the variable
  #to the end of the input market scenario spreadsheet "data"

munged.macro.data$date <- data$date
  munged.macro.data$quarter <- data$quarter
  munged.macro.data$year <- data$year
  munged.macro.data$term.spread <- (data$x10.year.treasury.yield - data$x3.month.treasury.yield)




  for (i in (1:(nrow(data)-3))) {
    munged.macro.data$annualized.real.gdp.growth[4*i] <- sum(data$real.gdp.growth[i:(i+3)])/4
  }

  munged.macro.data$quarterly.change.in.10.year.treasury.yield[2:nrow(data)] <- diff(data$x10.year.treasury.yield)

  munged.macro.data$stock.market.returns <- diff(log(data$dow.jones.total.stock.market.index))

  munged.macro.data$quarterly.change.in.bbb.bond.spread <- diff(data$bbb.corporate.yield)

  #returns maximum of quarterly change, and zero. Seems unncessary, according to whitepaper
#  munged.macro.data$quarterly.change.in.bbb.spread.if.change.is.positive <- ifelse(data$quarterly.change.in.bbb.bond.spread < 
#                                                                      0, 0, data$quarterly.change.in.bbb.bond.spread)


  for (i in (1:(nrow(data)-4))) { # 3 entries of NA, with fourth entry corresponding to annual growth
    munged.macro.data$home.price.growth[4*i] <- 100 * (log(data$house.price.index[i+4]) - log(data$house.price.index[i]))
    munged.macro.data$commercial.property.price.growth[4*i] <- 100 * (log(data$commercial.real.estate.price.index[i+4]) - 
                                                       log(data$commercial.real.estate.price.index[i]))
  }

#  munged.macro.data$home.price.growth.if.growth.is.negative[4*i] <- min(munged.macro.data$home.price.growth[4*i], 0) 
#  munged.macro.data$commercial.property.price.growth.negative[4*i] <- min(munged.macro.data$commercial.property.price.growth[4*i], 0)

  munged.macro.data$annualized.change.in.unemployment <- diff(data$unemployment.rate) * 4

  munged.macro.data$time.trend <- data$quarter * 0.25 + (data$year - 1991)
  #munged.macro.data <- munged.macro.data[which(munged.macro.data$time.trend >= (start.y - 1991) + 0.25 * start.q), ]
  #may not be necessary,  will revisit later
 
  #return munged.macro.data
  munged.macro.data
}

#Generate Output only for base macro. 
#TODO: rewrite code later on so that it takes a user input (base, adverse, or severely adverse)
#nd outputs the associated capital forecast

InputMacroMunger("base_macro.csv")
