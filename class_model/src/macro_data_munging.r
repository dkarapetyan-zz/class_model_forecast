#Take Sample of many macroeconomic variables, and munge so that we compute some
#new variables that are crucial as inputs for our autoregression model. 



setwd("C:/ppnr.quant.repo/class_model/data/")
Base.macro <- read.csv("Base.macro.csv")
Adverse.macro <- read.csv("Adverse.macro.csv")
Severely.Adverse.macro <- read.csv("Severely.Adverse.macro.csv")




Macro.Input.f <- function(scenario = "", data = "", start.Y = 2014, start.Q = 3) {

  #First, declare all macroeconomic variables we will generate from the input
  #macroeconomic scenario data
 

    data$Annualized.Real.GDP.growth <- NULL
    data$Quarterly.change.in.10.year.Treasury.yield <- NULL
    data$Quarterly.change.in.BBB.bond.spread <- NULL
    data$Home.price.growth <- NULL
    data$Commercial.Property.Price.Growth <- NULL
    data$Stock.Market.returns <- NULL
    data$Annualized.Change.in.Unemployment <- NULL

    #Next, we run our computations for each variable, then append the variable
    #to the end of the input market scenario spreadsheet "data"

    data$Term.Spread <- (data$X10.year.Treasury.yield - data$X3.Month.Treasury.Yield)
    
    for (i in (4:nrow(data))) {
        data$Annualized.Real.GDP.growth[i] <- sum(data$Real.GDP.growth[(i - 3):i])/4
    }
    
    data$Quarterly.change.in.10.year.Treasury.yield[2:nrow(data)] <- diff(data$X10.year.Treasury.yield)
    
    data$Stock.Market.returns <- diff(log(data$Dow.Jones.Total.Stock.Market.Index..Level.))
    
    data$Quarterly.change.in.BBB.bond.spread <- diff(data$BBB.corporate.yield)
   
    #returns maximum of quarterly change, and zero
    data$Quarterly.change.in.BBB.Spread.if.change.is.positive <- ifelse(data$Quarterly.change.in.BBB.bond.spread < 
        0, 0, data$Quarterly.change.in.BBB.bond.spread)
    
    
    for (i in (5:nrow(data))) {
        data$Home.price.growth[i] <- 100 * (log(data$House.Price.Index..Level.[i]) - log(data$House.Price.Index..Level.[i - 
            4]))
        data$Commercial.Property.Price.Growth[i] <- 100 * (log(data$Commercial.Real.Estate.Price.Index..Level.[i]) - 
            log(data$Commercial.Real.Estate.Price.Index..Level.[i - 4]))
        
    }
    
    data$Home.price.growth.if.growth.is.negative <- ifelse(data$Home.price.growth < 0, data$Home.price.growth, 
        0)
    
    data$Commercial.Property.Price.Growth.Negative <- ifelse(data$Commercial.Property.Price.Growth < 
        0, data$Commercial.Property.Price.Growth, 0)
    
    data$Annualized.Change.in.Unemployment[-1] <- diff(data$Unemployment.rate) * 4
    
    data$Time.trend <- data$Quarter * 0.25 + (data$Year - 1991)
    data <- data[which(data$Time.trend >= (start.Y - 1991) + 0.25 * start.Q), ]
    write.csv(data, file = paste(getwd(), "/", scenario, "Macro.Model.Input.csv", sep = ""), 
        row.names = FALSE)
    
    print(data[1:5, ])
}

Macro.Input.f(scenario = "Base", data = Base.macro)
Macro.Input.f(scenario = "Adverse", data = Adverse.macro)
Macro.Input.f(scenario = "Severely.Adverse", data = Severely.Adverse.macro) 
