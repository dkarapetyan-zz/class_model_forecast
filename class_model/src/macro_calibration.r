setwd("C:/ppnr.quant.repo/class_model/data/")

data=read.csv("CLASS_MACRO_Calibration_Data_Jan31.csv")
#Base.macro=read.csv("Base.macro.csv")

names(data)
dim(data)

data$Year=as.numeric(substr(data$Period,1,4))
data$Qt=as.numeric(substr(data$Period,6,6))
data$Time.trend=data$Year-1991+0.25*data$Qt

data$Term.Spread=data$X10.year.Treasury.yield-data$X3.Month.Treasury.Yield

data$Quarterly.change.in.10.year.Treasury.yield=NA
data$Quarterly.change.in.10.year.Treasury.yield[2: nrow(data)]=diff(data$X10.year.Treasury.yield)

data$Stock.Market.returns=NA
data$Stock.Market.returns[-1]=diff(log(data$Dow.Jones.Total.Stock.Market.Index..Level.))

data$Quarterly.change.in.BBB.bond.spread=NA
data$Quarterly.change.in.BBB.bond.spread[-1]=diff(data$BBB.corporate.yield)

data$Quarterly.change.in.BBB.Spread.if.change.is.positive=ifelse(data$Quarterly.change.in.BBB.bond.spread<0,0,data$Quarterly.change.in.BBB.bond.spread)

data$Home.price.growth=NA 
data$Commercial.Property.Price.Growth=NA
for (i in (5:nrow(data)))
{ 
  data$Home.price.growth[i]=100*(log(data$House.Price.Index..Level.[i])-
                                 log(data$House.Price.Index..Level.[i-4]))
  data$Commercial.Property.Price.Growth[i]=100*(log(data$Commercial.Real.Estate.Price.Index..Level.[i])-
                                                log(data$Commercial.Real.Estate.Price.Index..Level.[i-4]))
}

data$Home.price.growth.if.growth.is.negative=ifelse(data$Home.price.growth<0, 
                                                    data$Home.price.growth, 0)

data$Commercial.Property.Price.Growth.Negative=ifelse(data$Commercial.Property.Price.Growth<0, 
                                                      data$Commercial.Property.Price.Growth, 0)

data$Annualized.Change.in.Unemployment=NA
data$Annualized.Change.in.Unemployment[-1]=diff(data$Unemployment.rate)*4

write.csv(data,file=paste(getwd(), "Macro.Historical.Input.csv", sep=''))

print(data[1:5,])


###input data: "CLASS_MACRO_Calibration_Data_Jan31.csv"
###output data:  ".../CLASS Model/Calibration/Data/Input/Macro.Historical.Input.csv"



