t <- 4 * (2014 + 0.75 - 1991)
t0 <- t/4

setwd("c:/ppnr.quant.repo/class_model/data/")
list.files()
# load ppnr and nco response data
ppnr.response <- read.csv("response.ppnr.csv")
ppnr.response$snl.institution.key <- ifelse(!is.na(ppnr.response$snl.institution.key), ppnr.response$snl.institution.key, 
    0)
nco.response <- read.csv("response.nco.csv")
nco.stepoff <- nco.response[which(nco.response[, "time.trend"] == t/4), ]
ppnr.stepoff <- ppnr.response[which(ppnr.response[, "time.trend"] == t/4), ]


# load the balance ratio data
ratio.input.data <- read.csv("balanceratio.csv")
dim(ratio.input.data)
names(ratio.input.data)
ratio.input.data$snl.institution.key <- ifelse(!is.na(ratio.input.data$snl.institution.key), 
    ratio.input.data$snl.institution.key, 0)

ratio.input.data <- ratio.input.data[which(ratio.input.data[, "time.trend"] == t/4), ]
dim(ratio.input.data)

setwd("h:/ey/class model/forecast/data/input")
list.files()

# load the macros
base_macro.input.data <- read.csv("basemacro.model.input.csv")
adverse_macro.input.data <- read.csv("adversemacro.model.input.csv")
severely.macro.input.data <- read.csv("severely.adversemacro.model.input.csv")

dim(base_macro.input.data)



# load the coeffcients
coeff.data <- read.csv("market_data_intercept.csv")
names(coeff.data)
# load the current ppnr and nco data output.current=read.csv('response.csv')
# dim(output.current) names(output.current)



############################################################ forecast under base scenario#######################
macro.data <- base_macro.input.data
t.forecast <- nrow(macro.data)


############ for ppnr except # 3 return on trading assets: ##########################
forecast.ppnr.base <- matrix(na, nrow = 0, ncol = t.forecast + 2)
colnames(forecast.ppnr.base) <- c(as.character(macro.data[, "date"]), "snl.institution.key", 
    "variable")

for (i in c(1, 2, 4:7)) {
    # loop for ppnr step 0 set up the output matrix for each line item
    y.hat.mat <- matrix(na, ncol = (t.forecast + 2), nrow = 201)  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014q3 in this experiment @t=1
    y0 <- ppnr.stepoff[, which(colnames(ppnr.stepoff) == coeff.data[i, 1])]
    y.hat.mat[, 1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    l <- length(list)
    x.list <- matrix(na, ncol = l, nrow = 201)
    colnames(x.list) <- list
    colnames(y.hat.mat) <- c(as.character(macro.data[, "date"]), "snl.institution.key", "variable")
    y.hat.mat[, (t.forecast + 1)] <- ratio.input.data$snl
    y.hat.mat[, (t.forecast + 2)] <- as.character(coeff.data[i, 1])
    
    
    for (t in (1:t.forecast)) {
        for (j in (1:l)) {
            if (list[j] == "lagged.dependent.variable") {
                x.list[, j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[, j] <- y0
            }
            if (t > 1) {
                x.list[, j] <- yhat
            }
            
            if (list[j] == "time.trend") {
                x.list[, j] <- t0 + 0.25 * t
            }
            
            if (list[j] == "intercept") {
                x.list[, j] <- 1
            }
            
            if (list[j] == "interaction.bbb.spread.change.and.risky.afs") {
                x.list[, j] <- macro.data$quarterly.change.in.bbb.spread.if.change.is.positive[t] * 
                  ratio.input.data$risky.afs.ratio
            }
            if (list[j] %in% colnames(ratio.input.data)) {
                x.list[, j] <- ratio.input.data[, list[j]]
            }
            if (list[j] %in% colnames(macro.data)) {
                x.list[, j] <- macro.data[t, list[j]]
            }
        }
        yhat <- as.matrix(x.list) %*% t(as.vector(coef.list))
        if (t > 1) {
            y.hat.mat[, t] <- yhat
        }
    }
    forecast.ppnr.base <- rbind(forecast.ppnr.base, y.hat.mat)
}

write.csv(forecast.ppnr.base, file = paste("c:/ppnr.quant.repo/class_model/data/", "ppnr.forecastoutput.base.csv", 
    sep = ""))


####################################################################################### for ppnr# 3 return on trading assets and all the nco: ##########################
forecast.nco.base <- matrix(na, nrow = 0, ncol = t.forecast + 2)
colnames(forecast.nco.base) <- c(as.character(macro.data[, "date"]), "snl.institution.key", 
    "variable")


for (i in c(3, 8:22)) {
    # loop for nco step 0 set up the output matrix for each line item
    y.hat.array <- array(na, (2 + t.forecast))  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014q3 in this experiment @t=1
    y0 <- nco.stepoff[which(colnames(nco.stepoff) == coeff.data[i, 1])]
    y.hat.array[1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    l <- length(list)
    x.list <- array(na, l)
    names(x.list) <- list
    names(y.hat.array) <- macro.data[, "date"]
    for (t in (1:t.forecast)) {
        for (j in (1:l)) {
            if (list[j] == "lagged.dependent.variable") {
                x.list[j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[j] <- y0
            }
            if (t > 1) {
                x.list[j] <- yhat
            }
            
            if (list[j] == "time.trend") {
                x.list[j] <- t0 + 0.25 * t
            }
            
            if (list[j] == "intercept") {
                x.list[j] <- 1
            }
            
            # if (list[j] %in% colnames(ratio.input.data)) {x.list[j]=ratio.input.data[,list[j]]}
            if (list[j] %in% colnames(macro.data)) {
                x.list[j] <- macro.data[t, list[j]]
            }
        }
        yhat <- as.numeric(x.list) %*% (as.numeric(coef.list))
        if (t > 1) {
            y.hat.array[t] <- yhat
        }
        
        y.hat.array[t.forecast + 1] <- "na"
        y.hat.array[t.forecast + 2] <- as.character(coeff.data[i, 1])
        
    }
    forecast.nco.base <- rbind(forecast.nco.base, y.hat.array)
}

write.csv(forecast.nco.base, file = paste("c:/ppnr.quant.repo/class_model/data/", "nco.forecastoutput.base.csv", 
    sep = ""))




######### forecast under adverse scenario
macro.data <- adverse_macro.input.data
t.forecast <- nrow(macro.data)

############ for ppnr except # 3 return on trading assets: ##########################
forecast.ppnr.adverse <- matrix(na, nrow = 0, ncol = t.forecast + 2)
colnames(forecast.ppnr.adverse) <- c(as.character(macro.data[, "date"]), "snl.institution.key", 
    "variable")

for (i in c(1, 2, 4:7)) {
    # loop for ppnr step 0 set up the output matrix for each line item
    y.hat.mat <- matrix(na, ncol = (t.forecast + 2), nrow = 201)  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014q3 in this experiment @t=1
    y0 <- ppnr.stepoff[, which(colnames(ppnr.stepoff) == coeff.data[i, 1])]
    y.hat.mat[, 1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    l <- length(list)
    x.list <- matrix(na, ncol = l, nrow = 201)
    colnames(x.list) <- list
    colnames(y.hat.mat) <- c(as.character(macro.data[, "date"]), "snl.institution.key", "variable")
    y.hat.mat[, (t.forecast + 1)] <- ratio.input.data$snl
    y.hat.mat[, (t.forecast + 2)] <- as.character(coeff.data[i, 1])
    
    
    for (t in (1:t.forecast)) {
        for (j in (1:l)) {
            if (list[j] == "lagged.dependent.variable") {
                x.list[, j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[, j] <- y0
            }
            if (t > 1) {
                x.list[, j] <- yhat
            }
            
            if (list[j] == "time.trend") {
                x.list[, j] <- t0 + 0.25 * t
            }
            
            if (list[j] == "intercept") {
                x.list[, j] <- 1
            }
            
            if (list[j] == "interaction.bbb.spread.change.and.risky.afs") {
                x.list[, j] <- macro.data$quarterly.change.in.bbb.spread.if.change.is.positive[t] * 
                  ratio.input.data$risky.afs.ratio
            }
            if (list[j] %in% colnames(ratio.input.data)) {
                x.list[, j] <- ratio.input.data[, list[j]]
            }
            if (list[j] %in% colnames(macro.data)) {
                x.list[, j] <- macro.data[t, list[j]]
            }
        }
        yhat <- as.matrix(x.list) %*% t(as.vector(coef.list))
        if (t > 1) {
            y.hat.mat[, t] <- yhat
        }
    }
    forecast.ppnr.adverse <- rbind(forecast.ppnr.adverse, y.hat.mat)
}

write.csv(forecast.ppnr.adverse, file = paste("c:/ppnr.quant.repo/class_model/data/", "ppnr.forecastoutput.adverse.csv", 
    sep = ""))


####################################################################################### for ppnr# 3 return on trading assets and all the nco: ##########################
forecast.nco.adverse <- matrix(na, nrow = 0, ncol = t.forecast + 2)
colnames(forecast.nco.adverse) <- c(as.character(macro.data[, "date"]), "snl.institution.key", 
    "variable")


for (i in c(3, 8:22)) {
    # loop for nco step 0 set up the output matrix for each line item
    y.hat.array <- array(na, (2 + t.forecast))  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014q3 in this experiment @t=1
    y0 <- nco.stepoff[which(colnames(nco.stepoff) == coeff.data[i, 1])]
    y.hat.array[1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    l <- length(list)
    x.list <- array(na, l)
    names(x.list) <- list
    names(y.hat.array) <- macro.data[, "date"]
    for (t in (1:t.forecast)) {
        for (j in (1:l)) {
            if (list[j] == "lagged.dependent.variable") {
                x.list[j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[j] <- y0
            }
            if (t > 1) {
                x.list[j] <- yhat
            }
            
            if (list[j] == "time.trend") {
                x.list[j] <- t0 + 0.25 * t
            }
            
            if (list[j] == "intercept") {
                x.list[j] <- 1
            }
            
            # if (list[j] %in% colnames(ratio.input.data)) {x.list[j]=ratio.input.data[,list[j]]}
            if (list[j] %in% colnames(macro.data)) {
                x.list[j] <- macro.data[t, list[j]]
            }
        }
        yhat <- as.numeric(x.list) %*% (as.numeric(coef.list))
        if (t > 1) {
            y.hat.array[t] <- yhat
        }
        
        y.hat.array[t.forecast + 1] <- "na"
        y.hat.array[t.forecast + 2] <- as.character(coeff.data[i, 1])
        
    }
    forecast.nco.adverse <- rbind(forecast.nco.adverse, y.hat.array)
}

write.csv(forecast.nco.adverse, file = paste("c:/ppnr.quant.repo/class_model/data/", "nco.forecastoutput.adverse.csv", 
    sep = ""))













######### forecast under severely adverse scenario
macro.data <- severely.macro.input.data
t.forecast <- nrow(macro.data)

############ for ppnr except # 3 return on trading assets: ##########################
forecast.ppnr.severely.adverse <- matrix(na, nrow = 0, ncol = t.forecast + 2)
colnames(forecast.ppnr.severely.adverse) <- c(as.character(macro.data[, "date"]), "snl.institution.key", 
    "variable")

for (i in c(1, 2, 4:7)) {
    # loop for ppnr step 0 set up the output matrix for each line item
    y.hat.mat <- matrix(na, ncol = (t.forecast + 2), nrow = 201)  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014q3 in this experiment @t=1
    y0 <- ppnr.stepoff[, which(colnames(ppnr.stepoff) == coeff.data[i, 1])]
    y.hat.mat[, 1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    l <- length(list)
    x.list <- matrix(na, ncol = l, nrow = 201)
    colnames(x.list) <- list
    colnames(y.hat.mat) <- c(as.character(macro.data[, "date"]), "snl.institution.key", "variable")
    y.hat.mat[, (t.forecast + 1)] <- ratio.input.data$snl
    y.hat.mat[, (t.forecast + 2)] <- as.character(coeff.data[i, 1])
    
    
    for (t in (1:t.forecast)) {
        for (j in (1:l)) {
            if (list[j] == "lagged.dependent.variable") {
                x.list[, j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[, j] <- y0
            }
            if (t > 1) {
                x.list[, j] <- yhat
            }
            
            if (list[j] == "time.trend") {
                x.list[, j] <- t0 + 0.25 * t
            }
            
            if (list[j] == "intercept") {
                x.list[, j] <- 1
            }
            
            if (list[j] == "interaction.bbb.spread.change.and.risky.afs") {
                x.list[, j] <- macro.data$quarterly.change.in.bbb.spread.if.change.is.positive[t] * 
                  ratio.input.data$risky.afs.ratio
            }
            if (list[j] %in% colnames(ratio.input.data)) {
                x.list[, j] <- ratio.input.data[, list[j]]
            }
            if (list[j] %in% colnames(macro.data)) {
                x.list[, j] <- macro.data[t, list[j]]
            }
        }
        yhat <- as.matrix(x.list) %*% t(as.vector(coef.list))
        if (t > 1) {
            y.hat.mat[, t] <- yhat
        }
    }
    forecast.ppnr.severely.adverse <- rbind(forecast.ppnr.severely.adverse, y.hat.mat)
}

write.csv(forecast.ppnr.severely.adverse, file = paste("c:/ppnr.quant.repo/class_model/data/", 
    "ppnr.forecastoutput.severely.adverse.csv", sep = ""))


####################################################################################### for ppnr# 3 return on trading assets and all the nco: ##########################
forecast.nco.severely.adverse <- matrix(na, nrow = 0, ncol = t.forecast + 2)
colnames(forecast.nco.severely.adverse) <- c(as.character(macro.data[, "date"]), "snl.institution.key", 
    "variable")

for (i in c(3, 8:22)) {
    # loop for nco step 0 set up the output matrix for each line item
    y.hat.array <- array(na, (2 + t.forecast))  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014q3 in this experiment @t=1
    y0 <- nco.stepoff[which(colnames(nco.stepoff) == coeff.data[i, 1])]
    y.hat.array[1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    l <- length(list)
    x.list <- array(na, l)
    names(x.list) <- list
    names(y.hat.array) <- macro.data[, "date"]
    for (t in (1:t.forecast)) {
        for (j in (1:l)) {
            if (list[j] == "lagged.dependent.variable") {
                x.list[j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[j] <- y0
            }
            if (t > 1) {
                x.list[j] <- yhat
            }
            
            if (list[j] == "time.trend") {
                x.list[j] <- t0 + 0.25 * t
            }
            
            if (list[j] == "intercept") {
                x.list[j] <- 1
            }
            
            # if (list[j] %in% colnames(ratio.input.data)) {x.list[j]=ratio.input.data[,list[j]]}
            if (list[j] %in% colnames(macro.data)) {
                x.list[j] <- macro.data[t, list[j]]
            }
        }
        yhat <- as.numeric(x.list) %*% (as.numeric(coef.list))
        if (t > 1) {
            y.hat.array[t] <- yhat
        }
        
        y.hat.array[t.forecast + 1] <- "na"
        y.hat.array[t.forecast + 2] <- as.character(coeff.data[i, 1])
        
    }
    forecast.nco.severely.adverse <- rbind(forecast.nco.severely.adverse, y.hat.array)
}

write.csv(forecast.nco.severely.adverse, file = paste("c:/ppnr.quant.repo/class_model/data/", 
    "nco.forecastoutput.severely.adverse.csv", sep = "")) 
