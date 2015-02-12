T <- 4 * (2014 + 0.75 - 1991)
T0 <- T/4

setwd("C:/ppnr.quant.repo/class_model/data/")
list.files()
# load PPNR and NCO response data
PPNR.response <- read.csv("Response.PPNR.csv")
PPNR.response$SNL.Institution.Key <- ifelse(!is.na(PPNR.response$SNL.Institution.Key), PPNR.response$SNL.Institution.Key, 
    0)
NCO.response <- read.csv("Response.NCO.csv")
NCO.stepoff <- NCO.response[which(NCO.response[, "Time.trend"] == T/4), ]
PPNR.stepoff <- PPNR.response[which(PPNR.response[, "Time.trend"] == T/4), ]


# load the balance ratio data
ratio.input.data <- read.csv("BalanceRatio.csv")
dim(ratio.input.data)
names(ratio.input.data)
ratio.input.data$SNL.Institution.Key <- ifelse(!is.na(ratio.input.data$SNL.Institution.Key), 
    ratio.input.data$SNL.Institution.Key, 0)

ratio.input.data <- ratio.input.data[which(ratio.input.data[, "Time.trend"] == T/4), ]
dim(ratio.input.data)

setwd("H:/EY/CLASS Model/Forecast/Data/Input")
list.files()

# load the macros
base.macro.input.data <- read.csv("BaseMacro.Model.Input.csv")
adverse.macro.input.data <- read.csv("AdverseMacro.Model.Input.csv")
severely.macro.input.data <- read.csv("Severely.AdverseMacro.Model.Input.csv")

dim(base.macro.input.data)



# load the coeffcients
coeff.data <- read.csv("CLASS_Market_Data_Input_Intercept.csv")
names(coeff.data)
# load the current PPNR and NCO data Output.current=read.csv('response.csv')
# dim(Output.current) names(Output.current)



############################################################ forecast under base scenario#######################
macro.data <- base.macro.input.data
T.forecast <- nrow(macro.data)


############ for PPNR except # 3 Return on Trading Assets: ##########################
forecast.PPNR.base <- matrix(NA, nrow = 0, ncol = T.forecast + 2)
colnames(forecast.PPNR.base) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", 
    "Variable")

for (i in c(1, 2, 4:7)) {
    # loop for PPNR step 0 set up the output matrix for each line item
    y.hat.mat <- matrix(NA, ncol = (T.forecast + 2), nrow = 201)  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014Q3 in this experiment @t=1
    y0 <- PPNR.stepoff[, which(colnames(PPNR.stepoff) == coeff.data[i, 1])]
    y.hat.mat[, 1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    L <- length(list)
    x.list <- matrix(NA, ncol = L, nrow = 201)
    colnames(x.list) <- list
    colnames(y.hat.mat) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", "Variable")
    y.hat.mat[, (T.forecast + 1)] <- ratio.input.data$SNL
    y.hat.mat[, (T.forecast + 2)] <- as.character(coeff.data[i, 1])
    
    
    for (t in (1:T.forecast)) {
        for (j in (1:L)) {
            if (list[j] == "Lagged.dependent.variable") {
                x.list[, j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[, j] <- y0
            }
            if (t > 1) {
                x.list[, j] <- yhat
            }
            
            if (list[j] == "Time.trend") {
                x.list[, j] <- T0 + 0.25 * t
            }
            
            if (list[j] == "Intercept") {
                x.list[, j] <- 1
            }
            
            if (list[j] == "Interaction.BBB.Spread.change.and.Risky.AFS") {
                x.list[, j] <- macro.data$Quarterly.change.in.BBB.Spread.if.change.is.positive[t] * 
                  ratio.input.data$Risky.AFS.Ratio
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
    forecast.PPNR.base <- rbind(forecast.PPNR.base, y.hat.mat)
}

write.csv(forecast.PPNR.base, file = paste("C:/ppnr.quant.repo/class_model/data/", "PPNR.ForecastOutput.Base.csv", 
    sep = ""))


####################################################################################### for PPNR# 3 Return on trading assets and all the NCO: ##########################
forecast.NCO.base <- matrix(NA, nrow = 0, ncol = T.forecast + 2)
colnames(forecast.NCO.base) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", 
    "Variable")


for (i in c(3, 8:22)) {
    # loop for NCO step 0 set up the output matrix for each line item
    y.hat.array <- array(NA, (2 + T.forecast))  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014Q3 in this experiment @t=1
    y0 <- NCO.stepoff[which(colnames(NCO.stepoff) == coeff.data[i, 1])]
    y.hat.array[1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    L <- length(list)
    x.list <- array(NA, L)
    names(x.list) <- list
    names(y.hat.array) <- macro.data[, "Date"]
    for (t in (1:T.forecast)) {
        for (j in (1:L)) {
            if (list[j] == "Lagged.dependent.variable") {
                x.list[j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[j] <- y0
            }
            if (t > 1) {
                x.list[j] <- yhat
            }
            
            if (list[j] == "Time.trend") {
                x.list[j] <- T0 + 0.25 * t
            }
            
            if (list[j] == "Intercept") {
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
        
        y.hat.array[T.forecast + 1] <- "NA"
        y.hat.array[T.forecast + 2] <- as.character(coeff.data[i, 1])
        
    }
    forecast.NCO.base <- rbind(forecast.NCO.base, y.hat.array)
}

write.csv(forecast.NCO.base, file = paste("C:/ppnr.quant.repo/class_model/data/", "NCO.ForecastOutput.Base.csv", 
    sep = ""))




######### forecast under adverse scenario
macro.data <- adverse.macro.input.data
T.forecast <- nrow(macro.data)

############ for PPNR except # 3 Return on Trading Assets: ##########################
forecast.PPNR.adverse <- matrix(NA, nrow = 0, ncol = T.forecast + 2)
colnames(forecast.PPNR.adverse) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", 
    "Variable")

for (i in c(1, 2, 4:7)) {
    # loop for PPNR step 0 set up the output matrix for each line item
    y.hat.mat <- matrix(NA, ncol = (T.forecast + 2), nrow = 201)  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014Q3 in this experiment @t=1
    y0 <- PPNR.stepoff[, which(colnames(PPNR.stepoff) == coeff.data[i, 1])]
    y.hat.mat[, 1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    L <- length(list)
    x.list <- matrix(NA, ncol = L, nrow = 201)
    colnames(x.list) <- list
    colnames(y.hat.mat) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", "Variable")
    y.hat.mat[, (T.forecast + 1)] <- ratio.input.data$SNL
    y.hat.mat[, (T.forecast + 2)] <- as.character(coeff.data[i, 1])
    
    
    for (t in (1:T.forecast)) {
        for (j in (1:L)) {
            if (list[j] == "Lagged.dependent.variable") {
                x.list[, j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[, j] <- y0
            }
            if (t > 1) {
                x.list[, j] <- yhat
            }
            
            if (list[j] == "Time.trend") {
                x.list[, j] <- T0 + 0.25 * t
            }
            
            if (list[j] == "Intercept") {
                x.list[, j] <- 1
            }
            
            if (list[j] == "Interaction.BBB.Spread.change.and.Risky.AFS") {
                x.list[, j] <- macro.data$Quarterly.change.in.BBB.Spread.if.change.is.positive[t] * 
                  ratio.input.data$Risky.AFS.Ratio
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
    forecast.PPNR.adverse <- rbind(forecast.PPNR.adverse, y.hat.mat)
}

write.csv(forecast.PPNR.adverse, file = paste("C:/ppnr.quant.repo/class_model/data/", "PPNR.ForecastOutput.Adverse.csv", 
    sep = ""))


####################################################################################### for PPNR# 3 Return on trading assets and all the NCO: ##########################
forecast.NCO.adverse <- matrix(NA, nrow = 0, ncol = T.forecast + 2)
colnames(forecast.NCO.adverse) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", 
    "Variable")


for (i in c(3, 8:22)) {
    # loop for NCO step 0 set up the output matrix for each line item
    y.hat.array <- array(NA, (2 + T.forecast))  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014Q3 in this experiment @t=1
    y0 <- NCO.stepoff[which(colnames(NCO.stepoff) == coeff.data[i, 1])]
    y.hat.array[1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    L <- length(list)
    x.list <- array(NA, L)
    names(x.list) <- list
    names(y.hat.array) <- macro.data[, "Date"]
    for (t in (1:T.forecast)) {
        for (j in (1:L)) {
            if (list[j] == "Lagged.dependent.variable") {
                x.list[j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[j] <- y0
            }
            if (t > 1) {
                x.list[j] <- yhat
            }
            
            if (list[j] == "Time.trend") {
                x.list[j] <- T0 + 0.25 * t
            }
            
            if (list[j] == "Intercept") {
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
        
        y.hat.array[T.forecast + 1] <- "NA"
        y.hat.array[T.forecast + 2] <- as.character(coeff.data[i, 1])
        
    }
    forecast.NCO.adverse <- rbind(forecast.NCO.adverse, y.hat.array)
}

write.csv(forecast.NCO.adverse, file = paste("C:/ppnr.quant.repo/class_model/data/", "NCO.ForecastOutput.Adverse.csv", 
    sep = ""))













######### forecast under severely adverse scenario
macro.data <- severely.macro.input.data
T.forecast <- nrow(macro.data)

############ for PPNR except # 3 Return on Trading Assets: ##########################
forecast.PPNR.severely.adverse <- matrix(NA, nrow = 0, ncol = T.forecast + 2)
colnames(forecast.PPNR.severely.adverse) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", 
    "Variable")

for (i in c(1, 2, 4:7)) {
    # loop for PPNR step 0 set up the output matrix for each line item
    y.hat.mat <- matrix(NA, ncol = (T.forecast + 2), nrow = 201)  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014Q3 in this experiment @t=1
    y0 <- PPNR.stepoff[, which(colnames(PPNR.stepoff) == coeff.data[i, 1])]
    y.hat.mat[, 1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    L <- length(list)
    x.list <- matrix(NA, ncol = L, nrow = 201)
    colnames(x.list) <- list
    colnames(y.hat.mat) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", "Variable")
    y.hat.mat[, (T.forecast + 1)] <- ratio.input.data$SNL
    y.hat.mat[, (T.forecast + 2)] <- as.character(coeff.data[i, 1])
    
    
    for (t in (1:T.forecast)) {
        for (j in (1:L)) {
            if (list[j] == "Lagged.dependent.variable") {
                x.list[, j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[, j] <- y0
            }
            if (t > 1) {
                x.list[, j] <- yhat
            }
            
            if (list[j] == "Time.trend") {
                x.list[, j] <- T0 + 0.25 * t
            }
            
            if (list[j] == "Intercept") {
                x.list[, j] <- 1
            }
            
            if (list[j] == "Interaction.BBB.Spread.change.and.Risky.AFS") {
                x.list[, j] <- macro.data$Quarterly.change.in.BBB.Spread.if.change.is.positive[t] * 
                  ratio.input.data$Risky.AFS.Ratio
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
    forecast.PPNR.severely.adverse <- rbind(forecast.PPNR.severely.adverse, y.hat.mat)
}

write.csv(forecast.PPNR.severely.adverse, file = paste("C:/ppnr.quant.repo/class_model/data/", 
    "PPNR.ForecastOutput.Severely.Adverse.csv", sep = ""))


####################################################################################### for PPNR# 3 Return on trading assets and all the NCO: ##########################
forecast.NCO.severely.adverse <- matrix(NA, nrow = 0, ncol = T.forecast + 2)
colnames(forecast.NCO.severely.adverse) <- c(as.character(macro.data[, "Date"]), "SNL.Institution.Key", 
    "Variable")

for (i in c(3, 8:22)) {
    # loop for NCO step 0 set up the output matrix for each line item
    y.hat.array <- array(NA, (2 + T.forecast))  #loop of the forecasting period
    
    # step 1- read the response @stepoff point -- 2014Q3 in this experiment @t=1
    y0 <- NCO.stepoff[which(colnames(NCO.stepoff) == coeff.data[i, 1])]
    y.hat.array[1] <- y0
    
    # step 2 - read the names and the coef of the explanatory variable for this line item
    list <- colnames(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    coef.list <- coeff.data[i, which(coeff.data[i, ] != 0)][-1]
    
    # step 3 - read the x input for the variables
    L <- length(list)
    x.list <- array(NA, L)
    names(x.list) <- list
    names(y.hat.array) <- macro.data[, "Date"]
    for (t in (1:T.forecast)) {
        for (j in (1:L)) {
            if (list[j] == "Lagged.dependent.variable") {
                x.list[j] <- ifelse(t == 1, y0, yhat)
            }
            if (t == 1) {
                x.list[j] <- y0
            }
            if (t > 1) {
                x.list[j] <- yhat
            }
            
            if (list[j] == "Time.trend") {
                x.list[j] <- T0 + 0.25 * t
            }
            
            if (list[j] == "Intercept") {
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
        
        y.hat.array[T.forecast + 1] <- "NA"
        y.hat.array[T.forecast + 2] <- as.character(coeff.data[i, 1])
        
    }
    forecast.NCO.severely.adverse <- rbind(forecast.NCO.severely.adverse, y.hat.array)
}

write.csv(forecast.NCO.severely.adverse, file = paste("C:/ppnr.quant.repo/class_model/data/", 
    "NCO.ForecastOutput.Severely.Adverse.csv", sep = "")) 
