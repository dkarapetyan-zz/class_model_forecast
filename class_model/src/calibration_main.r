
t <- 4 * (2014 + 0.75 - 1991)
setwd("c:/ppnr.quant.repo/data")
list.files()

# load the coeffcients
coeff.data <- read.csv("original_class_market_data_input.csv")
names(coeff.data)


setwd("c:/ppnr.quant.repo/data")
list.files()
# load ppnr and nco response data
ppnr.response <- read.csv("response.ppnr.csv")
ppnr.response$snl.institution.key <- ifelse(!is.na(ppnr.response$snl.institution.key), ppnr.response$snl.institution.key, 
    0)
nco.response <- read.csv("response.nco.csv")

# load the balance ratio data
ratio.input.data <- read.csv("balanceratio.csv")
dim(ratio.input.data)
names(ratio.input.data)
ratio.input.data$snl.institution.key <- ifelse(!is.na(ratio.input.data$snl.institution.key), 
    ratio.input.data$snl.institution.key, 0)


# load the macro data
macro.input.data <- read.csv("macro.historical.input.csv")
names(macro.input.data)


list.keep <- which(colsums(coeff.data[, -1]) != 0)
coeff.orig <- coeff.data[, c("variable", colnames(coeff.data[, (list.keep + 1)]))]

coeff.new <- matrix(0, ncol = length(list.keep) + 2, nrow = nrow(coeff.data))
colnames(coeff.new) <- c("variable", colnames(coeff.data[, (list.keep + 1)]), "intercept")
coeff.new[, 1] <- as.character(coeff.data[, 1])

# define the function to calculate the lagged variables
lag1.fun <- function(y, y.name, data = ppnr.response) {
    y.lag <- array(na, length(y))
    for (ii in (1:length(y))) {
        time.trend1 <- data$time.trend[ii] - 0.25
        if (time.trend1 == 0) {
            y.lag[ii] == na
        }
        if (time.trend1 > 0) {
            snl <- data$snl.institution.key[ii]
            y.lag[ii] <- data[which((data$snl.institution.key == snl) + (data$time.trend == 
                time.trend1) == 2), y.name]
        }
    }
    return(y.lag = y.lag)
}


########### ppnr model calibration

for (i in c(1:2, 4:7)) {
    y.data <- ppnr.response[, c("time.trend", as.character(coeff.new[i, 1]), "snl.institution.key")]
    y1.data <- merge(y.data, macro.input.data[, -c(1, 2)], by = "time.trend", all = true)
    y2.data <- merge(y1.data, ratio.input.data[, -c(1, 3)], by = c("time.trend", "snl.institution.key"), 
        all = true)
    
    # response variable
    y <- y2.data[, as.character(coeff.new[i, 1])]
    listna <- c(which(y == "-inf"), which(y == "inf"), which(y == "nan"))
    y[listna] <- na
    y2.data[, as.character(coeff.new[i, 1])] <- y
    
    # x- risk drivers
    list <- names(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    # read the x input for the variables
    l <- length(list)
    x.list <- matrix(na, ncol = l, nrow = length(y))
    colnames(x.list) <- list
    
    for (j in (1:l)) {
        if (list[j] == "lagged.dependent.variable") {
            x.list[, j] <- lag1.fun(y = y, y.name = as.character(coeff.new[i, 1]), data = y2.data)
        }
        
        # if (list[j]== 'time.trend') {x.list[,j] = t0 + 0.25*t}
        
        if (list[j] == "interaction.bbb.spread.change.and.risky.afs") {
            x.list[, j] <- y2.data$quarterly.change.in.bbb.spread.if.change.is.positive * y2.data$risky.afs.ratio/100
        }
        if (list[j] %in% colnames(y2.data)) {
            x.list[, j] <- y2.data[, list[j]]
        }
        
    }
    
    res <- lm(y ~ x.list)
    
    summary(res)
    
    
    for (j in (1:l)) {
        coeff.new[i, list[j]] <- res$coef[1 + which(colnames(x.list) == list[j])]
    }
    coeff.new[i, "intercept"] <- res$coef[1]
}



########### nco model calibration

for (i in (c(3, 8:22))) {
    y.data <- nco.response[, c("time.trend", as.character(coeff.new[i, 1]))]
    y1.data <- merge(y.data, macro.input.data[, -c(1, 2)], by = "time.trend", all = true)
    # y2.data=merge(y1.data, ratio.input.data[,-c(1,3)],by='time.trend',all = true)
    
    # response variable
    y <- y.data[, as.character(coeff.new[i, 1])]
    listna <- c(which(y == "-inf"), which(y == "inf"), which(y == "nan"))
    y[listna] <- na
    y.data[, as.character(coeff.new[i, 1])] <- y
    
    # x- risk drivers
    list <- names(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    # read the x input for the variables
    l <- length(list)
    x.list <- matrix(na, ncol = l, nrow = length(y))
    colnames(x.list) <- list
    
    for (j in (1:l)) {
        if (list[j] == "lagged.dependent.variable") 
            # {x.list[,j]= c(y[-1],na)}
        {
            x.list[, j] <- c(na, y[-length(y)])
        }
        # if (list[j]== 'time.trend') {x.list[,j] = t0 + 0.25*t}
        
        if (list[j] %in% colnames(y1.data)) {
            x.list[, j] <- y1.data[, list[j]]
        }
        
    }
    
    
    res <- lm(y ~ x.list)
    
    summary(res)
    
    for (j in (1:l)) {
        coeff.new[i, list[j]] <- res$coef[1 + which(colnames(x.list) == list[j])]
    }
    coeff.new[i, "intercept"] <- res$coef[1]
    coeff.new[i, 1]
}

########### capital forecast model calibration

for (i in (c(3, 8:22))) {
    y.data <- capital.response[, c("time.trend", as.character(coeff.new[i, 1]))]
    y1.data <- merge(y.data, macro.input.data[, -c(1, 2)], by = "time.trend", all = true)
    # y2.data=merge(y1.data, ratio.input.data[,-c(1,3)],by='time.trend',all = true)
    
    # response variable
    y <- y.data[, as.character(coeff.new[i, 1])]
    listna <- c(which(y == "-inf"), which(y == "inf"), which(y == "nan"))
    y[listna] <- na
    y.data[, as.character(coeff.new[i, 1])] <- y
    
    # x- risk drivers
    list <- names(coeff.data)[which(coeff.data[i, ] != 0)][-1]
    # read the x input for the variables
    l <- length(list)
    x.list <- matrix(na, ncol = l, nrow = length(y))
    colnames(x.list) <- list
    
    for (j in (1:l)) {
        if (list[j] == "lagged.dependent.variable") 
            # {x.list[,j]= c(y[-1],na)}
        {
            x.list[, j] <- c(na, y[-length(y)])
        }
        # if (list[j]== 'time.trend') {x.list[,j] = t0 + 0.25*t}
        
        if (list[j] %in% colnames(y1.data)) {
            x.list[, j] <- y1.data[, list[j]]
        }
        
    }
    
    
    res <- lm(y ~ x.list)
    
    summary(res)
    
    for (j in (1:l)) {
        coeff.new[i, list[j]] <- res$coef[1 + which(colnames(x.list) == list[j])]
    }
    coeff.new[i, "intercept"] <- res$coef[1]
    coeff.new[i, 1]
}




write.csv(coeff.new, row.names = false, file = "c:/ppnr.quant.repo/class_model/data/market_data_intercept.csv")
write.csv(coeff.new, row.names = false, file = "c:/ppnr.quant.repo/class_model/data/market_data_intercept.csv")
write.csv(coeff.orig, row.names = false, file = "c:/ppnr.quant.repo/class_model/data/market_data_orginal.csv") 
