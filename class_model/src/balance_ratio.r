
## note that the balance sheets and asset share is used for PPNR model only. No need for NCO
## model.

T <- 4 * (2014 + 0.75 - 1991)
setwd("C:/ppnr.quant.repo/class_model/data")
list.files()
data.PPNR <- read.csv(skip = 1, "CLASS_PPNR_Calibration_Data.csv")
data.NCO <- read.csv(skip = 1, "CLASS_NCO_Calibration_Data.csv")
intersect(colnames(data.PPNR), colnames(data.NCO))


############################################################# two columns
a1 <- data.PPNR$Con..Total.Real.Estate.Loans...000. - data.NCO$Con..Total.Real.Estate.Loans...000.
a2 <- data.PPNR$Con..Tot.Comm...Ind.Loans...000. - data.NCO$Con..Tot.Comm...Ind.Loans...000.
summary(a1)
summary(a2)
data.PPNR$Con..Total.Real.Estate.Loans...000.[102586:102600]
data.NCO$Con..Total.Real.Estate.Loans...000.[102586:102600]
### given that in the majority of the cases NCO data have the larger values than PPNR, keep
### NCO values instead of PPNR

repeated.list <- intersect(names(data.PPNR), names(data.NCO))
repeated.list1 <- setdiff(repeated.list, c("Period", "Bank", "SNL.Institution.Key"))
data1.PPNR <- data.PPNR[, -which((colnames(data.PPNR) %in% repeated.list1))]
data1 <- merge(data1.PPNR, data.NCO, by = c("Period", "Bank", "SNL.Institution.Key"))

names(data1)
dim(data1)
flag.data <- read.csv("CLASS_Institution_Flag_Data.csv")
# sum(flag.data[,3]) #200, all the top banks are within
tabulate(flag.data[, 3])
# sum(as.numeric(as.character(flag.data[,3])),na.rm=TRUE) # 200

summary(data1$Total.Assets...000.)
class(data1$Total.Assets...000.)

data2 <- data1
data2$Year <- as.numeric(substr(data1$Period, 1, 4))
data2$Qt <- as.numeric(substr(data1$Period, 6, 6))
data2$Time.trend <- data2$Year - 1991 + 0.25 * data2$Qt

# To generate the average earning assets info for all the banks

data3 <- merge(flag.data, data2, by = "SNL.Institution.Key", all = TRUE)
dim(data3) == dim(data2)
names(data3)
data3$Avg.Earning.Assets...000. <- ifelse(!is.na(data3$Interest.Bearing.Balances...000.), data3$Interest.Bearing.Balances...000., 
    0) + ifelse(!is.na(data3$Tot.Fed.Funds...Reverse.Repos...000.), data3$Tot.Fed.Funds...Reverse.Repos...000., 
    0) + ifelse(!is.na(data3$Total.Securities...000.), data3$Total.Securities...000., 0) + ifelse(!is.na(data3$Gross.Loans...Leases...000.), 
    data3$Gross.Loans...Leases...000., 0) + ifelse(!is.na(data3$Total.Trading.Assets...000.), 
    data3$Total.Trading.Assets...000., 0)


data3$Other.RE.Loans <- ifelse(!is.na(data3$Con..Total.Real.Estate.Loans...000.), data3$Con..Total.Real.Estate.Loans...000., 
    0) - ifelse(!is.na(data3$U.S..RE..Cl.end.Frst.Lien.1.4...000.), data3$U.S..RE..Cl.end.Frst.Lien.1.4...000., 
    0) - ifelse(!is.na(data3$U.S..RE..Cl.end.Jr.Lien.1.4...000.), data3$U.S..RE..Cl.end.Jr.Lien.1.4...000., 
    0) - ifelse(!is.na(data3$U.S..RE..Rev.1.4.Fam..HE.Lines....000.), data3$U.S..RE..Rev.1.4.Fam..HE.Lines....000., 
    0) - ifelse(!is.na(data3$U.S..RE..Constr...Land.Dev...000.), data3$U.S..RE..Constr...Land.Dev...000., 
    0) - ifelse(!is.na(data3$U.S..RE..Multifamily.Loans...000.), data3$U.S..RE..Multifamily.Loans...000., 
    0) - ifelse(!is.na(data3$U.S..RE..Comm.RE.Nonfarm.NonRes....000.), data3$U.S..RE..Comm.RE.Nonfarm.NonRes....000., 
    0)

summary(data3$Other.RE.Loans)
### data cleaning
data3$Other.RE.Loans <- ifelse(data3$Other.RE.Loans < 0, 0, data3$Other.RE.Loans)
summary(data3$Other.RE.Loans)
length(which(data3$Other.RE.Loans < 0))  # 5

## split data into two sets: largest 200 banks by assets and the remaining
data_200 <- data3[which(data3$Flag == 1), ]
data_201 <- data3[-which(data3$Flag == 1), ]

setwd("H:/EY/CLASS Model/Calibration/Data")
data_Asset <- read.csv("CLASS_Industry_Asset_Data.csv")

data_Asset$Year <- as.numeric(substr(data_Asset$Period, 1, 4))
data_Asset$Qt <- as.numeric(substr(data_Asset$Period, 6, 6))
data_Asset$Time.trend <- data_Asset$Year - 1991 + 0.25 * data_Asset$Qt



########## construct the balance
balance.ratio.mat.200 <- matrix(NA, nrow = nrow(data_200), ncol = 11)
colnames(balance.ratio.mat.200) <- c("Time.trend", "Bank", "SNL.Institution.Key", "Residential.RE.Loans.Ratio", 
    "Commercial.RE.Loans.Ratio", "CI.Loans.Ratio", "Credit.Card.Loans.Ratio", "Trading.Assets.Ratio", 
    "Securities.Ratio", "Asset.Share", "Risky.AFS.Ratio")
balance.ratio.mat.200[, 1] <- data_200$Time.trend
balance.ratio.mat.200[, 2] <- data_200$Bank.x
balance.ratio.mat.200[, 3] <- data_200$SNL.Institution.Key

balance.ratio.mat.201 <- matrix(NA, nrow = T, ncol = 11)
colnames(balance.ratio.mat.201) <- c("Time.trend", "Bank", "SNL.Institution.Key", "Residential.RE.Loans.Ratio", 
    "Commercial.RE.Loans.Ratio", "CI.Loans.Ratio", "Credit.Card.Loans.Ratio", "Trading.Assets.Ratio", 
    "Securities.Ratio", "Asset.Share", "Risky.AFS.Ratio")
balance.ratio.mat.201[, 1] <- (1:T)/4
balance.ratio.mat.201[, 2] <- "Bank201"


####### Balance Ratios Calculation for top 200 banks
####### balance.ratio.mat.200[,4]=100*data_200$U.S..RE..Tot.Cl.end.1.4.Family...000./data_200$Avg.Earning.Assets...000.
####### U.S..RE..Tot.Cl.end.1.4.Family...000. ->U.S..RE..Total.1.4.Fmly...000.
balance.ratio.mat.200[, 4] <- 100 * data_200$U.S..RE..Total.1.4.Fmly...000./data_200$Avg.Earning.Assets...000.
balance.ratio.mat.200[, 5] <- 100 * data_200$Con..Total.Real.Estate.Loans...000./data_200$Avg.Earning.Assets...000.
balance.ratio.mat.200[, 6] <- 100 * data_200$Con..Tot.Comm...Ind.Loans...000./data_200$Avg.Earning.Assets...000.
balance.ratio.mat.200[, 7] <- 100 * data_200$Con..Credit.Cards...Rel.Plans...000./data_200$Avg.Earning.Assets...000.
balance.ratio.mat.200[, 8] <- 100 * data_200$Total.Trading.Assets...000./data_200$Avg.Earning.Assets...000.
balance.ratio.mat.200[, 9] <- 100 * data_200$Total.Securities...000./data_200$Avg.Earning.Assets...000.


####### Balance Ratios Calculation for bank 201

PPNR.aggregate.t <- function(x1, y, data.t = data_201, f = 400, t = T) {
    data <- data.t[which(data.t$Time.trend == t/4), ]
    X1 <- which(names(data) == x1)
    X2 <- which(names(data) == y)
    sumlist <- which((!is.na(data[, X1])) * (data[, X2] != 0) == 1)
    data1 <- data[sumlist, c(X1, X2)]
    ratio <- sum(data1[, 1])/sum(data1[, 2]) * f
    print(ratio)
    return(ratio)
}

for (tt in (1:T)) {
    balance.ratio.mat.201[tt, 4] <- PPNR.aggregate.t(x1 = "U.S..RE..Tot.Cl.end.1.4.Family...000.", 
        y = "Avg.Earning.Assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 5] <- PPNR.aggregate.t(x1 = "Con..Total.Real.Estate.Loans...000.", 
        y = "Avg.Earning.Assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 6] <- PPNR.aggregate.t(x1 = "Con..Tot.Comm...Ind.Loans...000.", 
        y = "Avg.Earning.Assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 7] <- PPNR.aggregate.t(x1 = "Con..Credit.Cards...Rel.Plans...000.", 
        y = "Avg.Earning.Assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 8] <- PPNR.aggregate.t(x1 = "Total.Trading.Assets...000.", y = "Avg.Earning.Assets...000.", 
        data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 9] <- PPNR.aggregate.t(x1 = "Total.Securities...000.", y = "Avg.Earning.Assets...000.", 
        data.t = data_201, t = tt, f = 400)
    
}


f.zero <- function(x) {
    return(ifelse(!is.na(x), x, 0))
}
########################## code run skips from here ########################## NonRisky portfolio >=2Q09- numerator
########################## AFS(F):US Treasury Secs ($000) AFS(F):Govt Ag Secs ($000) AFS(F):Govt-Spons Ag ($000)
########################## AFS(F): Pass-Through RMBS: Guar by GNMA ($000) AFS(F): Pass-Through RMBS: Issd by FNMA and
########################## FHLMC ($000) AFS(F): Other RMBS: Issd or Guar by GSEs ($000) AFS(F):Other RMBS:Coll by MBS
########################## Issd or Guar by GSEs ($000) AFS(F): Pass-Through CMBS Issd or Guar by GSEs ($000) AFS(F):
########################## Other CMBS Issd or Guar by GSEs ($000) AFS(C):US Treasury Secs ($000) AFS(C):Govt Ag Secs
########################## ($000) AFS(C):Govt-Spons Ag ($000) AFS(C): Pass-Through RMBS: Guar by GNMA ($000) AFS(C):
########################## Pass-Through RMBS: Issd by FNMA and FHLMC ($000) AFS(C): Other RMBS: Issd or Guar by GSEs
########################## ($000) AFS(C):Other RMBS:Coll by MBS Issd or Guar by GSEs ($000) AFS(C): Pass-Through CMBS
########################## Issd or Guar by GSEs ($000) AFS(C): Other CMBS Issd or Guar by GSEs ($000)



# NonRisky portfolio <2Q09 -numerator AFS(F):US Treasury Secs ($000) AFS(F):Govt Ag Secs
# ($000) AFS(F):Govt-Spons Ag ($000) AFS(F):MBS GNMA (historical) ($000) AFS(F):MBS Sp Ag
# (historical) ($000) AFS(F):CMOs (FNMA) (historical) ($000) AFS(C):US Treasury Secs ($000)
# AFS(C):Govt Ag Secs ($000) AFS(C):Govt-Spons Ag ($000) AFS(C):MBS GNMA (historical) ($000)
# AFS(C):MBS Sp Ag (historical) ($000) AFS(C):CMOs (FNMA) (historical) ($000)



# NonRisky.AFS.post=data_200$AFS.F..US.Treasury.Secs...000.+
# data_200$AFS.F..Govt.Ag.Secs...000.+ data_200$AFS.F..Govt.Spons.Ag...000.+
# data_200$AFS.F...Pass.Through.RMBS..Guar.by.GNMA...000.+
# data_200$AFS.F...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.+
# data_200$AFS.F...Other.RMBS..Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.F..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.F...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.F...Other.CMBS.Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.C..US.Treasury.Secs...000.+ data_200$AFS.C..Govt.Ag.Secs...000.+
# data_200$AFS.C..Govt.Spons.Ag...000.+
# data_200$AFS.C...Pass.Through.RMBS..Guar.by.GNMA...000.+
# data_200$AFS.C...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.+
# data_200$AFS.C...Other.RMBS..Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.C..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.C...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.+
# data_200$AFS.C...Other.CMBS.Issd.or.Guar.by.GSEs...000.

NonRisky.AFS.prior <- data_200$AFS.F..US.Treasury.Secs...000. + data_200$AFS.F..Govt.Ag.Secs...000. + 
    data_200$AFS.F..Govt.Spons.Ag...000. + data_200$AFS.F..MBS.GNMA..historical....000. + data_200$AFS.F..MBS.Sp.Ag..historical....000. + 
    data_200$AFS.F..CMOs..FNMA...historical....000. + data_200$AFS.C..US.Treasury.Secs...000. + 
    data_200$AFS.C..Govt.Ag.Secs...000. + data_200$AFS.C..Govt.Spons.Ag...000. + data_200$AFS.C..MBS.GNMA..historical....000. + 
    data_200$AFS.C..MBS.Sp.Ag..historical....000. + data_200$AFS.C..CMOs..FNMA...historical....000.


######## Denominator Total AFS Securities FV ($000) Total Securities AFS BV ($000)


NonRisky.AFS.post <- f.zero(data_200$AFS.F..US.Treasury.Secs...000.) + f.zero(data_200$AFS.F..Govt.Ag.Secs...000.) + 
    f.zero(data_200$AFS.F..Govt.Spons.Ag...000.) + f.zero(data_200$AFS.F...Pass.Through.RMBS..Guar.by.GNMA...000.) + 
    f.zero(data_200$AFS.F...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.) + f.zero(data_200$AFS.F...Other.RMBS..Issd.or.Guar.by.GSEs...000.) + 
    f.zero(data_200$AFS.F..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.) + f.zero(data_200$AFS.F...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.) + 
    f.zero(data_200$AFS.F...Other.CMBS.Issd.or.Guar.by.GSEs...000.) + f.zero(data_200$AFS.C..US.Treasury.Secs...000.) + 
    f.zero(data_200$AFS.C..Govt.Ag.Secs...000.) + f.zero(data_200$AFS.C..Govt.Spons.Ag...000.) + 
    f.zero(data_200$AFS.C...Pass.Through.RMBS..Guar.by.GNMA...000.) + f.zero(data_200$AFS.C...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.) + 
    f.zero(data_200$AFS.C...Other.RMBS..Issd.or.Guar.by.GSEs...000.) + f.zero(data_200$AFS.C..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.) + 
    f.zero(data_200$AFS.C...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.) + f.zero(data_200$AFS.C...Other.CMBS.Issd.or.Guar.by.GSEs...000.)

NonRisky.AFS.prior <- f.zero(data_200$AFS.F..US.Treasury.Secs...000.) + f.zero(data_200$AFS.F..Govt.Ag.Secs...000.) + 
    f.zero(data_200$AFS.F..Govt.Spons.Ag...000.) + f.zero(data_200$AFS.F..MBS.GNMA..historical....000.) + 
    f.zero(data_200$AFS.F..MBS.Sp.Ag..historical....000.) + f.zero(data_200$AFS.F..CMOs..FNMA...historical....000.) + 
    f.zero(data_200$AFS.C..US.Treasury.Secs...000.) + f.zero(data_200$AFS.C..Govt.Ag.Secs...000.) + 
    f.zero(data_200$AFS.C..Govt.Spons.Ag...000.) + f.zero(data_200$AFS.C..MBS.GNMA..historical....000.) + 
    f.zero(data_200$AFS.C..MBS.Sp.Ag..historical....000.) + f.zero(data_200$AFS.C..CMOs..FNMA...historical....000.)

AFS.Total <- f.zero(data_200$Total.Securities.AFS.BV...000.) + f.zero(data_200$Total.AFS.Securities.FV...000.)
balance.ratio.mat.200[, 11] <- ifelse(data_200$Time.trend >= (2009 - 1991 + 0.5), 100 - 100 * 
    NonRisky.AFS.post/AFS.Total, 100 - 100 * NonRisky.AFS.prior/AFS.Total)
summary(balance.ratio.mat.200[, 11])
which(balance.ratio.mat.200[, 11] < 0)  # five observations have negative values thus are deleted for model fitting
balance.ratio.mat.200[which(balance.ratio.mat.200[, 11] < 0), 11] <- NA


for (tt in (1:T)) {
    data <- data_201[data_201$Time.trend == tt/4, ]
    NonRisky.AFS.post.tt <- sum(f.zero(data$AFS.F..US.Treasury.Secs...000.) + f.zero(data$AFS.F..Govt.Ag.Secs...000.) + 
        f.zero(data$AFS.F..Govt.Spons.Ag...000.) + f.zero(data$AFS.F...Pass.Through.RMBS..Guar.by.GNMA...000.) + 
        f.zero(data$AFS.F...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.) + f.zero(data$AFS.F...Other.RMBS..Issd.or.Guar.by.GSEs...000.) + 
        f.zero(data$AFS.F..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.) + f.zero(data$AFS.F...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.) + 
        f.zero(data$AFS.F...Other.CMBS.Issd.or.Guar.by.GSEs...000.) + f.zero(data$AFS.C..US.Treasury.Secs...000.) + 
        f.zero(data$AFS.C..Govt.Ag.Secs...000.) + f.zero(data$AFS.C..Govt.Spons.Ag...000.) + 
        f.zero(data$AFS.C...Pass.Through.RMBS..Guar.by.GNMA...000.) + f.zero(data$AFS.C...Pass.Through.RMBS..Issd.by.FNMA.and.FHLMC...000.) + 
        f.zero(data$AFS.C...Other.RMBS..Issd.or.Guar.by.GSEs...000.) + f.zero(data$AFS.C..Other.RMBS.Coll.by.MBS.Issd.or.Guar.by.GSEs...000.) + 
        f.zero(data$AFS.C...Pass.Through.CMBS.Issd.or.Guar.by.GSEs...000.) + f.zero(data$AFS.C...Other.CMBS.Issd.or.Guar.by.GSEs...000.))
    
    NonRisky.AFS.prior.tt <- sum(f.zero(data$AFS.F..US.Treasury.Secs...000.) + f.zero(data$AFS.F..Govt.Ag.Secs...000.) + 
        f.zero(data$AFS.F..Govt.Spons.Ag...000.) + f.zero(data$AFS.F..MBS.GNMA..historical....000.) + 
        f.zero(data$AFS.F..MBS.Sp.Ag..historical....000.) + f.zero(data$AFS.F..CMOs..FNMA...historical....000.) + 
        f.zero(data$AFS.C..US.Treasury.Secs...000.) + f.zero(data$AFS.C..Govt.Ag.Secs...000.) + 
        f.zero(data$AFS.C..Govt.Spons.Ag...000.) + f.zero(data$AFS.C..MBS.GNMA..historical....000.) + 
        f.zero(data$AFS.C..MBS.Sp.Ag..historical....000.) + f.zero(data$AFS.C..CMOs..FNMA...historical....000.))
    
    AFS.Total.tt <- sum(f.zero(data$Total.Securities.AFS.BV...000.) + f.zero(data$Total.AFS.Securities.FV...000.))
    
    balance.ratio.mat.201[tt, 11] <- ifelse(tt >= 4 * (2009 - 1991 + 0.5), 100 - 100 * NonRisky.AFS.post.tt/AFS.Total.tt, 
        100 - 100 * NonRisky.AFS.prior.tt/AFS.Total.tt)
}


# Asset share (firm assets as a % of industry) 2170

#'Total.Assets...000.'
for (tt in (1:T)) {
    row.list <- which(data_200$Time.trend == tt/4)
    data <- data_200[data_200$Time.trend == tt/4, ]
    Period <- as.character(data$Period[1])
    Total.Assets <- data_Asset[which(data_Asset[, 1] == Period), 2]
    balance.ratio.mat.200[row.list, 10] <- 100 * data$Total.Assets...000./Total.Assets
    balance.ratio.mat.201[tt, 10] <- 100 * sum(na.rm = TRUE, data_201$Total.Assets...000.[data_201$Time.trend == 
        tt/4])/Total.Assets
}

# AFS risky ratio
summary(as.numeric(balance.ratio.mat.201[, 10]))

balance.ratio.mat <- rbind(balance.ratio.mat.201, balance.ratio.mat.200)
write.csv(balance.ratio.mat, file = paste(getwd(), "BalanceRatio.csv", sep = "")) 
