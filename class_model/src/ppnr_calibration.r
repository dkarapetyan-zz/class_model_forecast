# this file is to generate the ppnr response variables for the historical data, which will
# be used for recalibration purpose.


t <- 4 * (2014 + 0.75 - 1991)
setwd("c:/ppnr.quant.repo/class_model/data/")
list.files()
data.ppnr <- read.csv(skip = 1, "ppnr_macro_calibration_data.csv")
data.nco <- read.csv(skip = 1, "nco_macro_calibration_data.csv")
intersect(colnames(data.ppnr), colnames(data.nco))


############################################################# two columns
a1 <- data.ppnr$con..total.real.estate.loans...000. - data.nco$con..total.real.estate.loans...000.
a2 <- data.ppnr$con..tot.comm...ind.loans...000. - data.nco$con..tot.comm...ind.loans...000.
summary(a1)
summary(a2)
data.ppnr$con..total.real.estate.loans...000.[102586:102600]
data.nco$con..total.real.estate.loans...000.[102586:102600]
### given that in the majority of the cases nco data have the larger values than ppnr, keep
### nco values instead of ppnr

repeated.list <- intersect(names(data.ppnr), names(data.nco))
repeated.list1 <- setdiff(repeated.list, c("period", "bank", "snl.institution.key"))
data1.ppnr <- data.ppnr[, -which((colnames(data.ppnr) %in% repeated.list1))]
data1 <- merge(data1.ppnr, data.nco, by = c("period", "bank", "snl.institution.key"))

names(data1)
dim(data1)
flag.data <- read.csv("institution_flag_data.csv")
# sum(flag.data[,3]) #200, all the top banks are within
tabulate(flag.data[, 3])
# sum(as.numeric(as.character(flag.data[,3])),na.rm=true) # 200

summary(data1$total.assets...000.)
class(data1$total.assets...000.)

data2 <- data1
data2$year <- as.numeric(substr(data1$period, 1, 4))
data2$qt <- as.numeric(substr(data1$period, 6, 6))
data2$time.trend <- data2$year - 1991 + 0.25 * data2$qt

# to generate the average earning assets info for all the banks

data3 <- merge(flag.data, data2, by = "snl.institution.key", all = true)
dim(data3) == dim(data2)
names(data3)
data3$avg.earning.assets...000. <- ifelse(!is.na(data3$interest.bearing.balances...000.), data3$interest.bearing.balances...000., 
    0) + ifelse(!is.na(data3$tot.fed.funds...reverse.repos...000.), data3$tot.fed.funds...reverse.repos...000., 
    0) + ifelse(!is.na(data3$total.securities...000.), data3$total.securities...000., 0) + ifelse(!is.na(data3$gross.loans...leases...000.), 
    data3$gross.loans...leases...000., 0) + ifelse(!is.na(data3$total.trading.assets...000.), 
    data3$total.trading.assets...000., 0)


data3$other.re.loans <- ifelse(!is.na(data3$con..total.real.estate.loans...000.), data3$con..total.real.estate.loans...000., 
    0) - ifelse(!is.na(data3$u.s..re..cl.end.frst.lien.1.4...000.), data3$u.s..re..cl.end.frst.lien.1.4...000., 
    0) - ifelse(!is.na(data3$u.s..re..cl.end.jr.lien.1.4...000.), data3$u.s..re..cl.end.jr.lien.1.4...000., 
    0) - ifelse(!is.na(data3$u.s..re..rev.1.4.fam..he.lines....000.), data3$u.s..re..rev.1.4.fam..he.lines....000., 
    0) - ifelse(!is.na(data3$u.s..re..constr...land.dev...000.), data3$u.s..re..constr...land.dev...000., 
    0) - ifelse(!is.na(data3$u.s..re..multifamily.loans...000.), data3$u.s..re..multifamily.loans...000., 
    0) - ifelse(!is.na(data3$u.s..re..comm.re.nonfarm.nonres....000.), data3$u.s..re..comm.re.nonfarm.nonres....000., 
    0)

summary(data3$other.re.loans)
### data cleaning
data3$other.re.loans <- ifelse(data3$other.re.loans < 0, 0, data3$other.re.loans)
summary(data3$other.re.loans)
length(which(data3$other.re.loans < 0))  # 5

## split data into two sets: largest 200 banks by assets and the remaining
data_200 <- data3[which(data3$flag == 1), ]
data_201 <- data3[-which(data3$flag == 1), ]

################################################################################################ start to construct the response responses for both ppnr and nco ####################

y.mat.200.ppnr <- matrix(na, ncol = 10, nrow = nrow(data_200))
colnames(y.mat.200.ppnr) <- c("time.trend", "net.interest.margin", "noninterest.nontrading.income.ratio", 
    "return.on.trading.assets", "compensation.noninterest.expense.ratio", "fixed.asset.noninterest.expense.ratio", 
    "other.noninterest.expense.ratio", "return.on.afs.securities", "bank", "snl.institution.key")

y.mat.200.ppnr[, "bank"] <- data_200[, "bank.x"]
y.mat.200.ppnr[, "snl.institution.key"] <- data_200$snl.institution.key

y.mat.201.ppnr <- matrix(na, ncol = 10, nrow = t)
colnames(y.mat.201.ppnr) <- c("time.trend", "net.interest.margin", "noninterest.nontrading.income.ratio", 
    "return.on.trading.assets", "compensation.noninterest.expense.ratio", "fixed.asset.noninterest.expense.ratio", 
    "other.noninterest.expense.ratio", "return.on.afs.securities", "bank", "snl.institution.key")


y.mat.nco <- matrix(na, ncol = 16, nrow = t)
colnames(y.mat.nco) <- c("time.trend", "firstlien.residential.real.estate", "junior.lien.residential.real.estate", 
    "heloc.residential.real.estate", "construction.commercial.real.estate", "multifamily.commercial.real.estate", 
    "nonfarm.nonresidential.cre", "credit.card", "other.consumer", "ci", "leases", "other.real.estate", 
    "loans.to.foreign.governments", "agriculture", "loans.to.depository.institutions", "other")


y.mat.200.ppnr[, 1] <- data_200$time.trend
y.mat.201.ppnr[, 1] <- (1:t)/4
y.mat.nco[, 1] <- (1:t)/4

# ppnr item 1; bank 1- 200

# y.mat.200.ppnr[,2]=ifelse(
# !is.na(data_200$net.interest.income...000./data_200$avg.earning.assets...000.),
# data_200$net.interest.income...000./data_200$avg.earning.assets...000.*400, 0)

y.mat.200.ppnr[, 2] <- data_200$net.interest.income...000./data_200$avg.earning.assets...000. * 
    400

# define the aggregation function to calculate the ppnr/nco response for the hypothetical
# bank 201
ppnr.aggregate.t <- function(x, y, data.t = data_201, f = 400, t = t) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x)
    x2 <- which(names(data) == y)
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (data[, x2] != 0) == 1)
    data1 <- data[sumlist, c(x1, x2)]
    ratio <- sum(data1[, 1])/sum(data1[, 2]) * f
    print(ratio)
    return(ratio = ratio)
}

# ppnr item 1 - bank 201

for (tt in (1:t)) {
    y.mat.201.ppnr[tt, 2] <- ppnr.aggregate.t(x = "net.interest.income...000.", y = "avg.earning.assets...000.", 
        data.t = data_201, t = tt, f = 400)
}

summary(y.mat.200.ppnr[, 2])
summary(y.mat.201.ppnr[, 2])

############################################################## 

# ppnr item 2- noninterest.nontrading.income.ratio

y.mat.200.ppnr[, 3] <- (data_200$total.noninterest.income...000. - ifelse(!is.na(data_200$nii..trading.revenue...000.), 
    data_200$nii..trading.revenue...000., 0))/data_200$total.assets...000. * 400

ppnr2.aggregate.t.sign <- function(x1, x2, y, data.t = data_201, f = 400, t = t, sign.list = c(1, 
    -1)) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x1)
    x2 <- which(names(data) == x2)
    x3 <- which(names(data) == y)
    
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (!is.na(data[, x3])) * (data[, 
        x3] != 0) == 1)
    data1 <- data[sumlist, c(x1, x2, x3)]
    ratio <- sum(sign.list[1] * data1[, 1] + sign.list[2] * data1[, 2])/sum(data1[, 3]) * f
    print(ratio)
    return(ratio)
}

for (tt in (1:t)) {
    y.mat.201.ppnr[tt, 3] <- ppnr2.aggregate.t.sign(x1 = "total.noninterest.income...000.", 
        x2 = "nii..trading.revenue...000.", y = "total.assets...000.", data.t = data_201, t = tt, 
        f = 400, sign.list = c(1, -1))
}

summary(y.mat.201.ppnr[, 3])
summary(y.mat.200.ppnr[, 3])

############################################################## 

############# for ppnr item return on trading assets, we also aggregate all the banks for each quarter
############# ppnr_item 3 return.on.trading.assets

return.on.trading.assets <- array(na, t)
for (tt in (1:t)) {
    return.on.trading.assets[tt] <- ppnr.aggregate.t(x = "nii..trading.revenue...000.", y = "total.trading.assets...000.", 
        data.t = data3, t = tt, f = 400)
    
}


############################################################## ppnr item 4: compensation.noninterest.expense.ratio
y.mat.200.ppnr[, 5] <- data_200$nie..salary...benefits...000./data_200$total.assets...000. * 
    400
for (tt in (1:t)) {
    y.mat.201.ppnr[tt, 5] <- ppnr.aggregate.t(x = "nie..salary...benefits...000.", y = "total.assets...000.", 
        data.t = data_201, t = tt, f = 400)
}

summary(y.mat.200.ppnr[, 5])
summary(y.mat.201.ppnr[, 5])


############################################################## ppnr item 5:
############################################################## fixed.asset.noninterest.expense.ratio=1/ppnr_200$total.assets...000.*400*ppnr_200$nie..premises...fixed.assets...000.
y.mat.200.ppnr[, 6] <- data_200$nie..premises...fixed.assets...000./data_200$total.assets...000. * 
    400
for (tt in (1:t)) {
    y.mat.201.ppnr[tt, 6] <- ppnr.aggregate.t(x = "nie..premises...fixed.assets...000.", y = "total.assets...000.", 
        data.t = data_201, t = tt, f = 400)
}

summary(y.mat.200.ppnr[, 6])
summary(y.mat.201.ppnr[, 6])


############################################################## ppnr item 6: other.noninterest.expense.ratio

y.mat.200.ppnr[, 7] <- (data_200$total.noninterest.income...000. - data_200$nie..salary...benefits...000. - 
    data_200$nie..premises...fixed.assets...000.)/data_200$total.assets...000. * 400

ppnr3.aggregate.t.sign <- function(x1, x2, x3, y, data.t = data_201, f = 400, t = t, sign.list = c(1, 
    -1, -1)) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x1)
    x2 <- which(names(data) == x2)
    x3 <- which(names(data) == x3)
    x4 <- which(names(data) == y)
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (!is.na(data[, x3])) * (!is.na(data[, 
        x4])) * (data[, x4] != 0) == 1)
    data1 <- data[sumlist, c(x1, x2, x3, x4)]
    ratio <- sum(sign.list[1] * data1[, 1] + sign.list[2] * data1[, 2] + sign.list[3] * data1[, 
        3])/sum(data1[, 4]) * f
    print(ratio)
    return(ratio)
}

for (tt in (1:t)) {
    y.mat.201.ppnr[tt, 7] <- ppnr3.aggregate.t.sign(x1 = "total.noninterest.income...000.", 
        x2 = "nie..salary...benefits...000.", x3 = "nie..premises...fixed.assets...000.", y = "total.assets...000.", 
        data.t = data_201, t = tt, f = 400, sign.list = c(1, -1, -1))
}

summary(y.mat.200.ppnr[, 7])
summary(y.mat.201.ppnr[, 7])


############################################################## ppnr item 7: return.on.afs.securities

y.mat.200.ppnr[, 8] <- data_200$gain.realized.gns.afs.secs...000./(data_200$total.afs.securities.fv...000. + 
    data_200$total.securities.afs.bv...000.) * 400

############################################################## 
ppnr2a.aggregate.t.sign <- function(x, y1, y2, data.t = data_201, f = 400, t = t, sign.list = c(1, 
    +1)) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x)
    x2 <- which(names(data) == y1)
    x3 <- which(names(data) == y2)
    
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (!is.na(data[, x3])) * ((data[, 
        x2] + data[, x3]) != 0) == 1)
    data1 <- data[sumlist, c(x1, x2, x3)]
    ratio <- sum(data1[, 1])/sum(sign.list[1] * data1[, 2] + sign.list[2] * data1[, 3]) * f
    print(ratio)
    return(ratio)
}


for (tt in (1:t)) {
    y.mat.201.ppnr[tt, 8] <- ppnr2a.aggregate.t.sign(x = "gain.realized.gns.afs.secs...000.", 
        y1 = "total.afs.securities.fv...000.", y2 = "total.securities.afs.bv...000.", data.t = data3, 
        t = tt, sign.list = c(1, 1), f = 400)
}

summary(y.mat.200.ppnr[, 8])
summary(as.numeric(y.mat.201.ppnr[, 8]))
summary(y.mat.200.ppnr)
summary(y.mat.201.ppnr)


############ note that nco, we aggregate all the banks together and to generate one observation within
############ one quarter.


###################### define functions to calculate the nco time series #########################

### define the fucntion to calcualte nco ratio #8
ppnr6.aggregate.t.sign <- function(x1, x2, x3, x4, y1, y2, data.t = data3, t = t, f = 400, sign.list = c(1, 
    -1, 1, -1, 1, 1)) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x1)
    x2 <- which(names(data) == x2)
    x3 <- which(names(data) == x3)
    x4 <- which(names(data) == x4)
    y1 <- which(names(data) == y1)
    y2 <- which(names(data) == y2)
    
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (!is.na(data[, x3])) * (!is.na(data[, 
        x4])) * (!is.na(data[, y1])) * (!is.na(data[, y2])) * ((data[, y1] + data[, y2]) != 
        0) == 1)
    data1 <- data[sumlist, c(x1, x2, x3, x4, y1, y2)]
    ratio <- sum(sign.list[1] * data1[, 1] + sign.list[2] * data1[, 2] + sign.list[3] * data1[, 
        3] + sign.list[4] * data1[, 4])/sum(sign.list[5] * data1[, 5] + sign.list[6] * data1[, 
        6]) * f
    print(ratio)
    return(ratio)
}


############################ nco response items #######################################


############ nco item 1: first.lien.residential.real.estate
############ y.mat.nco[1:200,8]=(data_200$co..u.s..re..close.end.first.lien.1.4.family...000.-
############ data_200$rec..u.s..re..close.end.first.lien.1.4.family...000.)/data_200$u.s..re..cl.end.frst.lien.1.4...000.*400

for (tt in (1:t)) {
    y.mat.nco[tt, 2] <- ppnr2.aggregate.t.sign(x1 = "co..u.s..re..close.end.first.lien.1.4.family...000.", 
        x2 = "rec..u.s..re..close.end.first.lien.1.4.family...000.", y = "u.s..re..cl.end.frst.lien.1.4...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
    
}
############ nco item 2:junior lien residential real estate*

# y.mat.nco[1:200,9]=(data_200$co..u.s..re..close.end.jr.lien.1.4.family...000.-
# data_200$rec..u.s..re..close.end.jr.lien.1.4.family...000.)/data_200$u.s..re..cl.end.jr.lien.1.4...000.*400

for (tt in (1:t)) {
    y.mat.nco[tt, 3] <- ppnr2.aggregate.t.sign(x1 = "co..u.s..re..close.end.jr.lien.1.4.family...000.", 
        x2 = "rec..u.s..re..close.end.jr.lien.1.4.family...000.", y = "u.s..re..cl.end.jr.lien.1.4...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}

############ nco item 3: heloc.residential.real.estate co: u.s. re: revolving 1-4 family (he lines)
############ ($000) 142695 recoveries rec: u.s. re: revolving 1-4 family (he lines) ($000) 142729
############ balance u.s. re: rev 1-4 fam (he lines) ($000) 142403

for (tt in (1:t)) {
    y.mat.nco[tt, 4] <- ppnr2.aggregate.t.sign(x1 = "co..u.s..re..revolving.1.4.family..he.lines....000.", 
        x2 = "rec..u.s..re..revolving.1.4.family..he.lines....000.", y = "u.s..re..rev.1.4.fam..he.lines....000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}



########### nco 4: construction commercial real estate chargeoffs co: u.s. re: construction & land
########### development ($000) 142690 recoveries rec: u.s. re: construction & land development ($000)
########### 142724 balance u.s. re: constr & land dev ($000) 142398

for (tt in (1:t)) {
    y.mat.nco[tt, 5] <- ppnr2.aggregate.t.sign(x1 = "co..u.s..re..construction...land.development...000.", 
        x2 = "rec..u.s..re..construction...land.development...000.", y = "u.s..re..constr...land.dev...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}


########### nco 5: multifamily.commercial.real.estate chargeoffs co: u.s. re: multifamily ($000)
########### 142696 recoveries rec: u.s. re: multifamily ($000) 142730 balance u.s. re: multifamily
########### loans ($000) 142405

for (tt in (1:t)) {
    y.mat.nco[tt, 6] <- ppnr2.aggregate.t.sign(x1 = "co..u.s..re..multifamily...000.", x2 = "rec..u.s..re..multifamily...000.", 
        y = "u.s..re..multifamily.loans...000.", t = tt, data.t = data3, sign.list = c(1, -1), 
        f = 400)
}



# nco 6: nonfarm.nonresidential.cre co..u.s..re..commercial...000.'
#'rec..u.s..re..commercial...000.'
#'u.s..re..comm.re.nonfarm.nonres....000.' 

# chargeoffs co: u.s. re: commercial ($000) 142699 recoveries rec: u.s. re: commercial
# ($000) 142733 balance u.s. re: comm re(nonfarm/nonres) ($000) 142408

for (tt in (1:t)) {
    y.mat.nco[tt, 7] <- ppnr2.aggregate.t.sign(x1 = "co..u.s..re..commercial...000.", x2 = "rec..u.s..re..commercial...000.", 
        y = "u.s..re..comm.re.nonfarm.nonres....000.", t = tt, data.t = data3, sign.list = c(1, 
            -1), f = 400)
}


################ nco 7: credit card 'co..credit.card.loans...000.'  'rec..credit.card.loans...000.'
################ 'con..credit.card.loans...000.'

# chargeoffs - > 2001 co: credit card loans ($000) 210814 chargeoffs - < 2001 co: credit
# card & related plans (historical) ($000) 142709 recoveries - > 2001 rec: credit card loans
# ($000) 210848 recoveries - < 2001 rec: credit card & related plans (historical) ($000)
# 142743 balance con: credit cards & rel plans ($000) 142417
for (tt in (1:t)) {
    y.mat.nco[tt, 8] <- ifelse(y.mat.nco[tt, 1] > 10, ppnr2.aggregate.t.sign(x1 = "co..credit.card.loans...000.", 
        x2 = "rec..credit.card.loans...000.", y = "con..credit.cards...rel.plans...000.", t = tt, 
        data.t = data3, sign.list = c(1, -1), f = 400), ppnr2.aggregate.t.sign(x1 = "co..credit.card...related.plans..historical....000.", 
        x2 = "rec..credit.card...related.plans..historical....000.", y = "con..credit.cards...rel.plans...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400))
}

summary(y.mat.nco[, 8])

############### nco 8: other consumer co..other.consumer.loans...000.'
#'rec..other.consumer.loans...000.'   
# con..other.consumer.loans...000.'

# chargeoffs co: consumer loans ($000) 210818 chargeoffs - > 2001 co: credit card loans
# ($000) 210814 chargeoffs - < 2001 co: credit card & related plans (historical) ($000)
# 142709 recoveries rec: consumer loans ($000) 210852 recoveries - > 2001 rec: credit card
# loans ($000) 210848 recoveries - < 2001 rec: credit card & related plans (historical)
# ($000) 142743 balance con: tot consumer loans ($000) 142414 con: credit cards & rel plans
# ($000) 142417 charge-offs co: consumer loans ($000) - [co: credit card loans ($000) or co:
# credit card & related plans (historical) ($000)] 210818 - [210814 or 142709] recoveries
# rec: consumer loans ($000) - [rec: credit card loans ($000) or rec: credit card & related
# plans (historical) ($000)] 210852 - [210848 or 142743] balance con: tot consumer loans
# ($000) - con: credit cards & rel plans ($000) 142414 - 142417

for (tt in (1:t)) {
    y.mat.nco[tt, 9] <- ifelse(y.mat.nco[tt, 1] > 10, ppnr6.aggregate.t.sign(x1 = "co..consumer.loans...000.", 
        x2 = "co..credit.card.loans...000.", x3 = "rec..consumer.loans...000.", x4 = "rec..credit.card.loans...000.", 
        y1 = "con..tot.consumer.loans...000.", y2 = "con..credit.cards...rel.plans...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1, -1, 1, 1, -1), f = 400), ppnr6.aggregate.t.sign(x1 = "co..consumer.loans...000.", 
        x2 = "co..credit.card...related.plans..historical....000.", x3 = "rec..consumer.loans...000.", 
        x4 = "rec..credit.card...related.plans..historical....000.", y1 = "con..tot.consumer.loans...000.", 
        y2 = "con..credit.cards...rel.plans...000.", t = tt, data.t = data3, sign.list = c(1, 
            -1, -1, 1, 1, -1), f = 400))
}

summary(y.mat.nco[, 9])


############### nco 9: commercial and industrial

# co..commercial...industrial.lns.u.s..addressees...000.'
# 'rec..commercial...industrial.lns.u.s..addressees...000.'
# 'co..commercial...industrial.lns.non.u.s..address...000.'
# 'rec..commercial...industrial.lns.non.u.s..address...000.'
# 'con.comm...ind.us.addr...000.'  'con.comm...ind.non.us.addr...000.'

# chargeoffs co: commercial & industrial lns u.s. addressees ($000) 142703 co: commercial &
# industrial lns non-u.s. address ($000) 142704 recoveries rec: commercial & industrial lns
# u.s. addressees ($000) 142737 rec: commercial & industrial lns non-u.s. address ($000)
# 142738 balance con: tot comm & ind loans ($000) 142413

### define the fucntion to calcualte nco ratio #9
ppnr5.aggregate.t.sign <- function(x1, x2, x3, x4, y, data.t = data3, t = t, f = 400, sign.list = c(1, 
    -1, 1, -1, 1)) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x1)
    x2 <- which(names(data) == x2)
    x3 <- which(names(data) == x3)
    x4 <- which(names(data) == x4)
    y <- which(names(data) == y)
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (!is.na(data[, x3])) * (!is.na(data[, 
        x4])) * (!is.na(data[, y])) * (data[, y] != 0) == 1)
    data1 <- data[sumlist, c(x1, x2, x3, x4, y)]
    ratio <- sum(sign.list[1] * data1[, 1] + sign.list[2] * data1[, 2] + sign.list[3] * data1[, 
        3] + sign.list[4] * data1[, 4])/sum(data1[, 5]) * f
    print(ratio)
    return(ratio)
}

for (tt in (1:t)) {
    y.mat.nco[tt, 10] <- ppnr5.aggregate.t.sign(x1 = "co..commercial...industrial.lns.u.s..addressees...000.", 
        x2 = "rec..commercial...industrial.lns.u.s..addressees...000.", x3 = "co..commercial...industrial.lns.non.u.s..address...000.", 
        x4 = "rec..commercial...industrial.lns.non.u.s..address...000.", y = "con..tot.comm...ind.loans...000.", 
        data.t = data3, t = tt, f = 400, sign.list = c(1, -1, 1, -1, 1))
}
summary(y.mat.nco[, 10])


################################# nco 10: leases co: total lease financing receivables ($000) rec: total lease financing
################################# receivables ($000) con: total leases ($000)

for (tt in (1:t)) {
    y.mat.nco[tt, 11] <- ppnr2.aggregate.t.sign(x1 = "co..total.lease.financing.receivables...000.", 
        x2 = "rec..total.lease.financing.receivables...000.", y = "con..total.leases...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}
summary(y.mat.nco[, 11])

################################### nco 11: other real estate (0.48)

# chargeoffs co: u.s. re: farm loans ($000) 142698 co: loans sec by real estate to non-u.s.
# address ($000) 142689 recoveries rec: u.s. re: farm loans ($000) 142732 rec: loans sec by
# real estate to non-u.s. address ($000) 142723 balance con: total real estate loans ($000)
# 142397 u.s. re: cl-end frst lien 1-4 ($000) 142401 u.s. re: cl-end jr lien 1-4 ($000)
# 142402 u.s. re: rev 1-4 fam (he lines) ($000) 142403 u.s. re: constr & land dev ($000)
# 142398 u.s. re: multifamily loans ($000) 142405 u.s. re: comm re(nonfarm/nonres) ($000)
# 142408


ppnr11.aggregate.t.sign <- function(x1, x2, x3, x4, y1, y2, y3, y4, y5, y6, y7, data.t = data3, 
    t = t, f = 400, sign.list = c(1, 1, -1, -1)) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x1)
    x2 <- which(names(data) == x2)
    x3 <- which(names(data) == x3)
    x4 <- which(names(data) == x4)
    y1 <- which(names(data) == y1)
    y2 <- which(names(data) == y2)
    y3 <- which(names(data) == y3)
    y4 <- which(names(data) == y4)
    y5 <- which(names(data) == y5)
    y6 <- which(names(data) == y6)
    y7 <- which(names(data) == y7)
    
    sumlist <- which((!is.na(data[, x1])) * (!is.na(data[, x2])) * (!is.na(data[, x3])) * (!is.na(data[, 
        x4])) * (!is.na(data[, y1])) * (!is.na(data[, y2])) * (!is.na(data[, y3])) * (!is.na(data[, 
        y4])) * (!is.na(data[, y5])) * (!is.na(data[, y6])) * (!is.na(data[, y7])) * ((data[, 
        y1] - data[, y2] - data[, y3] - data[, y4] - data[, y5] - data[, y6] - data[, y7]) != 
        0) == 1)
    data1 <- data[sumlist, c(x1, x2, x3, x4, y1, y2, y3, y4, y5, y6, y7)]
    ratio <- sum(sign.list[1] * data1[, 1] + sign.list[2] * data1[, 2] + sign.list[3] * data1[, 
        3] + sign.list[4] * data1[, 4])/sum(data1[, 5] - data1[, 6] - data1[, 7] - data1[, 8] - 
        data1[, 9] - data1[, 10] - data1[, 11]) * f
    print(ratio)
    return(ratio)
}

for (tt in (1:t)) {
    y.mat.nco[tt, 12] <- ppnr11.aggregate.t.sign(x1 = "co..u.s..re..farm.loans...000.", x2 = "co..loans.sec.by.real.estate.to.non.u.s..address...000.", 
        x3 = "rec..u.s..re..farm.loans...000.", x4 = "rec..loans.sec.by.real.estate.to.non.u.s..address...000.", 
        y1 = "con..total.real.estate.loans...000.", y2 = "u.s..re..cl.end.frst.lien.1.4...000.", 
        y3 = "u.s..re..cl.end.jr.lien.1.4...000.", y4 = "u.s..re..rev.1.4.fam..he.lines....000.", 
        y5 = "u.s..re..constr...land.dev...000.", y6 = "u.s..re..multifamily.loans...000.", 
        y7 = "u.s..re..comm.re.nonfarm.nonres....000.", data.t = data3, t = tt, f = 400, sign.list = c(1, 
            -1, 1, -1))
}
summary(y.mat.nco[, 12])
# min. 1st qu.  median mean 3rd qu.  max.  -4.1920 -0.4727 -0.2576 -0.5270 -0.1280 0.1652


####### stopped here ###################



################################## nco 12: loans to foreign governments foreign government 4643 4627 2081
################################## co..lns.to.non.u.s..gov...official.institutions...000.'
#'rec..lns.to.non.u.s..gov...official.institutions...000.' 
#'con..non.u.s..government.loans...000.'  
# chargeoffs co: lns to non-u.s. gov & official institutions ($000) 142714 recoveries rec:
# lns to non-u.s. gov & official institutions ($000) 142748 balance con: non-u.s. government
# loans ($000) 142422


for (tt in (1:t)) {
    y.mat.nco[tt, 13] <- ppnr2.aggregate.t.sign(x1 = "co..lns.to.non.u.s..gov...official.institutions...000.", 
        x2 = "rec..lns.to.non.u.s..gov...official.institutions...000.", y = "con..non.u.s..government.loans...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}
summary(y.mat.nco[, 13])


#################### nco 13: agriculture chargeoffs co: agricultural production loans ($000) 142715 recoveries
#################### rec: agricultural production loans ($000) 142749 balance con: agricultural prod loans
#################### ($000) 142419

#'co..agricultural.production.loans...000.'                
#'rec..agricultural.production.loans...000.'
#'con..agricultural.prod.loans...000.'

for (tt in (1:t)) {
    y.mat.nco[tt, 14] <- ppnr2.aggregate.t.sign(x1 = "co..agricultural.production.loans...000.", 
        x2 = "rec..agricultural.production.loans...000.", y = "con..agricultural.prod.loans...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}
summary(y.mat.nco[, 14])


################################## nco 14: loans to depository institutions

# depository institutions chargeoffs co: total lns to dep institutions & acceptances ($000)
# 142711 recoveries rec: total lns to dep institutions & acceptances ($000) 142745 balance
# con: loans to depository institutions ($000) 142420
"co..total.lns.to.dep.institutions...acceptances...000."
"rec..lns.to.non.u.s..gov...official.institutions...000."
"con..loans.to.depository.institutions...000."


for (tt in (1:t)) {
    y.mat.nco[tt, 15] <- ppnr2.aggregate.t.sign(x1 = "co..total.lns.to.dep.institutions...acceptances...000.", 
        x2 = "rec..lns.to.non.u.s..gov...official.institutions...000.", y = "con..loans.to.depository.institutions...000.", 
        t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}
summary(y.mat.nco[, 15])


################################## nco 15 other loans

# chargeoffs co: all other loans ($000) 142705 recoveries rec: all other loans ($000) 142739
# balance other loans ($000) 142423


for (tt in (1:t)) {
    y.mat.nco[tt, 16] <- ppnr2.aggregate.t.sign(x1 = "co..all.other.loans...000.", x2 = "rec..all.other.loans...000.", 
        y = "other.loans...000.", t = tt, data.t = data3, sign.list = c(1, -1), f = 400)
}

y.mat.nco <- cbind(y.mat.nco, return.on.trading.assets)
summary(y.mat.nco)
write.csv(y.mat.nco, file = paste(getwd(), "response.nco.csv", sep = ""))

response.ppnr <- rbind(y.mat.200.ppnr, y.mat.201.ppnr)
write.csv(response.ppnr, file = paste(getwd(), "response.ppnr.csv", sep = ""))

dim(y.mat.nco)
# nco.stepoff=y.mat.nco[which(y.mat.nco[,'time.trend']==t/4),]
# ppnr.stepoff=response.ppnr[which(response.ppnr[,'time.trend']==t/4),]
# write.csv(nco.stepoff,file='h:/ey/class model/forecast/data/input/nco.stepoff.csv')
# write.csv(ppnr.stepoff,file='h:/ey/class model/forecast/data/input/ppnr.stepoff.csv') 
