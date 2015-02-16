
## note that the balance sheets and asset share is used for ppnr model only. no need for nco
## model.

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

setwd("c:/ppnr.quant.repo/class_model/data/")
data_asset <- read.csv("industry_asset_data.csv")

data_asset$year <- as.numeric(substr(data_asset$period, 1, 4))
data_asset$qt <- as.numeric(substr(data_asset$period, 6, 6))
data_asset$time.trend <- data_asset$year - 1991 + 0.25 * data_asset$qt



########## construct the balance
balance.ratio.mat.200 <- matrix(na, nrow = nrow(data_200), ncol = 11)
colnames(balance.ratio.mat.200) <- c("time.trend", "bank", "snl.institution.key", "residential.re.loans.ratio", 
    "commercial.re.loans.ratio", "ci.loans.ratio", "credit.card.loans.ratio", "trading.assets.ratio", 
    "securities.ratio", "asset.share", "risky.afs.ratio")
balance.ratio.mat.200[, 1] <- data_200$time.trend
balance.ratio.mat.200[, 2] <- data_200$bank.x
balance.ratio.mat.200[, 3] <- data_200$snl.institution.key

balance.ratio.mat.201 <- matrix(na, nrow = t, ncol = 11)
colnames(balance.ratio.mat.201) <- c("time.trend", "bank", "snl.institution.key", "residential.re.loans.ratio", 
    "commercial.re.loans.ratio", "ci.loans.ratio", "credit.card.loans.ratio", "trading.assets.ratio", 
    "securities.ratio", "asset.share", "risky.afs.ratio")
balance.ratio.mat.201[, 1] <- (1:t)/4
balance.ratio.mat.201[, 2] <- "bank201"


####### balance ratios calculation for top 200 banks
####### balance.ratio.mat.200[,4]=100*data_200$u.s..re..tot.cl.end.1.4.family...000./data_200$avg.earning.assets...000.
####### u.s..re..tot.cl.end.1.4.family...000. ->u.s..re..total.1.4.fmly...000.
balance.ratio.mat.200[, 4] <- 100 * data_200$u.s..re..total.1.4.fmly...000./data_200$avg.earning.assets...000.
balance.ratio.mat.200[, 5] <- 100 * data_200$con..total.real.estate.loans...000./data_200$avg.earning.assets...000.
balance.ratio.mat.200[, 6] <- 100 * data_200$con..tot.comm...ind.loans...000./data_200$avg.earning.assets...000.
balance.ratio.mat.200[, 7] <- 100 * data_200$con..credit.cards...rel.plans...000./data_200$avg.earning.assets...000.
balance.ratio.mat.200[, 8] <- 100 * data_200$total.trading.assets...000./data_200$avg.earning.assets...000.
balance.ratio.mat.200[, 9] <- 100 * data_200$total.securities...000./data_200$avg.earning.assets...000.


####### balance ratios calculation for bank 201

ppnr.aggregate.t <- function(x1, y, data.t = data_201, f = 400, t = t) {
    data <- data.t[which(data.t$time.trend == t/4), ]
    x1 <- which(names(data) == x1)
    x2 <- which(names(data) == y)
    sumlist <- which((!is.na(data[, x1])) * (data[, x2] != 0) == 1)
    data1 <- data[sumlist, c(x1, x2)]
    ratio <- sum(data1[, 1])/sum(data1[, 2]) * f
    print(ratio)
    return(ratio)
}

for (tt in (1:t)) {
    balance.ratio.mat.201[tt, 4] <- ppnr.aggregate.t(x1 = "u.s..re..tot.cl.end.1.4.family...000.", 
        y = "avg.earning.assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 5] <- ppnr.aggregate.t(x1 = "con..total.real.estate.loans...000.", 
        y = "avg.earning.assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 6] <- ppnr.aggregate.t(x1 = "con..tot.comm...ind.loans...000.", 
        y = "avg.earning.assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 7] <- ppnr.aggregate.t(x1 = "con..credit.cards...rel.plans...000.", 
        y = "avg.earning.assets...000.", data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 8] <- ppnr.aggregate.t(x1 = "total.trading.assets...000.", y = "avg.earning.assets...000.", 
        data.t = data_201, t = tt, f = 400)
    
    balance.ratio.mat.201[tt, 9] <- ppnr.aggregate.t(x1 = "total.securities...000.", y = "avg.earning.assets...000.", 
        data.t = data_201, t = tt, f = 400)
    
}


f.zero <- function(x) {
    return(ifelse(!is.na(x), x, 0))
}
########################## code run skips from here ########################## nonrisky portfolio >=2q09- numerator
########################## afs(f):us treasury secs ($000) afs(f):govt ag secs ($000) afs(f):govt-spons ag ($000)
########################## afs(f): pass-through rmbs: guar by gnma ($000) afs(f): pass-through rmbs: issd by fnma and
########################## fhlmc ($000) afs(f): other rmbs: issd or guar by gses ($000) afs(f):other rmbs:coll by mbs
########################## issd or guar by gses ($000) afs(f): pass-through cmbs issd or guar by gses ($000) afs(f):
########################## other cmbs issd or guar by gses ($000) afs(c):us treasury secs ($000) afs(c):govt ag secs
########################## ($000) afs(c):govt-spons ag ($000) afs(c): pass-through rmbs: guar by gnma ($000) afs(c):
########################## pass-through rmbs: issd by fnma and fhlmc ($000) afs(c): other rmbs: issd or guar by gses
########################## ($000) afs(c):other rmbs:coll by mbs issd or guar by gses ($000) afs(c): pass-through cmbs
########################## issd or guar by gses ($000) afs(c): other cmbs issd or guar by gses ($000)



# nonrisky portfolio <2q09 -numerator afs(f):us treasury secs ($000) afs(f):govt ag secs
# ($000) afs(f):govt-spons ag ($000) afs(f):mbs gnma (historical) ($000) afs(f):mbs sp ag
# (historical) ($000) afs(f):cmos (fnma) (historical) ($000) afs(c):us treasury secs ($000)
# afs(c):govt ag secs ($000) afs(c):govt-spons ag ($000) afs(c):mbs gnma (historical) ($000)
# afs(c):mbs sp ag (historical) ($000) afs(c):cmos (fnma) (historical) ($000)



# nonrisky.afs.post=data_200$afs.f..us.treasury.secs...000.+
# data_200$afs.f..govt.ag.secs...000.+ data_200$afs.f..govt.spons.ag...000.+
# data_200$afs.f...pass.through.rmbs..guar.by.gnma...000.+
# data_200$afs.f...pass.through.rmbs..issd.by.fnma.and.fhlmc...000.+
# data_200$afs.f...other.rmbs..issd.or.guar.by.gses...000.+
# data_200$afs.f..other.rmbs.coll.by.mbs.issd.or.guar.by.gses...000.+
# data_200$afs.f...pass.through.cmbs.issd.or.guar.by.gses...000.+
# data_200$afs.f...other.cmbs.issd.or.guar.by.gses...000.+
# data_200$afs.c..us.treasury.secs...000.+ data_200$afs.c..govt.ag.secs...000.+
# data_200$afs.c..govt.spons.ag...000.+
# data_200$afs.c...pass.through.rmbs..guar.by.gnma...000.+
# data_200$afs.c...pass.through.rmbs..issd.by.fnma.and.fhlmc...000.+
# data_200$afs.c...other.rmbs..issd.or.guar.by.gses...000.+
# data_200$afs.c..other.rmbs.coll.by.mbs.issd.or.guar.by.gses...000.+
# data_200$afs.c...pass.through.cmbs.issd.or.guar.by.gses...000.+
# data_200$afs.c...other.cmbs.issd.or.guar.by.gses...000.

nonrisky.afs.prior <- data_200$afs.f..us.treasury.secs...000. + data_200$afs.f..govt.ag.secs...000. + 
    data_200$afs.f..govt.spons.ag...000. + data_200$afs.f..mbs.gnma..historical....000. + data_200$afs.f..mbs.sp.ag..historical....000. + 
    data_200$afs.f..cmos..fnma...historical....000. + data_200$afs.c..us.treasury.secs...000. + 
    data_200$afs.c..govt.ag.secs...000. + data_200$afs.c..govt.spons.ag...000. + data_200$afs.c..mbs.gnma..historical....000. + 
    data_200$afs.c..mbs.sp.ag..historical....000. + data_200$afs.c..cmos..fnma...historical....000.


######## denominator total afs securities fv ($000) total securities afs bv ($000)


nonrisky.afs.post <- f.zero(data_200$afs.f..us.treasury.secs...000.) + f.zero(data_200$afs.f..govt.ag.secs...000.) + 
    f.zero(data_200$afs.f..govt.spons.ag...000.) + f.zero(data_200$afs.f...pass.through.rmbs..guar.by.gnma...000.) + 
    f.zero(data_200$afs.f...pass.through.rmbs..issd.by.fnma.and.fhlmc...000.) + f.zero(data_200$afs.f...other.rmbs..issd.or.guar.by.gses...000.) + 
    f.zero(data_200$afs.f..other.rmbs.coll.by.mbs.issd.or.guar.by.gses...000.) + f.zero(data_200$afs.f...pass.through.cmbs.issd.or.guar.by.gses...000.) + 
    f.zero(data_200$afs.f...other.cmbs.issd.or.guar.by.gses...000.) + f.zero(data_200$afs.c..us.treasury.secs...000.) + 
    f.zero(data_200$afs.c..govt.ag.secs...000.) + f.zero(data_200$afs.c..govt.spons.ag...000.) + 
    f.zero(data_200$afs.c...pass.through.rmbs..guar.by.gnma...000.) + f.zero(data_200$afs.c...pass.through.rmbs..issd.by.fnma.and.fhlmc...000.) + 
    f.zero(data_200$afs.c...other.rmbs..issd.or.guar.by.gses...000.) + f.zero(data_200$afs.c..other.rmbs.coll.by.mbs.issd.or.guar.by.gses...000.) + 
    f.zero(data_200$afs.c...pass.through.cmbs.issd.or.guar.by.gses...000.) + f.zero(data_200$afs.c...other.cmbs.issd.or.guar.by.gses...000.)

nonrisky.afs.prior <- f.zero(data_200$afs.f..us.treasury.secs...000.) + f.zero(data_200$afs.f..govt.ag.secs...000.) + 
    f.zero(data_200$afs.f..govt.spons.ag...000.) + f.zero(data_200$afs.f..mbs.gnma..historical....000.) + 
    f.zero(data_200$afs.f..mbs.sp.ag..historical....000.) + f.zero(data_200$afs.f..cmos..fnma...historical....000.) + 
    f.zero(data_200$afs.c..us.treasury.secs...000.) + f.zero(data_200$afs.c..govt.ag.secs...000.) + 
    f.zero(data_200$afs.c..govt.spons.ag...000.) + f.zero(data_200$afs.c..mbs.gnma..historical....000.) + 
    f.zero(data_200$afs.c..mbs.sp.ag..historical....000.) + f.zero(data_200$afs.c..cmos..fnma...historical....000.)

afs.total <- f.zero(data_200$total.securities.afs.bv...000.) + f.zero(data_200$total.afs.securities.fv...000.)
balance.ratio.mat.200[, 11] <- ifelse(data_200$time.trend >= (2009 - 1991 + 0.5), 100 - 100 * 
    nonrisky.afs.post/afs.total, 100 - 100 * nonrisky.afs.prior/afs.total)
summary(balance.ratio.mat.200[, 11])
which(balance.ratio.mat.200[, 11] < 0)  # five observations have negative values thus are deleted for model fitting
balance.ratio.mat.200[which(balance.ratio.mat.200[, 11] < 0), 11] <- na


for (tt in (1:t)) {
    data <- data_201[data_201$time.trend == tt/4, ]
    nonrisky.afs.post.tt <- sum(f.zero(data$afs.f..us.treasury.secs...000.) + f.zero(data$afs.f..govt.ag.secs...000.) + 
        f.zero(data$afs.f..govt.spons.ag...000.) + f.zero(data$afs.f...pass.through.rmbs..guar.by.gnma...000.) + 
        f.zero(data$afs.f...pass.through.rmbs..issd.by.fnma.and.fhlmc...000.) + f.zero(data$afs.f...other.rmbs..issd.or.guar.by.gses...000.) + 
        f.zero(data$afs.f..other.rmbs.coll.by.mbs.issd.or.guar.by.gses...000.) + f.zero(data$afs.f...pass.through.cmbs.issd.or.guar.by.gses...000.) + 
        f.zero(data$afs.f...other.cmbs.issd.or.guar.by.gses...000.) + f.zero(data$afs.c..us.treasury.secs...000.) + 
        f.zero(data$afs.c..govt.ag.secs...000.) + f.zero(data$afs.c..govt.spons.ag...000.) + 
        f.zero(data$afs.c...pass.through.rmbs..guar.by.gnma...000.) + f.zero(data$afs.c...pass.through.rmbs..issd.by.fnma.and.fhlmc...000.) + 
        f.zero(data$afs.c...other.rmbs..issd.or.guar.by.gses...000.) + f.zero(data$afs.c..other.rmbs.coll.by.mbs.issd.or.guar.by.gses...000.) + 
        f.zero(data$afs.c...pass.through.cmbs.issd.or.guar.by.gses...000.) + f.zero(data$afs.c...other.cmbs.issd.or.guar.by.gses...000.))
    
    nonrisky.afs.prior.tt <- sum(f.zero(data$afs.f..us.treasury.secs...000.) + f.zero(data$afs.f..govt.ag.secs...000.) + 
        f.zero(data$afs.f..govt.spons.ag...000.) + f.zero(data$afs.f..mbs.gnma..historical....000.) + 
        f.zero(data$afs.f..mbs.sp.ag..historical....000.) + f.zero(data$afs.f..cmos..fnma...historical....000.) + 
        f.zero(data$afs.c..us.treasury.secs...000.) + f.zero(data$afs.c..govt.ag.secs...000.) + 
        f.zero(data$afs.c..govt.spons.ag...000.) + f.zero(data$afs.c..mbs.gnma..historical....000.) + 
        f.zero(data$afs.c..mbs.sp.ag..historical....000.) + f.zero(data$afs.c..cmos..fnma...historical....000.))
    
    afs.total.tt <- sum(f.zero(data$total.securities.afs.bv...000.) + f.zero(data$total.afs.securities.fv...000.))
    
    balance.ratio.mat.201[tt, 11] <- ifelse(tt >= 4 * (2009 - 1991 + 0.5), 100 - 100 * nonrisky.afs.post.tt/afs.total.tt, 
        100 - 100 * nonrisky.afs.prior.tt/afs.total.tt)
}


# asset share (firm assets as a % of industry) 2170

#'total.assets...000.'
for (tt in (1:t)) {
    row.list <- which(data_200$time.trend == tt/4)
    data <- data_200[data_200$time.trend == tt/4, ]
    period <- as.character(data$period[1])
    total.assets <- data_asset[which(data_asset[, 1] == period), 2]
    balance.ratio.mat.200[row.list, 10] <- 100 * data$total.assets...000./total.assets
    balance.ratio.mat.201[tt, 10] <- 100 * sum(na.rm = true, data_201$total.assets...000.[data_201$time.trend == 
        tt/4])/total.assets
}

# afs risky ratio
summary(as.numeric(balance.ratio.mat.201[, 10]))

balance.ratio.mat <- rbind(balance.ratio.mat.201, balance.ratio.mat.200)
write.csv(balance.ratio.mat, file = paste(getwd(), "balanceratio.csv", sep = "")) 
