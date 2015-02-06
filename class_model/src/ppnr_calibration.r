# this file is to generate the PPNR response variables for the historical data, which will be used for recalibration purpose.


T=4*(2014+0.75-1991)
setwd("H:/EY/CLASS Model/Calibration/Data")
list.files()
data.PPNR=read.csv(skip=1,"CLASS_PPNR_Calibration_Data.csv")
data.NCO=read.csv(skip=1,"CLASS_NCO_Calibration_Data.csv")
intersect(colnames(data.PPNR), colnames(data.NCO))


#############################################################
#### two columns 
a1=data.PPNR$Con..Total.Real.Estate.Loans...000.-data.NCO$Con..Total.Real.Estate.Loans...000.
a2=data.PPNR$Con..Tot.Comm...Ind.Loans...000.-data.NCO$Con..Tot.Comm...Ind.Loans...000.
summary(a1)
summary(a2)
data.PPNR$Con..Total.Real.Estate.Loans...000.[102586 :102600]
data.NCO$Con..Total.Real.Estate.Loans...000.[102586 :102600]
### given that in the majority of the cases NCO data have the larger values than PPNR, 
#############################keep NCO values instead of PPNR
#############################################################

data1.PPNR=data.PPNR[,-which((colnames(data.PPNR) %in% c("Con..Total.Real.Estate.Loans...000.", "Con..Tot.Comm...Ind.Loans...000.")))]
data1=merge(data1.PPNR,data.NCO, by=c("Period","Bank","SNL.Institution.Key"))

names(data1)
dim(data1)  
flag.data=read.csv("CLASS_Institution_Flag_Data.csv")
sum(flag.data[,3])  #200, all the top banks are within 
tabulate(flag.data[,3])
sum(as.numeric(as.character(flag.data[,3])),na.rm=TRUE)  # 200

summary(data1$Total.Assets...000.)
class(data1$Total.Assets...000.)

data2=data1
data2$Year=as.numeric(substr(data1$Period,1,4))
data2$Qt=as.numeric(substr(data1$Period,6,6))
data2$Time.trend=data2$Year-1991+0.25*data2$Qt

#To generate the average earning assets info for all the banks

data3=merge(flag.data,data2, by="SNL.Institution.Key", all=TRUE)
dim(data3)==dim(data2)
names(data3)
data3$Avg.Earning.Assets...000.=
ifelse(!is.na(data3$Interest.Bearing.Balances...000.),data3$Interest.Bearing.Balances...000.,0)+
ifelse(!is.na(data3$Tot.Fed.Funds...Reverse.Repos...000.), data3$Tot.Fed.Funds...Reverse.Repos...000., 0)+
ifelse(!is.na(data3$Total.Securities...000.), data3$Total.Securities...000.,0)+
ifelse(!is.na(data3$Gross.Loans...Leases...000.), data3$Gross.Loans...Leases...000.,0)+
ifelse(!is.na(data3$Total.Trading.Assets...000.), data3$Total.Trading.Assets...000.,0)


data3$Other.RE.Loans=
ifelse(!is.na(data3$Con..Total.Real.Estate.Loans...000.), data3$Con..Total.Real.Estate.Loans...000.,0) -
ifelse(!is.na(data3$U.S..RE..Cl.end.Frst.Lien.1.4...000.), data3$U.S..RE..Cl.end.Frst.Lien.1.4...000.,0) -
ifelse(!is.na(data3$U.S..RE..Cl.end.Jr.Lien.1.4...000.), data3$U.S..RE..Cl.end.Jr.Lien.1.4...000.,0) -   
ifelse(!is.na(data3$U.S..RE..Rev.1.4.Fam..HE.Lines....000.), data3$U.S..RE..Rev.1.4.Fam..HE.Lines....000.,0)-
ifelse(!is.na(data3$U.S..RE..Constr...Land.Dev...000.), data3$U.S..RE..Constr...Land.Dev...000.,0) -
ifelse(!is.na(data3$U.S..RE..Multifamily.Loans...000.),data3$U.S..RE..Multifamily.Loans...000.,0) -
ifelse(!is.na(data3$U.S..RE..Comm.RE.Nonfarm.NonRes....000.),data3$U.S..RE..Comm.RE.Nonfarm.NonRes....000.,0)

summary(data3$Other.RE.Loans)
### data cleaning
data3$Other.RE.Loans = ifelse(data3$Other.RE.Loans<0,0,data3$Other.RE.Loans)
summary(data3$Other.RE.Loans)
length(which(data3$Other.RE.Loans<0))  # 5

## split data into two sets: largest 200 banks by assets and the remaining 
data_200=data3[which(data3$Flag==1),]
data_201=data3[-which(data3$Flag==1),]

################################################################################################
########### start to construct the response responses for both PPNR and NCO ####################
################################################################################################

Y.mat.200.PPNR=matrix(NA,ncol=10,nrow=nrow(data_200))
colnames(Y.mat.200.PPNR)=c("Time.trend",
"Net.Interest.Margin",
"Noninterest.Nontrading.Income.Ratio",
"Return.on.Trading.Assets",
"Compensation.Noninterest.Expense.Ratio",
"Fixed.Asset.Noninterest.Expense.Ratio",
"Other.Noninterest.Expense.Ratio",
"Return.on.AFS.Securities",
"Bank","SNL.Institution.Key")

Y.mat.200.PPNR[,"Bank"]=data_200[,"Bank.x"]
Y.mat.200.PPNR[,"SNL.Institution.Key"]=data_200$SNL.Institution.Key

Y.mat.201.PPNR=matrix(NA,ncol=10,nrow=T)
colnames(Y.mat.201.PPNR)=c("Time.trend",
"Net.Interest.Margin",
"Noninterest.Nontrading.Income.Ratio",
"Return.on.Trading.Assets",
"Compensation.Noninterest.Expense.Ratio",
"Fixed.Asset.Noninterest.Expense.Ratio",
"Other.Noninterest.Expense.Ratio",
"Return.on.AFS.Securities",
"Bank","SNL.Institution.Key")


Y.mat.NCO=matrix(NA,ncol=16,nrow=T)
colnames(Y.mat.NCO)=c("Time.trend",
"FirstLien.Residential.Real.Estate",
"Junior.Lien.Residential.Real.Estate",
"HELOC.Residential.Real.Estate",
"Construction.Commercial.Real.Estate",
"Multifamily.Commercial.Real.Estate",
"NonFarm.NonResidential.CRE",
"Credit.Card",
"Other.Consumer",
"CI",
"Leases",
"Other.Real.Estate",
"Loans.to.Foreign.Governments",
"Agriculture",
"Loans.to.Depository.Institutions",
"Other")


Y.mat.200.PPNR[,1]=data_200$Time.trend
Y.mat.201.PPNR[,1]=(1:T)/4
Y.mat.NCO[,1]=(1:T)/4

#PPNR item 1; Bank 1- 200

#Y.mat.200.PPNR[,2]=ifelse(  !is.na(data_200$Net.Interest.Income...000./data_200$Avg.Earning.Assets...000.),
#data_200$Net.Interest.Income...000./data_200$Avg.Earning.Assets...000.*400, 0)

Y.mat.200.PPNR[,2]=data_200$Net.Interest.Income...000./data_200$Avg.Earning.Assets...000.*400

# define the aggregation function to calculate the PPNR/NCO response for the hypothetical bank 201
PPNR.aggregate.t<-function(x,y,data.t=data_201,f=400,t=T ) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x)
  X2=which(names(data)==y)   
  sumlist=which((!is.na(data[,X1])) * (!is.na(data[,X2])) * (data[,X2] !=0)==1)
  data1= data[sumlist,c(X1,X2)]
  ratio= sum(data1[,1])/sum(data1[,2])*f
  print(ratio)  
  return(ratio=ratio)  
}
 
#PPNR item 1 - bank 201

  for (tt in (1:T))
{ 
	Y.mat.201.PPNR[tt,2]=PPNR.aggregate.t(x="Net.Interest.Income...000.", y="Avg.Earning.Assets...000.", data.t=data_201,t=tt,f=400)
}

summary(Y.mat.200.PPNR[,2])
summary(Y.mat.201.PPNR[,2])

##############################################################

#PPNR item 2- Noninterest.Nontrading.Income.Ratio
##############  

Y.mat.200.PPNR[,3]=(data_200$Total.Noninterest.Income...000.-ifelse(!is.na(data_200$NII..Trading.Revenue...000.),
data_200$NII..Trading.Revenue...000.,0))/data_200$Total.Assets...000.*400

PPNR2.aggregate.t.sign<- function(x1,x2, y,data.t=data_201,f=400,t=T,sign.list=c(1,-1)) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x1)
  X2=which(names(data)==x2)
  X3=which(names(data)==y)

  sumlist=which( (!is.na(data[,X1])) * (!is.na(data[,X2]))* (!is.na(data[,X3])) * (data[,X3] !=0)==1)
  data1= data[sumlist,c(X1,X2,X3)]
  ratio= sum(sign.list[1]*data1[,1]+sign.list[2]*data1[,2])/sum(data1[,3])*f
  print(ratio)  
  return(ratio)  
}

  for (tt in (1:T))
{ 
	Y.mat.201.PPNR[tt,3]=PPNR2.aggregate.t.sign(x1="Total.Noninterest.Income...000.", x2="NII..Trading.Revenue...000.",
	y="Total.Assets...000.", data.t=data_201,t=tt,f=400,sign.list=c(1,-1))
}

summary(Y.mat.201.PPNR[,3])
summary(Y.mat.200.PPNR[,3])

##############################################################

#############for PPNR item Return on Trading Assets, we also aggregate all the banks for each quarter
#PPNR_item 3 Return.on.Trading.Assets

Return.on.Trading.Assets=array(NA, T)
	for (tt in (1:T))
{
	Return.on.Trading.Assets[tt]=PPNR.aggregate.t(x="NII..Trading.Revenue...000.", y="Total.Trading.Assets...000.", 
	data.t=data3,t=tt,f=400)

}


##############################################################
#PPNR item 4:  Compensation.Noninterest.Expense.Ratio
Y.mat.200.PPNR[,5]=data_200$NIE..Salary...Benefits...000./data_200$Total.Assets...000.*400
 	for (tt in (1:T))
{ 
	Y.mat.201.PPNR[tt,5]=PPNR.aggregate.t(x="NIE..Salary...Benefits...000.",y="Total.Assets...000.", 
	data.t=data_201,t=tt, f=400)
}

summary(Y.mat.200.PPNR[,5])
summary(Y.mat.201.PPNR[,5])


##############################################################
#PPNR item 5: Fixed.Asset.Noninterest.Expense.Ratio=1/PPNR_200$Total.Assets...000.*400*PPNR_200$NIE..Premises...Fixed.Assets...000.
Y.mat.200.PPNR[,6]=data_200$NIE..Premises...Fixed.Assets...000./data_200$Total.Assets...000.*400
 	for (tt in (1:T))
{ 
	Y.mat.201.PPNR[tt,6]=PPNR.aggregate.t(x="NIE..Premises...Fixed.Assets...000.",y="Total.Assets...000.", 
	data.t=data_201,t=tt, f=400)
}

summary(Y.mat.200.PPNR[,6])
summary(Y.mat.201.PPNR[,6])


##############################################################
########PPNR item 6: Other.Noninterest.Expense.Ratio

Y.mat.200.PPNR[,7]=(data_200$Total.Noninterest.Income...000.-data_200$NIE..Salary...Benefits...000.-
data_200$NIE..Premises...Fixed.Assets...000.)/data_200$Total.Assets...000.*400  

PPNR3.aggregate.t.sign<- function(x1,x2,x3, y,data.t=data_201,f=400,t=T,sign.list=c(1,-1,-1)) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x1)
  X2=which(names(data)==x2)
  X3=which(names(data)==x3)
  X4=which(names(data)==y)
  sumlist=which( (!is.na(data[,X1])) * (!is.na(data[,X2]))* (!is.na(data[,X3]))*(!is.na(data[,X4])) *(data[,X4] !=0)==1)
  data1= data[sumlist,c(X1,X2,X3,X4)]
  ratio= sum(sign.list[1]*data1[,1]+sign.list[2]*data1[,2]+sign.list[3]*data1[,3])/sum(data1[,4])*f
  print(ratio)  
  return(ratio)  
}

for (tt in (1:T))
{ 
	Y.mat.201.PPNR[tt,7]=PPNR3.aggregate.t.sign(x1="Total.Noninterest.Income...000.", 
	x2="NIE..Salary...Benefits...000.",x3="NIE..Premises...Fixed.Assets...000.",
	y="Total.Assets...000.", data.t=data_201,t=tt, f=400,sign.list=c(1,-1,-1))
}

summary(Y.mat.200.PPNR[,7])
summary(Y.mat.201.PPNR[,7])


##############################################################
########PPNR item 7: Return.on.AFS.Securities 

Y.mat.200.PPNR[,8]=data_200$Gain.Realized.Gns.AFS.Secs...000./(data_200$Total.AFS.Securities.FV...000.+
data_200$Total.Securities.AFS.BV...000.)*400

##############################################################
PPNR2A.aggregate.t.sign<- function(x,y1,y2,data.t=data_201,f=400,t=T,sign.list=c(1,+1)) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x)
  X2=which(names(data)==y1)
  X3=which(names(data)==y2)

  sumlist=which( (!is.na(data[,X1])) * (!is.na(data[,X2]))* (!is.na(data[,X3])) * ((data[,X2]+data[,X3]) !=0)==1)
  data1= data[sumlist,c(X1,X2,X3)]
  ratio= sum(data1[,1])/sum(sign.list[1]*data1[,2]+sign.list[2]*data1[,3])*f
  print(ratio)  
  return(ratio)  
}


 	for (tt in (1:T))
{ 
	Y.mat.201.PPNR[tt,8]=PPNR2A.aggregate.t.sign(x="Gain.Realized.Gns.AFS.Secs...000.",
	y1="Total.AFS.Securities.FV...000.", y2="Total.Securities.AFS.BV...000.",
	data.t=data3,t=tt, sign.list=c(1,1), f=400)
}

summary(Y.mat.200.PPNR[,8])
summary(as.numeric(Y.mat.201.PPNR[,8]))
summary(Y.mat.200.PPNR)
summary(Y.mat.201.PPNR)


############ Note that NCO, we aggregate all the banks together and to generate one observation within one quarter.


###################### define functions to calculate the NCO time series  #########################

### define the fucntion to calcualte NCO ratio #8
PPNR6.aggregate.t.sign<- function(x1,x2,x3,x4, y1,y2,data.t=data3,t=T, f=400,sign.list=c(1,-1,1,-1,1,1)) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x1)
  X2=which(names(data)==x2)
  X3=which(names(data)==x3)
  X4=which(names(data)==x4)
  Y1=which(names(data)==y1)
  Y2=which(names(data)==y2)

  sumlist=which(    (!is.na(data[,X1])) * (!is.na(data[,X2]))* (!is.na(data[,X3])) * (!is.na(data[,X4]))*
  (!is.na(data[,Y1])) *(!is.na(data[,Y2]))*(  (data[,Y1] +data[,Y2] )!=0)     ==1)
  data1= data[sumlist,c(X1,X2,X3,X4, Y1, Y2)]
  ratio= sum(sign.list[1]*data1[,1]+sign.list[2]*data1[,2]+sign.list[3]*data1[,3]+sign.list[4]* data1[,4])/sum(sign.list[5]*data1[,5]+sign.list[6]*data1[,6])*f
  print(ratio)  
  return(ratio)  
}


############################ NCO response items #######################################


############ NCO item 1: First.Lien.Residential.Real.Estate
#Y.mat.NCO[1:200,8]=(data_200$CO..U.S..RE..Close.end.First.Lien.1.4.Family...000.-
#data_200$Rec..U.S..RE..Close.end.First.Lien.1.4.Family...000.)/data_200$U.S..RE..Cl.end.Frst.Lien.1.4...000.*400

for (tt in (1:T))
{
	Y.mat.NCO[tt,2]= PPNR2.aggregate.t.sign(x1="CO..U.S..RE..Close.end.First.Lien.1.4.Family...000.",
	x2="Rec..U.S..RE..Close.end.First.Lien.1.4.Family...000.",
	y="U.S..RE..Cl.end.Frst.Lien.1.4...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)
	
}
############ NCO item 2:Junior Lien Residential Real Estate*

#Y.mat.NCO[1:200,9]=(data_200$CO..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.- 
#data_200$Rec..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.)/data_200$U.S..RE..Cl.end.Jr.Lien.1.4...000.*400

for (tt in (1:T))
{
	Y.mat.NCO[tt,3]= PPNR2.aggregate.t.sign(x1="CO..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.", 
	x2="Rec..U.S..RE..Close.end.Jr.Lien.1.4.Family...000.",
	y="U.S..RE..Cl.end.Jr.Lien.1.4...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	
}

############ NCO item 3: HELOC.Residential.Real.Estate
#	CO: U.S. RE: Revolving 1-4 Family (HE Lines) ($000)	142695
#	Recoveries	Rec: U.S. RE: Revolving 1-4 Family (HE Lines) ($000)	142729
#	Balance	U.S. RE: Rev 1-4 Fam (HE Lines) ($000)	142403

for (tt in (1:T))
{
	Y.mat.NCO[tt,4]= PPNR2.aggregate.t.sign(x1="CO..U.S..RE..Revolving.1.4.Family..HE.Lines....000.", 
	x2="Rec..U.S..RE..Revolving.1.4.Family..HE.Lines....000.",
	y="U.S..RE..Rev.1.4.Fam..HE.Lines....000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	
}



###########  NCO 4: Construction Commercial Real Estate
#Chargeoffs	CO: U.S. RE: Construction & Land Development ($000)	142690
#Recoveries	Rec: U.S. RE: Construction & Land Development ($000)	142724
#Balance	U.S. RE: Constr & Land Dev ($000)	142398

for (tt in (1:T))
{	
	Y.mat.NCO[tt,5]= PPNR2.aggregate.t.sign(x1="CO..U.S..RE..Construction...Land.Development...000.", 
	x2="Rec..U.S..RE..Construction...Land.Development...000.",
	y="U.S..RE..Constr...Land.Dev...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	
}


########### NCO 5: Multifamily.Commercial.Real.Estate
#Chargeoffs	CO: U.S. RE: Multifamily ($000)	142696
#Recoveries	Rec: U.S. RE: Multifamily ($000)	142730
#Balance	U.S. RE: Multifamily Loans ($000)	142405

for (tt in (1:T))
{	
	Y.mat.NCO[tt,6]= PPNR2.aggregate.t.sign(x1="CO..U.S..RE..Multifamily...000.", 
	x2="Rec..U.S..RE..Multifamily...000.",
	y="U.S..RE..Multifamily.Loans...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	
}



# NCO 6: NonFarm.NonResidential.CRE
#CO..U.S..RE..Commercial...000."                          
#"Rec..U.S..RE..Commercial...000."
#"U.S..RE..Comm.RE.Nonfarm.NonRes....000." 

#Chargeoffs	CO: U.S. RE: Commercial ($000)	142699
#Recoveries	Rec: U.S. RE: Commercial ($000)	142733
#Balance	U.S. RE: Comm RE(Nonfarm/NonRes) ($000)	142408

for (tt in (1:T))
{	
	Y.mat.NCO[tt,7]= PPNR2.aggregate.t.sign(x1="CO..U.S..RE..Commercial...000.", 
	x2="Rec..U.S..RE..Commercial...000.",
	y="U.S..RE..Comm.RE.Nonfarm.NonRes....000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	
}


################ NCO 7: Credit Card
# "CO..Credit.Card.Loans...000."                            
# "Rec..Credit.Card.Loans...000." 
# "Con..Credit.Card.Loans...000." 

#Chargeoffs - > 2001	CO: Credit Card Loans ($000)	210814
#Chargeoffs - < 2001	CO: Credit Card & Related Plans (historical) ($000)	142709
#Recoveries - > 2001	Rec: Credit Card Loans ($000)	210848
#Recoveries - < 2001	Rec: Credit Card & Related Plans (historical) ($000)	142743
#Balance	Con: Credit Cards & Rel Plans ($000)	142417
for (tt in (1:T))
{	
	Y.mat.NCO[tt,8]= 
	ifelse(Y.mat.NCO[tt,1]>10,     
	PPNR2.aggregate.t.sign(x1="CO..Credit.Card.Loans...000.", 
	x2="Rec..Credit.Card.Loans...000.",
	y="Con..Credit.Cards...Rel.Plans...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400),
	PPNR2.aggregate.t.sign(x1="CO..Credit.Card...Related.Plans..historical....000.", 
	x2="Rec..Credit.Card...Related.Plans..historical....000.",
	y="Con..Credit.Cards...Rel.Plans...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400))
}

summary(Y.mat.NCO[,8])

############### NCO 8: Other Consumer
#CO..Other.Consumer.Loans...000."                         
#"REC..Other.Consumer.Loans...000."   
#Con..Other.Consumer.Loans...000."

#Chargeoffs	CO: Consumer Loans ($000)	210818
#Chargeoffs - > 2001	CO: Credit Card Loans ($000)	210814
#Chargeoffs - < 2001	CO: Credit Card & Related Plans (historical) ($000)	142709
#Recoveries	Rec: Consumer Loans ($000)	210852
#Recoveries - > 2001	Rec: Credit Card Loans ($000)	210848
#Recoveries - < 2001	Rec: Credit Card & Related Plans (historical) ($000)	142743
#Balance	Con: Tot Consumer Loans ($000)	142414
#	Con: Credit Cards & Rel Plans ($000)	142417
#Charge-offs	CO: Consumer Loans ($000) - [CO: Credit Card Loans ($000) OR CO: Credit Card & Related Plans (historical) ($000)]	210818 - [210814 OR 142709]
#Recoveries	REC: Consumer Loans ($000) - [REC: Credit Card Loans ($000) OR REC: Credit Card & Related Plans (historical) ($000)]	210852 - [210848 OR 142743]
#Balance	Con: Tot Consumer Loans ($000) - Con: Credit Cards & Rel Plans ($000)	142414 - 142417

for (tt in (1:T))
{	
	Y.mat.NCO[tt,9]= 
	ifelse(Y.mat.NCO[tt,1]>10,     
	PPNR6.aggregate.t.sign(x1="CO..Consumer.Loans...000.", x2="CO..Credit.Card.Loans...000.", 
	x3="Rec..Consumer.Loans...000.", x4="Rec..Credit.Card.Loans...000.",
	y1="Con..Tot.Consumer.Loans...000.",
	y2="Con..Credit.Cards...Rel.Plans...000.", t=tt, data.t=data3,
	sign.list=c(1,-1,-1,1,1,-1), f=400),
	PPNR6.aggregate.t.sign(x1="CO..Consumer.Loans...000.", x2="CO..Credit.Card...Related.Plans..historical....000.", 
	x3="Rec..Consumer.Loans...000.", x4="Rec..Credit.Card...Related.Plans..historical....000.",
	y1="Con..Tot.Consumer.Loans...000.",
	y2="Con..Credit.Cards...Rel.Plans...000.", t=tt, data.t=data3,
	sign.list=c(1,-1,-1,1,1,-1), f=400))
}

summary(Y.mat.NCO[,9])


############### NCO 9: Commercial and Industrial

#CO..Commercial...Industrial.Lns.U.S..Addressees...000."  
# "Rec..Commercial...Industrial.Lns.U.S..Addressees...000." 
# "CO..Commercial...Industrial.Lns.Non.U.S..Address...000." 
# "Rec..Commercial...Industrial.Lns.Non.U.S..Address...000."
# "Con.Comm...Ind.US.Addr...000."                           
# "Con.Comm...Ind.Non.US.Addr...000."  

#Chargeoffs	CO: Commercial & Industrial Lns U.S. Addressees ($000)	142703
#	CO: Commercial & Industrial Lns Non-U.S. Address ($000)	142704
#Recoveries	Rec: Commercial & Industrial Lns U.S. Addressees ($000)	142737
#	Rec: Commercial & Industrial Lns Non-U.S. Address ($000)	142738
#Balance	Con: Tot Comm & Ind Loans ($000)	142413

### define the fucntion to calcualte NCO ratio #9
PPNR5.aggregate.t.sign<- function(x1,x2,x3,x4, y,data.t=data3,t=T, f=400,sign.list=c(1,-1,1,-1,1)) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x1)
  X2=which(names(data)==x2)
  X3=which(names(data)==x3)
  X4=which(names(data)==x4)
  Y=which(names(data)==y)
    sumlist=which((!is.na(data[,X1])) * (!is.na(data[,X2]))* (!is.na(data[,X3])) * (!is.na(data[,X4]))*
  (!is.na(data[,Y])) *  (data[,Y] !=0) ==1)
  data1= data[sumlist,c(X1,X2,X3,X4, Y)]
  ratio= sum(sign.list[1]*data1[,1]+sign.list[2]*data1[,2]+sign.list[3]*data1[,3]+ sign.list[4]*data1[,4])/sum(data1[,5])*f
  print(ratio)  
  return(ratio)  
}

for (tt in (1:T))
{	
	 Y.mat.NCO[tt,10]=PPNR5.aggregate.t.sign(x1="CO..Commercial...Industrial.Lns.U.S..Addressees...000."
	,x2="Rec..Commercial...Industrial.Lns.U.S..Addressees...000." 
	,x3="CO..Commercial...Industrial.Lns.Non.U.S..Address...000." 
	,x4="Rec..Commercial...Industrial.Lns.Non.U.S..Address...000."
	,y="Con..Tot.Comm...Ind.Loans...000." ,data.t=data3,t=tt, f=400,sign.list=c(1,-1,1,-1,1))
}
summary(Y.mat.NCO[,10])


#################################NCO 10: leases
#CO: Total Lease Financing Receivables ($000)
#Rec: Total Lease Financing Receivables ($000)
#Con: Total Leases ($000)

for (tt in (1:T))
{	
	Y.mat.NCO[tt,11]= 
	PPNR2.aggregate.t.sign(x1="CO..Total.Lease.Financing.Receivables...000.", 
	x2="Rec..Total.Lease.Financing.Receivables...000.",
	y="Con..Total.Leases...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	  
}
summary(Y.mat.NCO[,11])

###################################NCO 11: Other Real Estate (0.48)

#Chargeoffs	CO: U.S. RE: Farm Loans ($000)	142698
#	CO: Loans Sec by Real Estate to Non-U.S. Address ($000)	142689
#Recoveries	Rec: U.S. RE: Farm Loans ($000)	142732
#	Rec: Loans Sec by Real Estate to Non-U.S. Address ($000)	142723
#Balance	Con: Total Real Estate Loans ($000)	142397
#	U.S. RE: Cl-end Frst Lien 1-4 ($000)	142401
#	U.S. RE: Cl-end Jr Lien 1-4 ($000)	142402
#	U.S. RE: Rev 1-4 Fam (HE Lines) ($000)	142403
#	U.S. RE: Constr & Land Dev ($000)	142398
#	U.S. RE: Multifamily Loans ($000)	142405
#	U.S. RE: Comm RE(Nonfarm/NonRes) ($000)	142408


PPNR11.aggregate.t.sign<- function(x1,x2,x3,x4, y1,y2,y3,y4,y5,y6,y7,
data.t=data3,t=T, f=400,sign.list=c(1,1,-1,-1)) {
  data=data.t[which(data.t$Time.trend==t/4),]
  X1=which(names(data)==x1)
  X2=which(names(data)==x2)
  X3=which(names(data)==x3)
  X4=which(names(data)==x4)
  Y1=which(names(data)==y1)
  Y2=which(names(data)==y2)
  Y3=which(names(data)==y3)
  Y4=which(names(data)==y4) 
  Y5=which(names(data)==y5)
  Y6=which(names(data)==y6)
  Y7=which(names(data)==y7)

    sumlist=which((!is.na(data[,X1])) * (!is.na(data[,X2]))* (!is.na(data[,X3])) * (!is.na(data[,X4]))*
  (!is.na(data[,Y1])) * (!is.na(data[,Y2])) * (!is.na(data[,Y3])) * (!is.na(data[,Y4])) *
 (!is.na(data[,Y5])) * (!is.na(data[,Y6])) * (!is.na(data[,Y7])) * 
 (    (data[,Y1]-data[,Y2]-data[,Y3]-data[,Y4]-data[,Y5]-data[,Y6]-data[,Y7])  !=0) ==1)
  data1= data[sumlist,c(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Y5,Y6,Y7)]
  ratio= sum(sign.list[1]*data1[,1]+sign.list[2]*data1[,2]+sign.list[3]*data1[,3]+ sign.list[4]*data1[,4])/
	sum(data1[,5]-data1[,6]-data1[,7]-data1[,8]-data1[,9]-data1[,10]-data1[,11])*f
  print(ratio)  
  return(ratio)  
}

for (tt in (1:T))
{	
	 Y.mat.NCO[tt,12]=PPNR11.aggregate.t.sign(x1="CO..U.S..RE..Farm.Loans...000."
	,x2="CO..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000." 
	,x3="Rec..U.S..RE..Farm.Loans...000." 
	,x4="Rec..Loans.Sec.by.Real.Estate.to.Non.U.S..Address...000."
	,y1="Con..Total.Real.Estate.Loans...000." 
	,y2="U.S..RE..Cl.end.Frst.Lien.1.4...000."
	,y3="U.S..RE..Cl.end.Jr.Lien.1.4...000."
	,y4="U.S..RE..Rev.1.4.Fam..HE.Lines....000."
	,y5="U.S..RE..Constr...Land.Dev...000."
	,y6="U.S..RE..Multifamily.Loans...000."
	,y7="U.S..RE..Comm.RE.Nonfarm.NonRes....000.",data.t=data3,t=tt, f=400,sign.list=c(1,-1,1,-1))
}
summary(Y.mat.NCO[,12])
#
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.1920 -0.4727 -0.2576 -0.5270 -0.1280  0.1652 


#######  stopped here ###################
#####################################



##################################NCO 12: Loans to Foreign Governments 
#Foreign government 	4643	4627	2081
#CO..Lns.to.non.U.S..Gov...Official.Institutions...000."  
#"Rec..Lns.to.non.U.S..Gov...Official.Institutions...000." 
#"Con..non.U.S..Government.Loans...000."  
#Chargeoffs	CO: Lns to non-U.S. Gov & Official Institutions ($000)	142714
#Recoveries	Rec: Lns to non-U.S. Gov & Official Institutions ($000)	142748
#Balance	Con: non-U.S. Government Loans ($000)	142422


for (tt in (1:T))
{	
	Y.mat.NCO[tt,13]= 
	PPNR2.aggregate.t.sign(x1="CO..Lns.to.non.U.S..Gov...Official.Institutions...000.", 
	x2="Rec..Lns.to.non.U.S..Gov...Official.Institutions...000.",
	y="Con..non.U.S..Government.Loans...000.", t=tt, data.t=data3,sign.list=c(1,-1), f=400)	  
}
summary(Y.mat.NCO[,13])


####################NCO 13: Agriculture
#Chargeoffs	CO: Agricultural Production Loans ($000)	142715
#Recoveries	Rec: Agricultural Production Loans ($000)	142749
#Balance	Con: Agricultural Prod Loans ($000)	142419

#"CO..Agricultural.Production.Loans...000."                
#"Rec..Agricultural.Production.Loans...000."
#"Con..Agricultural.Prod.Loans...000."

for (tt in (1:T))
{	
	Y.mat.NCO[tt,14]= 
	PPNR2.aggregate.t.sign(x1="CO..Agricultural.Production.Loans...000.", 
	x2="Rec..Agricultural.Production.Loans...000.", y="Con..Agricultural.Prod.Loans...000.",
	t=tt, data.t=data3,sign.list=c(1,-1), f=400)	  
}
summary(Y.mat.NCO[,14])


##################################NCO 14: Loans to Depository Institutions

#Depository institutions	
#Chargeoffs	CO: Total Lns to Dep Institutions & Acceptances ($000)	142711
#Recoveries	Rec: Total Lns to Dep Institutions & Acceptances ($000)	142745
#Balance	Con: Loans to Depository Institutions ($000)	142420
"CO..Total.Lns.to.Dep.Institutions...Acceptances...000."
"Rec..Lns.to.non.U.S..Gov...Official.Institutions...000."
"Con..Loans.to.Depository.Institutions...000."   


for (tt in (1:T))
{	
	Y.mat.NCO[tt,15]= 
	PPNR2.aggregate.t.sign(x1="CO..Total.Lns.to.Dep.Institutions...Acceptances...000.",
	x2="Rec..Lns.to.non.U.S..Gov...Official.Institutions...000.",
	y="Con..Loans.to.Depository.Institutions...000.",
	t=tt, data.t=data3,sign.list=c(1,-1), f=400)	  
}
summary(Y.mat.NCO[,15])


##################################NCO 15 Other Loans

#Chargeoffs	CO: All Other Loans ($000)	142705
#Recoveries	Rec: All Other Loans ($000)	142739
#Balance	Other Loans ($000)	142423


for (tt in (1:T))
{	
	Y.mat.NCO[tt,16]= 
	PPNR2.aggregate.t.sign(x1="CO..All.Other.Loans...000.", x2="Rec..All.Other.Loans...000.",
	y="Other.Loans...000.",
	t=tt, data.t=data3,sign.list=c(1,-1), f=400)	  
}

Y.mat.NCO=cbind(Y.mat.NCO,Return.on.Trading.Assets)
summary(Y.mat.NCO)
write.csv(Y.mat.NCO,file='Response.NCO.csv')
Response.PPNR=rbind(Y.mat.200.PPNR,Y.mat.201.PPNR)
write.csv(Response.PPNR, file=paste(getwd(),"/Input/",  "Response.PPNR.csv", sep=''))

dim(Y.mat.NCO)
NCO.stepoff=Y.mat.NCO[which(Y.mat.NCO[,"Time.trend"]==T/4),]
PPNR.stepoff=Response.PPNR[which(Response.PPNR[,"Time.trend"]==T/4),]
#write.csv(NCO.stepoff,file="H:/EY/CLASS Model/Forecast/Data/Input/NCO.stepoff.csv")
#write.csv(PPNR.stepoff,file="H:/EY/CLASS Model/Forecast/Data/Input/PPNR.stepoff.csv")




