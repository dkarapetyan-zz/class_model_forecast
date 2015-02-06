
T <- 4*(2014+0.75-1991)
setwd("H:/EY/CLASS Model/Calibration/Data")
list.files()
setwd("H:/EY/CLASS Model/Calibration/Data/Input")
list.files()
#load PPNR and NCO response data
PPNR.response <- read.csv("Response.PPNR.csv")
PPNR.response$SNL.Institution.Key <- ifelse(!is.na(PPNR.response$SNL.Institution.Key),PPNR.response$SNL.Institution.Key,0)
NCO.response <- read.csv("Response.NCO.csv")

#load the balance ratio data
ratio.input.data <- read.csv("BalanceRatio.csv")
dim(ratio.input.data)
names(ratio.input.data)
ratio.input.data$SNL.Institution.Key <- ifelse(!is.na(ratio.input.data$SNL.Institution.Key),ratio.input.data$SNL.Institution.Key,0)


#load the macro data
macro.input.data <- read.csv("Macro.Historical.Input.csv")

#load the coeffcients
coeff.data <- read.csv("Original_CLASS_Market_Data_Input.csv")

list.keep <- which(colSums(coeff.data[,-1])!=0)
coeff.orig <- coeff.data[,c("Variable",colnames(coeff.data[,(list.keep+1)]))]

coeff.new <- matrix(0,ncol=length(list.keep)+2, nrow=nrow(coeff.data))
colnames(coeff.new) <- c("Variable",colnames(coeff.data[,(list.keep+1)]),"Intercept")
coeff.new[,1] <- as.character(coeff.data[,1])

# define the function to calculate the lagged  variables
lag1.fun<-function(y, y.name, data=PPNR.response){
      y.lag <- array(NA, length(y))
	for (ii in (1:length(y))) {
		Time.trend1 <- data$Time.trend[ii]-0.25
		if (Time.trend1==0) {y.lag[ii]==NA}
		if (Time.trend1>0){ 
			SNL <- data$SNL.Institution.Key[ii]
			y.lag[ii] <- data[which( (data$SNL.Institution.Key==SNL)+(data$Time.trend==Time.trend1)==2),y.name]
			}
		}
	return(y.lag <- y.lag)
}


###########  PPNR model calibration

for ( i in c(1:2,4:7))
{
	Y.data <- PPNR.response[,c("Time.trend",as.character(coeff.new[i,1]),"SNL.Institution.Key")]
	Y1.data <- merge(Y.data,macro.input.data[,-c(1,2)],by="Time.trend",all = TRUE)
	Y2.data <- merge(Y1.data, ratio.input.data[,-c(1,3)],by=c("Time.trend","SNL.Institution.Key"),all = TRUE)

# response variable
	y <- Y2.data[,as.character(coeff.new[i,1])]
	listNA <- c(which(y=="-Inf"), which(y=="Inf"), which(y=="NaN"))
	y[listNA] <- NA
	Y2.data[,as.character(coeff.new[i,1])] <- y

# x- risk drivers
	list <- names(coeff.data)[which(coeff.data[i,]!=0)][-1]
# read the x input for the variables
	L <- length(list)
	x.list <- matrix(NA, ncol=L,nrow=length(y))
     	colnames(x.list)=list
	
	for (j in (1: L))
			{
	      	if (list[j]== "Lagged.dependent.variable") 	
				{x.list[,j] <-  lag1.fun(y=y, y.name=as.character(coeff.new[i,1]),data=Y2.data )      } 
 
			#if (list[j]== "Time.trend")
    			#{x.list[,j] = T0 + 0.25*t}

	        	if (list[j]== "Interaction.BBB.Spread.change.and.Risky.AFS")  
				{x.list[,j] <- Y2.data$Quarterly.change.in.BBB.Spread.if.change.is.positive*
						Y2.data$Risky.AFS.Ratio/100
					}				
		      if (list[j] %in% colnames(Y2.data))
					{x.list[,j] <- Y2.data[,list[j]]} 

			}
		
		res <- lm(y ~ x.list)
		
		summary(res)

		
		for (j in (1: L))
			{ coeff.new[i,list[j]] <- res$coef[1+which(colnames(x.list)==list[j])] }
		coeff.new[i, "Intercept"] <- res$coef[1]
}



###########  NCO model calibration

for ( i in (c(3,8: 22)))
{
	Y.data <- NCO.response[,c("Time.trend",as.character(coeff.new[i,1]))]
	Y1.data <- merge(Y.data,macro.input.data[,-c(1,2)],by="Time.trend",all = TRUE)
	#Y2.data <- merge(Y1.data, ratio.input.data[,-c(1,3)],by="Time.trend",all = TRUE)

# response variable
	y <- Y.data[,as.character(coeff.new[i,1])]
	listNA <- c(which(y=="-Inf"), which(y=="Inf"), which(y=="NaN"))
	y[listNA] <- NA
	Y.data[,as.character(coeff.new[i,1])] <- y

# x- risk drivers
	list <- names(coeff.data)[which(coeff.data[i,]!=0)][-1]
# read the x input for the variables
	L <- length(list)
	x.list <- matrix(NA, ncol=L,nrow=length(y))
     	colnames(x.list) <- list
	
	for (j in (1: L))
			{
	      	if (list[j]== "Lagged.dependent.variable") 	
				#{x.list[,j] <-  c(y[-1],NA)} 
 				{x.list[,j] <-  c(NA, y[-length(y)])}
			#if (list[j]== "Time.trend")
    			#{x.list[,j]  <-  T0 + 0.25*t}
	        										
		      if (list[j] %in% colnames(Y1.data))
					{x.list[,j] <- Y1.data[,list[j]]} 

			}
		

		res <- lm(y ~ x.list)
		
		summary(res)
		
		for (j in (1: L))
			{ coeff.new[i,list[j]] <- res$coef[1+which(colnames(x.list)==list[j])] }
		coeff.new[i, "Intercept"] <- res$coef[1]
		coeff.new[i,1]
}


write.csv(coeff.new, row.names=FALSE,file="H:/EY/CLASS Model/Forecast/Data/Input/CLASS_Market_Data_Input_Intercept.csv")
write.csv(coeff.new, row.names=FALSE,file="H:/EY/CLASS Model/Calibration/Data/Output/CLASS_Market_Data_Input_Intercept.csv")
write.csv(coeff.orig,row.names=FALSE,file="H:/EY/CLASS Model/Calibration/Data/Output/CLASS_Market_Data_Input_Orginal.csv")



write.csv(coeff.new, row.names=FALSE,file="H:/EY/CLASS Model/Forecast/Data/Input/CLASS_NCO_Intercept.csv")

