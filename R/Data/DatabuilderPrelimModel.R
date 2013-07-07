# Dataset for prelim analysis

####################################################################################
# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data";
	pathDataC="~/Dropbox/Research/Magnesium/Data/Components"}

# Libraries and functions
require(countrycode)
####################################################################################	

####################################################################################	
# Dataset frame
setwd(pathData)
load('dataStructure.rda')

# These are all temporary fixes they are needed because these countries are
# listed as having sanctions before they are classified as being existing
# by the COW codes in the cshapes package
# Lithuania Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=368, year=c(1990,1991)))
# Latvia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=367, year=c(1990,1991)))
# Estonia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=366, year=c(1990,1991)))
# Slovenia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=349, year=c(1990,1991)))
# Azerbaijan Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=373, year=c(1991)))
# Armenia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=371, year=c(1991)))
####################################################################################	

####################################################################################
# Key IV sanction weighted by trade dependence between target and receivers 
# sanction sources: UNC sanction dataset (1971-2000)
setwd(pathData)
load('sanction.rda')
# Changing position of senders and receivers...now receivers on rows and senders on columns
TsanctionDyadData <- lapply(sanctionDyadData, function(x) FUN=t(x))
# Clean sanctions matrix data by dropping -99s
remove <- '-99'
TsanctionDyadData <- lapply(TsanctionDyadData, 
	function(x) FUN=x[!rownames(x) %in% remove, !colnames(x) %in% remove])


## trade dependence = (x + m)/GDP; COW trade data, World Bank GDP data
# tradeData <- read.csv('dyadic_trade_3.0vSM.csv',header=T)
# save(tradeData, file='trade.rda')
load('trade.rda')

# Need to convert to matrix
# Setting up list of country names in existence for time period of analysis
cowcodesYear <- list()
years <- seq(1971, 2000, 1)
for(ii in 1:length(years)){
	cowcodesYear[[ii]] <- cowPanel[cowPanel$year==years[ii],'cowcode'] }

# tradeDyadData <- list()

# for(ii in 1:length(years)){

# 	countries <- cowcodesYear[[ii]]
# 	yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
# 	rownames(yearMatrix) <- colnames(yearMatrix) <- countries

# 	yearData <- tradeData[tradeData$year==years[ii],]

# 	for(jj in 1:length(countries)){
# 	for(kk in 1:length(countries)){

# 		if(countries[jj]==countries[kk]) {value <- 0} else {
# 			if(nrow(yearData[yearData$ccode1==countries[jj] &
# 			 yearData$ccode2==countries[kk],])==0)
# 			{value <- yearData[yearData$ccode1==countries[kk] &
# 			 yearData$ccode2==countries[jj],'tradedep2']}
# 			else
# 			{value <- yearData[yearData$ccode1==countries[jj] &
# 			 yearData$ccode2==countries[kk],'tradedep1']}
# 		}

# 		if(length(value)==0) {value<-0}
# 		yearMatrix[as.character(countries[jj]),as.character(countries[kk])] <- value
# 	}

# 		temp <- round(quantile(1:length(countries), probs=seq(.1,1,.1)),0)
# 		if(jj%in%temp){cat(round(100*jj/length(countries),0),"%  ", sep="")}

# 	}

# 	tradeDyadData[[ii]] <- yearMatrix
# 	print(years[ii])
# }
# save(tradeDyadData, file='tradeDyad.rda')
load('tradeDyad.rda')

# Creating trade dependence weighted sanction variable
sanctionTrade <- NULL
for(ii in 1:length(years)){

	sanctionMatrix <- TsanctionDyadData[[ii]]
	tradeMatrix <- tradeDyadData[[ii]]
	tradeMatrix[is.na(tradeMatrix)] <- 0
	tradeMatrix <- abs(tradeMatrix)

	for(jj in 1:(dim(tradeMatrix)[1])){

		sanctionSlice <- t(t(t(sanctionMatrix[jj,])))
		tradeSlice <- t(t(tradeMatrix[jj,]))

		sancTrade <- as.vector(sanctionSlice %*% tradeSlice)

		sanctionTrade <- rbind(sanctionTrade, 
			cbind(rownames(tradeMatrix)[jj], sancTrade, years[ii]))
	}
}
sanctionTrade <- data.frame(sanctionTrade)
colnames(sanctionTrade) <- c('ccode', 'sancTrade', 'year')
sanctionTrade$ccode <- as.numeric(as.character(sanctionTrade$ccode))
sanctionTrade$sancTrade <- as.numeric(as.character(sanctionTrade$sancTrade))
sanctionTrade$year <- as.numeric(as.character(sanctionTrade$year))
sanctionTrade$cyear <- paste(sanctionTrade$ccode, sanctionTrade$year, sep='_')
####################################################################################

####################################################################################
# DV = internal stability from PRS group (Govt stability or internal conflict)
# Other IVs: World Bank GDP per capita, polity score
setwd(pathData)
otherData <- read.csv('combined.csv')
otherData$ccode <- as.numeric(as.character(otherData$ccode))
otherData$year <- as.numeric(as.character(otherData$year))
otherData$cyear <- paste(otherData$ccode, otherData$year, sep='_')
####################################################################################

####################################################################################
# Merging into final dataset
analysisData <- merge(otherData, sanctionTrade[,c(2,4)], by='cyear', all.x=T)
setwd(pathData)
save(analysisData, file='dataForAnalysisV1.rda')
####################################################################################