# Constructing dyadic variable for trade between i and j pairs

####################################################################################
#Setup
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
load('/Users/janus829/Desktop/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
####################################################################################	

####################################################################################	
# Dataset frame
setwd(pathData)
load('megaDataClean.rda')
load('combinedData.rda')
####################################################################################	

####################################################################################
## trade dependence = (x + m)/GDP; COW trade data, World Bank GDP data
tradeFINAL$cyear_1 <- paste(tradeFINAL$ccode_1, tradeFINAL$year, sep='')
tradeFINAL$cyear_2 <- paste(tradeFINAL$ccode_2, tradeFINAL$year, sep='')

# Need to convert to matrix
# Setting up list of country names in existence for time period of analysis
cntriesYear <- list()
years <- seq(1970, 2000, 1)
for(ii in 1:length(years)){
	cntriesYear[[ii]] <- panel[panel$year==years[ii],'ccode'] }

tradeDyadData <- list()

for(ii in 1:length(years)){

	countries <- cntriesYear[[ii]]
	yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
	rownames(yearMatrix) <- colnames(yearMatrix) <- countries

	yearData <- tradeFINAL[tradeFINAL$year==years[ii],]

	for(jj in 1:length(countries)){
	for(kk in 1:length(countries)){

		if(countries[jj]==countries[kk]) {value <- 0} else {
			if(nrow(yearData[yearData$ccode_1==countries[jj] &
			 yearData$ccode_2==countries[kk],])==0)
			{value <- yearData[yearData$ccode_1==countries[kk] &
			 yearData$ccode_2==countries[jj],'tradedep2']}
			else
			{value <- yearData[yearData$ccode_1==countries[jj] &
			 yearData$ccode_2==countries[kk],'tradedep1']}
		}

		if(length(value)==0) {value<-0}
		yearMatrix[as.character(countries[jj]),as.character(countries[kk])] <- value
	}

		temp <- round(quantile(1:length(countries), probs=seq(.1,1,.1)),0)
		if(jj%in%temp){cat(round(100*jj/length(countries),0),"%  ", sep="")}

	}

	tradeDyadData[[ii]] <- yearMatrix
	print(years[ii])
}


save(tradeDyadData, file='tradeDyad.rda')
# load('tradeDyad.rda')