# Need to pull out sanction variable for use in analysis
# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data"}

# Loading Excel data from UNC
setwd(pathData)
sanctionData <- read.csv('SanctionsDataV3.5.csv')

# Creating frame for dataset
# date params for cshapes
years <- seq(1971, 2000, 1)
# date <- paste(years, '-6-30', sep='')
# Constructing data with COW codes
# library(cshapes)
# cowPanel <- NULL
# for(ii in 1:length(date)){
# 	cowcodesYear <- cshp(date=as.Date(date[ii]), useGW=FALSE)$COWCODE
# 	cowPanel <- rbind(cowPanel, cbind(cowcode=cowcodesYear, year=years[ii])) }

# cowPanel <- as.data.frame(cowPanel)	
# setwd(pathData)
# save(cowPanel, file='dataStructure.rda')
setwd(pathData)
load('dataStructure.rda')

# Add a -99 code for each year in cowPanel, makes it easier to 
# deal with -99 in sanctionData
cowPanel <- rbind(cowPanel, cbind(cowcode=-99, year=years))
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
# Germany Fix
cowPanel$cowcode[cowPanel$cowcode==260] <- 255
cowPanel$cowcode[cowPanel$cowcode==265] <- 255


# Setting up list of country names in existence for time period of analysis
cowcodesYear <- list()
for(ii in 1:length(years)){
	cowcodesYear[[ii]] <- cowPanel[cowPanel$year==years[ii],'cowcode'] }

# Pulling out data from sanctionData file
# first find sanctionData in a given year
# replace endyear -99 which means not ended with 2020
sanctionData$endyear2 <- sanctionData$endyear
sanctionData$endyear2[sanctionData$endyear2==-99] <- 2020
sanctionData <- sanctionData[sanctionData$targetstate!=1000,]
sanctionData <- sanctionData[sanctionData$targetstate!=1001,]
sanctionData <- sanctionData[sanctionData$targetstate!=1002,]

# Germany Fix
sanctionData$targetstate[sanctionData$targetstate==260] <- 255
sanctionData$targetstate[sanctionData$targetstate==265] <- 255
sanctionData$sender1[sanctionData$sender1==260] <- 255
sanctionData$sender2[sanctionData$sender2==260] <- 255
sanctionData$sender3[sanctionData$sender3==260] <- 255
sanctionData$sender4[sanctionData$sender4==260] <- 255
sanctionData$sender5[sanctionData$sender5==260] <- 255
sanctionData$primarysender[sanctionData$primarysender==260] <- 255
cowPanel$sanction <- NA

sanctionDyadData <- list()
for(ii in 1:length(years)){
	temp <- sanctionData[years[ii]>=sanctionData$startyear & years[ii]<=sanctionData$endyear2, 
		c('startyear', 'endyear2', 'sender1', 'sender2', 'sender3', 'sender4', 'sender5', 
			'primarysender', 'targetstate')]  
	
	cntry <- cowcodesYear[[ii]]
	temp3 <- matrix(0, nrow=length(cntry), ncol=length(cntry), dimnames=list(cntry, cntry))
	
	for(jj in 1:ncol(t(temp))){
		senders <- as.character(as.vector(t(temp)[3:7,jj]))
		senderPrim <- as.character(as.vector(t(temp)[8,jj]))
		target <- as.character(as.vector(t(temp)[9,jj]))

		temp2 <- matrix(0, nrow=length(cntry), ncol=length(cntry), dimnames=list(cntry, cntry))
		temp2[senders, target] <- 1
		temp2[senderPrim, target] <- 2
		temp3 <- temp3 + temp2
	}

	sanctionDyadData[[ii]] <- temp3
}

setwd(pathData)
save(sanctionDyadData, file='sanction.rda')