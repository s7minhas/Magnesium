# setup workspace
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Loading Excel data from UNC
setwd(pathData)
load('sanctionData.rda')
load('monadData.rda')

# Subsetting to economic sanctions (issue = 4, 12, 13, 14)
econ <- c(4, 12, 13, 14)
sanctionDataFinal$issue1[is.na(sanctionDataFinal$issue1)] <- 0
sanctionDataFinal$issue2[is.na(sanctionDataFinal$issue2)] <- 0
sanctionDataFinal$issue3[is.na(sanctionDataFinal$issue3)] <- 0
sanctionData <- sanctionDataFinal[sanctionDataFinal$issue1==econ |
	sanctionDataFinal$issue2==econ |
	sanctionDataFinal$issue3==econ, ]

# Pulling out vars
# compliance is defined as 2, 5, and 7
# threat timeframe = startyear, endyear
# sanction timeframe = sancimpositionstartyear
comp <- c(1,2,5,6,7,10)
vars <- c('caseid', 'startyear', 'endyear', 
	'targetstate', 'finaloutcome')
sanctionSlice <- sanctionData[,vars]
# names(sanctionSlice)[2] <- 'startyear'
sanctionSlice$startyear <- numSM(sanctionSlice$startyear)
sanctionSlice$endyear <- numSM(sanctionSlice$endyear)
sanctionSlice$endyear[is.na(sanctionSlice$endyear)] <- -99

sanctionSlice$compliance <- 0
sanctionSlice$compliance[which(sanctionSlice$finaloutcome %in% comp)] <- 1

sanctionSlice$time <- NA
sanctionSlice[sanctionSlice$endyear!=-99 & sanctionSlice$compliance==1, 'time'] <-
 sanctionSlice$endyear[sanctionSlice$endyear!=-99 & sanctionSlice$compliance==1] - 
 sanctionSlice$startyear[sanctionSlice$endyear!=-99 & sanctionSlice$compliance==1] + 1

sanctionSliceComp <- sanctionSlice[sanctionSlice$compliance==1 &
 sanctionSlice$endyear!=-99,]
head(sanctionSliceComp)

# Set up frame
durData <- NULL
ii=1
for(ii in 1:nrow(sanctionSliceComp)){
	slice <- sanctionSliceComp[ii,]
	year <- seq(slice$startyear, slice$endyear, 1)
	tcountry <- slice$targetstate; case <- slice$CaseID
	temp <- cbind(slice$CaseID, slice$targetstate, 
		year, duration=seq(1,length(year), 1))
	durData <- rbind(durData, temp) }