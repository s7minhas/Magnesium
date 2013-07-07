# setup workspace
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Loading Excel data from UNC
setwd(pathData)
sanctionData <- read.csv('SanctionsDataV3.5vSM_GermanyFix.csv')

# Pulling out vars
# compliance is defined as 2, 5, and 7
vars <- c('CaseID', 'startyear', 'endyear', 'targetstate', 'finaloutcome')
sanctionSlice <- sanctionData[,vars]
sanctionSlice$compliance <- 0
sanctionSlice$compliance[sanctionSlice$finaloutcome==2 | sanctionSlice$finaloutcome==5 | 
							sanctionSlice$finaloutcome==7] <- 1
sanctionSlice$time <- NA
sanctionSlice[sanctionSlice$endyear!=-99 & sanctionSlice$compliance==1, 'time'] <-
 sanctionSlice$endyear[sanctionSlice$endyear!=-99 & sanctionSlice$compliance==1] - 
 sanctionSlice$startyear[sanctionSlice$endyear!=-99 & sanctionSlice$compliance==1] + 1
head(sanctionSlice)

# plot(sort(sanctionSlice$time))
# plot(sort(sanctionSlice[sanctionSlice$endyear==-99,'startyear']))
# t1 <- t(t(round(table(sanctionSlice$endyear)/sum(table(sanctionSlice$endyear)),2)))
# t2 <- t(t(table(sanctionSlice$endyear)))
# cbind(t2,t1)
# sanctionSlice$time2 <- sanctionSlice$time
# sanctionSlice$time2[sanctionSlice$time2==-99] <- 100
# temp <- sanctionSlice[is.na(sanctionSlice$time) & sanctionSlice$finaloutcome!=11,]

# Set up frame
sanctionSliceComp <- sanctionSlice[sanctionSlice$compliance==1 & sanctionSlice$endyear!=-99,]
durData <- NULL
ii=1
for(ii in 1:nrow(sanctionSliceComp)){
	slice <- sanctionSliceComp[ii,]
	year <- seq(slice$startyear, slice$endyear, 1)
	tcountry <- slice$targetstate; case <- slice$CaseID
	temp <- cbind(slice$CaseID, slice$targetstate, 
		year, duration=seq(1,length(year), 1))
	durData <- rbind(durData, temp) }