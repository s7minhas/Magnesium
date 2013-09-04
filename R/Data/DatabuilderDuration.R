# setup workspace
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

###############################################################
# Loading Excel data from UNC
setwd(pathPData)
load('panel.rda')
setwd(pathData)
load('sanctionData.rda')
load('monadData.rda')
load('dyadMats.rda')
###############################################################

###############################################################
# Subsetting to economic sanctions (issue = 4, 12, 13, 14)
econ <- c(4, 12, 13, 14)
sanctionDataFinal$issue1[is.na(sanctionDataFinal$issue1)] <- 0
sanctionDataFinal$issue2[is.na(sanctionDataFinal$issue2)] <- 0
sanctionDataFinal$issue3[is.na(sanctionDataFinal$issue3)] <- 0
sanctionData <- sanctionDataFinal[sanctionDataFinal$issue1==econ |
	sanctionDataFinal$issue2==econ |
	sanctionDataFinal$issue3==econ, ]
###############################################################

###############################################################
# Pulling out vars
# threat timeframe = startyear, endyear
# sanction timeframe = sancimpositionstartyear
vars <- c('caseid', 'startyear', 'endyear', 
	'targetstate_ccode', 'finaloutcome')
sanctionSlice <- sanctionData[,vars]
names(sanctionSlice) <- c('caseid', 'startyear', 'endyear', 
	'targetstate', 'finaloutcome')

sanctionSlice$startyear <- numSM(sanctionSlice$startyear)
sanctionSlice$endyear <- numSM(sanctionSlice$endyear)
sanctionSlice$endyear[is.na(sanctionSlice$endyear)] <- 2013

comp <- c(1,2,5,6,7,10)
sanctionSlice$compliance <- 0
sanctionSlice$compliance[which(sanctionSlice$finaloutcome %in% comp)] <- 1

sanctionSlice$time <- NA
sanctionSlice$time <- sanctionSlice$endyear - sanctionSlice$startyear + 1
###############################################################

###############################################################
# Set up duration frame
durData <- NULL
for(ii in 1:nrow(sanctionSlice)){
	slice <- sanctionSlice[ii,]
	year <- seq(slice$startyear, slice$endyear, 1)
	tcountry <- slice$targetstate; case <- slice$CaseID
	temp <- cbind(slice$caseid, slice$targetstate, 
		slice$compliance, slice$time,
		year, duration=seq(1,length(year), 1))
	durData <- rbind(durData, temp) }

durData <- data.frame(durData)
colnames(durData)[1:4] <- c('caseid', 'targetstate', 'compliance', 'slength')
durData$tyear <- paste(durData$targetstate, durData$year, sep='')

# Subsetting duration dataset
durData <- durData[durData$year>=1960 & durData$year<=2012,]
durData <- durData[durData$targetstate!=1000,] # Eliminating cases where EU is sanction target
senders <- sanctionDataFinal[,c(1,75:80)]

# Adding sender info
senders[senders==1000] <- NA # Turning cases where EU is sender to NA
durData <- merge(x=durData, y=senders, by='caseid')

# Dropping fucked up cases
durData <- durData[which(
	paste(durData$targetstate, durData$year, sep='') %in% 
	panel$ccodeYear),]

# Adding in add'l caseid info for surv object
durData <- merge(x=durData, y=sanctionSlice[,c(1:3)], by='caseid', all.x=T)
durData$endyear2 <- durData$endyear + 1

# Strange issue where some cases have no senders...wtf
noS <- apply(durData[,9:13], 1, function(x) FUN=sum(!is.na(x)) )
durData <- cbind(durData, noS)

# Dropping cases where therea are zero senders
durData <- durData[durData$noS!=0,]
###############################################################

###############################################################
# Add in monadic variables for target state
aData <- merge(x=durData, y=monadData[,c(1,3,5:ncol(monadData))], 
	by.x='tyear', by.y='cyear', all.x=T)

aData <- aData[aData$year<=2005,]
###############################################################

###############################################################
# Building network data
senders <- aData[,c(6, 3, 9:13)]
# dyadic datasets: exportMats, tradeTotMats, allyMats, warMats, igoMats, religMats
igoMats$'1961' <- igoMats$'1960'; igoMats$'1962' <- igoMats$'1960'
igoMats$'1963' <- igoMats$'1960'; igoMats$'1964' <- igoMats$'1960'
igoMats$'1965' <- igoMats$'1960'

ndata <- NULL
for(ii in 1:nrow(senders)){
	slice <- senders[ii,]
	sen <- as.character(na.omit(t(slice[,3:7]))[[1]])
	tar <- as.character(slice$targetstate)

	# Row standardize
	ddata <- exportMats[[as.character(slice$year)]]
	matDenom <- apply(ddata, 1, sum); matDenom[matDenom==0] <- 1
	ddata <- ddata/matDenom

	# Row/Col Rel.	
	ddata[tar, sen]

	# Network measure
	ddata <- mean(ddata)

	# Combine
	ndata <- rbind(ndata, ddata)
}

aData <- cbind(aData, ndata)

save(aData, file='forCassyDurPractice.rda')
###############################################################