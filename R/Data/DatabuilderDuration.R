# setup workspace
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
# source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')

###############################################################
# Loading Excel data from UNC
setwd(pathPData)
load('panel.rda')
setwd(pathData)
load('sanctionData.rda')
load('monadData.rda')
load('dyadMats.rda')
load('mindistMatrices.rda')
###############################################################

###############################################################
# Subsetting to economic sanctions
econ <- c(4, 13, 14) # saved as durDataEcon
# econ <- c(4, 8, 12, 13, 14) # saved as durDataEcon2
sanctionDataFinal$issue1[is.na(sanctionDataFinal$issue1)] <- 0
sanctionDataFinal$issue2[is.na(sanctionDataFinal$issue2)] <- 0
sanctionDataFinal$issue3[is.na(sanctionDataFinal$issue3)] <- 0
sanctionData <- sanctionDataFinal[which(sanctionDataFinal$issue1%in%econ |
	sanctionDataFinal$issue2%in%econ |
	sanctionDataFinal$issue3%in%econ), ]

# Sanctions covering all issues
# sanctionData=sanctionDataFinal # saved as durDataAll
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
# ~40% of cases right censored [i.e., sanctions unresolved]
sanctionSlice$endyear[is.na(sanctionSlice$endyear)] <- 2013

comp <- c(1,2,5,6,7,10)
sanctionSlice$compliance <- 0
sanctionSlice$compliance[which(sanctionSlice$finaloutcome %in% comp)] <- 1
table(sanctionSlice$compliance)/nrow(sanctionSlice) # ~58% compliance

sanctionSlice$time <- NA
sanctionSlice$time <- sanctionSlice$endyear - sanctionSlice$startyear + 1
###############################################################

###############################################################
# Analyzing outcomes of sanction cases
outcomeData=sanctionSlice[,c('caseid','endyear','finaloutcome','compliance')]

table(outcomeData$finaloutcome[which(outcomeData$compliance==1)])

outcomeData$noComp = 1
outcomeData$noComp[ which(outcomeData$endyear==2013 & outcomeData$compliance==0 | outcomeData$compliance==1) ] = 0
table(outcomeData$finaloutcome[which(outcomeData$noComp==1)])
###############################################################

###############################################################
# Set up duration frame
durData <- NULL
for(ii in 1:nrow(sanctionSlice)){
	slice <- sanctionSlice[ii,]
	year <- seq(slice$startyear, slice$endyear, 1)
	tcountry <- slice$targetstate; case <- slice$caseid
	temp <- cbind(slice$caseid, slice$targetstate, 
		slice$time, year, durationSM=seq(1,length(year), 1))
	if(slice$compliance==1){
		temp=cbind(temp, c(rep(0,nrow(temp)-1),1))} else {
			temp=cbind(temp, rep(0,nrow(temp)))
		}
	durData <- rbind(durData, temp) }

durData <- data.frame(durData)
colnames(durData) <- c('caseid', 'targetstate', 'slength', 'year', 'durationSM', 'compliance')
durData$tyear <- paste(durData$targetstate, durData$year, sep='')

# Setting up start and stop times
durData$start=durData$durationSM-1
durData$stop=durData$durationSM

# Subsetting durationSM dataset
durData <- durData[durData$year>=1960 & durData$year<=2012,]
durData <- durData[durData$targetstate!=1000,] # Eliminating cases where EU is sanction target
senders <- sanctionDataFinal[,c(1,75:80)]

# Adding sender info
senders[senders==1000] <- NA # Turning cases where EU is sender to NA
durData <- merge(x=durData, y=senders, by='caseid')

# Drops weird case of sanction against E. Germ from W. Germ during 1990-2012
durData <- durData[which(
	paste(durData$targetstate, durData$year, sep='') %in% 
	panel$ccodeYear),]

# Adding in add'l caseid info for surv object
durData <- merge(x=durData, y=sanctionSlice[,c(1:3)], by='caseid', all.x=T)
durData$endyear2 <- durData$endyear + 1

# Some cases have only IGOs as senders
sVars=paste('sender',1:5,'_ccode',sep='')
noS <- apply(durData[,sVars], 1, function(x) FUN=sum(!is.na(x)) )
durData <- cbind(durData, noS)

# This drops cases where sanctions were
# sent by institutions,instits sending sanction include:
# European Economic Community (1653), European Union (1830)
# OECD (3750), WTO (4580). Excluding these organizations
# leads to the exclusion of 22 total sanction cases and 11
# cases where target state complied 
durData <- durData[durData$noS!=0,]
###############################################################

###############################################################
# Add in monadic variables for target state
monadData$lgdp=log(monadData$gdp)
monadData$lgdpCAP=log(monadData$gdpCAP)
monadData$lpopulation=log(monadData$population)
monadData$domSUM=sum(monadData$domestic1, monadData$domestic2, monadData$domestic3,
	monadData$domestic4,monadData$domestic5,monadData$domestic6,monadData$domestic7,
	monadData$domestic8)
monadData$autocBIN=ifelse(monadData$autoc>=6,1,0)
monadData$democBIN=ifelse(monadData$autoc>=6,1,0)

aData <- merge(x=durData, y=monadData[,c(1,3,5:ncol(monadData))], 
	by.x='tyear', by.y='cyear', all.x=T)
###############################################################

###############################################################
# Building network data
sVars=paste('sender',1:5,'_ccode',sep='')
vars=c('year',sVars)
varsT=c('year','targetstate',sVars)
senders <- aData[,varsT]

# dyadic datasets: exportMats, tradeTotMats, allyMats, warMats, igoMats, religMats
logSM=function(x){ y=x; y[y!=0]=log(x[x!=0]); y }
exportMats=lapply(exportMats, function(x) FUN=logSM(x))
tradeTotMats=lapply(tradeTotMats, function(x) FUN=logSM(x))
edata=netMelt(senders, 'targetstate', 'year', exportMats)
tdata=netMelt(senders, 'targetstate', 'year', tradeTotMats)
allydata=netMelt(senders, 'targetstate', 'year', allyMats, rst=FALSE)

igoMats$'1961' <- igoMats$'1960'; igoMats$'1962' <- igoMats$'1960'
igoMats$'1963' <- igoMats$'1960'; igoMats$'1964' <- igoMats$'1960'
igoMats$'1965' <- igoMats$'1960'
igodata=netMelt(senders, 'targetstate', 'year', igoMats, rst=FALSE)

religMats2 <- rep(religMats, 5)
religMats2 <- religMats2[sort(names(religMats2))]
names(religMats2) <- 1960:2014
religMats2 <- religMats2[as.character(1960:2010)]
religdata=netMelt(senders, 'targetstate', 'year', religMats2, rst=FALSE)

religMats2 <- rep(CreligMats, 5)
religMats2 <- religMats2[sort(names(religMats2))]
names(religMats2) <- 1960:2014
religMats2 <- religMats2[as.character(1960:2010)]
religMats2=lapply(religMats2, function(x) FUN=x+1)
Creligdata=netMelt(senders, 'targetstate', 'year', religMats2, rst=FALSE)

distMats <- distMats[as.character(1960:2011)]
distdata=netMelt(senders, 'targetstate', 'year', distMats)

DdistMats <- lapply(distMats, function(x){
	x <- ifelse(x<=200,1,0); diag(x) <- 0; x })
Ddistdata=netMelt(senders, 'targetstate', 'year', DdistMats, rst=FALSE)

rownames(aData) <- 1:nrow(aData)
aData <- cbind(aData, edata, tdata, allydata, igodata, 
	religdata, Creligdata, distdata, Ddistdata)

# Other Network Variables
# Number of senders
aData$noS <- apply(aData[,sVars], 1, function(x) {sum(!is.na(x))} )

# Number of sanctions being received by targt state
sancRecCnt=summaryBy(targetstate ~ targetstate + year, data=aData, FUN=length)
names(sancRecCnt)[3] = 'sancRecCnt'
sancRecCnt$id=paste(sancRecCnt$targetstate, sancRecCnt$year, sep='')
aData$id=paste(aData$targetstate, aData$year, sep='')
aData=merge(aData, sancRecCnt[,3:4], by='id')

# Number of sanctions being sent by senders
senders=na.omit(melt(aData[,vars], id='year')[,c(1,3)])
sancSenCnt=summaryBy(value ~ value + year, data=senders, FUN=length)
names(sancSenCnt)=c('sender', 'year', 'sancSenCnt')

aData$sancSenCnt=NA
for(ii in 1:nrow(aData)){
	slice=aData[ii,]
	senSlice=slice[,sVars]; yrSlice=slice[,'year']
	senSlice=senSlice[!is.na(senSlice)]

	senCntSlice=sancSenCnt[
		which(sancSenCnt$sender %in% senSlice &
		 sancSenCnt$year %in% yrSlice),]
	cntSumm=mean(senCntSlice$sancSenCnt)

	aData$sancSenCnt[ii]=cntSumm }
###############################################################

###############################################################
# Lagging variables
aData <- lagDataSM(aData, 'tyear', 'targetstate', 
	names(aData)[c(18,20:length( names(aData) ) )],1)
###############################################################

###############################################################
save(aData, file='durDataEcon.rda')
# save(aData, file='durDataEcon2.rda')
# save(aData, file='durDataAll.rda')
###############################################################