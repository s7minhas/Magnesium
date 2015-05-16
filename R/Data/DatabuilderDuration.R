# setup workspace
if(Sys.info()["user"]=="janus829"){
source('~/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('~/ProjectsGit/Magnesium/R/Setup.R')}

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
	'targetstate_ccode', 'finaloutcome', 'imposition')
sanctionSlice <- sanctionData[,vars]
names(sanctionSlice) <- c('caseid', 'startyear', 'endyear', 
	'targetstate', 'finaloutcome', 'imposition')

# Remove threat cases
sanctionSlice = sanctionSlice[sanctionSlice$imposition==1,]

sanctionSlice$startyear <- numSM(sanctionSlice$startyear)
sanctionSlice$endyear <- numSM(sanctionSlice$endyear)
# ~40% of cases right censored [i.e., sanctions unresolved]
sanctionSlice$endyear[is.na(sanctionSlice$endyear)] <- 2013

comp <- c(1,2,5,6,7,10)
sanctionSlice$compliance <- 0
sanctionSlice$compliance[which(sanctionSlice$finaloutcome %in% comp)] <- 1
table(sanctionSlice$compliance)/nrow(sanctionSlice) # ~58% compliance
table(sanctionSlice$finaloutcome[which(sanctionSlice$compliance==1)])

sanctionSlice$time <- NA
sanctionSlice$time <- sanctionSlice$endyear - sanctionSlice$startyear + 1
###############################################################

###############################################################
# Descriptives of data
temp=sanctionSlice
dim(sanctionSlice)

# Outcome
temp$temp='fill me in'
temp[which(temp$finaloutcome %in% comp), 'temp'] = "rec'r coml'd"
temp[which(is.na(temp$finaloutcome)), 'temp'] = "ongoing"
temp$temp[which(temp$temp == 'fill me in')]=temp$finaloutcome[which(temp$temp == 'fill me in')]

table(temp$temp)
table(temp$temp)/sum(table(temp$temp))
sum((table(temp$temp)/sum(table(temp$temp)))[2:5])
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
# Choose between imputed and non-imputed versions

# targetData=monadData[,which(!names(monadData) %in% c('year','ccode','cname'))] # raw version 
targetData=impData; colnames(targetData)[1]='cyear' # imputed version

durData$tyear=numSM(durData$tyear)
aData <- merge(x=durData, y=targetData, by.x='tyear', by.y='cyear', all.x=T)
###############################################################

###############################################################
# Compliance Node level network measures from SRM
setwd(pathData)
load('compSRM.rda') # contains actorEffect, rcvrEffect, ueffect, colmeans

unpackSRM=function(srmObject, dim=T){

	yrs=names(srmObject)
	yrCts=unlist(lapply(srmObject, function(x) FUN=length(x)) )

	years=NULL
	for(ii in 1:length(yrs)){
		data=numSM(rep(yrs[ii], yrCts[ii]))
		years=append(years, data)
	}

	if(dim==T){
		cbind( years, numSM(unlist(srmObject)), 
			numSM(unlist(lapply(srmObject, function(x) rownames(x)))) )
		} else {
		cbind( years, numSM(unlist(srmObject)), 
			numSM(unlist(lapply(srmObject, function(x) names(x)))) )			
		}
}

actorData=unpackSRM(actorEffect)
rcvrData=unpackSRM(rcvrEffect)
colmData=unpackSRM(colmeans, dim=F)
srmData=data.frame(cbind(actorData[,c(3,1)],actorData[,2], rcvrData[,2], colmData[,2]))
colnames(srmData)=c('ccode','year','actor','partner','colmean')
srmData$cyear=numSM(paste0(srmData$ccode, srmData$year))

# Merge in for targetstate
aData=merge(aData, srmData[,3:ncol(srmData)], by.x='tyear', by.y='cyear', all.x=T)

# Merge in for sender states (avg, max)
# To do so we create a dataframe of just senders and year
# Then merge in srm data for each sender
# After this we can just take mean, max and merge back into adata

# Creating sender-year frame
sVars=paste('sender',1:5,'_ccode',sep='')
vars=c('year',sVars)
senders=aData[,vars]; colnames(senders)=c('year','s1','s2','s3','s4','s5')
syears=matrix(apply(senders[,2:ncol(senders)],2,function(x) FUN=numSM(paste(x,senders[,1], sep=''))),ncol=5)
colnames(syears)=paste0('syr',1:5)

# Adding SRM data, by column (five senders)
sData=senders
for(ii in 1:5){
	slice=cbind(syears[,paste0('syr',ii)],
		srmData[match(syears[,paste0('syr',ii)], srmData[,ncol(srmData)]),3:(ncol(srmData)-1)])
	colnames(slice)=c(paste0('syr',ii),paste0(colnames(slice)[2:ncol(slice)],ii))
	sData=cbind(sData, slice)
}

# Aggregating
meanSRM=function(x){if(sum(!is.na(x)!=0)){sum(x, na.rm=T)/sum(!is.na(x))}else{NA}}
maxSRM=function(x){if(sum(!is.na(x)!=0)){max(x, na.rm=T)}else{NA}}
sActor=t(apply(sData[,paste0('actor',1:5)], 1, function(x) FUN=cbind(meanSRM(x),maxSRM(x))))
colnames(sActor)=c('meanActorSndr', 'maxActorSndr')
sPtnr=t(apply(sData[,paste0('partner',1:5)], 1, function(x) FUN=cbind(meanSRM(x),maxSRM(x))))
colnames(sPtnr)=c('meanPtnrSndr', 'maxPtnrSndr')
sColm=t(apply(sData[,paste0('colmean',1:5)], 1, function(x) FUN=cbind(meanSRM(x),maxSRM(x))))
colnames(sColm)=c('meanColmSndr', 'maxColmSndr')
aData=cbind(aData, sActor, sPtnr, sColm)

# Incorporating dyadic effect
sVars=paste('sender',1:5,'_ccode',sep='')
vars=c('year',sVars)
varsT=c('year','targetstate',sVars)
senders <- aData[,varsT]

uDataRST=netMelt(senders, 'targetstate', 'year', ueffect)
uData=netMelt(senders, 'targetstate', 'year', ueffect, rst=FALSE)
ueffect2=lapply(ueffect, function(x) FUN=t(x)) # flipping pos of sen and rec
uData2=netMelt(senders, 'targetstate', 'year', ueffect2, rst=FALSE)
aData=cbind(aData, uDataRST, uData, uData2)
###############################################################

###############################################################
# Sanction  Node level network measures from SRM
setwd(pathData)
load('sancSRM.rda') # contains SactorEffect, SrcvrEffect, Sueffect, Scolmeans

SactorData=unpackSRM(SactorEffect)
SrcvrData=unpackSRM(SrcvrEffect)
ScolmData=unpackSRM(Scolmeans, dim=F)
SsrmData=data.frame(cbind(SactorData[,c(3,1)],SactorData[,2], SrcvrData[,2], ScolmData[,2]))
colnames(SsrmData)=c('ccode','year','Sactor','Spartner','Scolmean')
SsrmData$cyear=paste0(SsrmData$ccode, SsrmData$year)

# Merge in for targetstate
aData=merge(aData, SsrmData[,3:ncol(SsrmData)], by.x='tyear', by.y='cyear', all.x=T)

# Merge in for sender states (avg, max)
# To do so we create a dataframe of just senders and year
# Then merge in srm data for each sender
# After this we can just take mean, max and merge back into adata

# Creating sender-year frame
sVars=paste('sender',1:5,'_ccode',sep='')
vars=c('year',sVars)
senders=aData[,vars]; colnames(senders)=c('year','s1','s2','s3','s4','s5')
syears=matrix(apply(senders[,2:ncol(senders)],2,function(x) FUN=numSM(paste(x,senders[,1], sep=''))),ncol=5)
colnames(syears)=paste0('syr',1:5)

# Adding SRM data, by column (five senders)
sData=senders
for(ii in 1:5){
	slice=cbind(syears[,paste0('syr',ii)],
		SsrmData[match(syears[,paste0('syr',ii)], SsrmData[,ncol(SsrmData)]),3:(ncol(SsrmData)-1)])
	colnames(slice)=c(paste0('syr',ii),paste0(colnames(slice)[2:ncol(slice)],ii))
	sData=cbind(sData, slice)
}

# Aggregating
sActor=t(apply(sData[,paste0('Sactor',1:5)], 1, function(x) FUN=cbind(meanSRM(x),maxSRM(x))))
colnames(sActor)=c('SmeanActorSndr', 'SmaxActorSndr')
sPtnr=t(apply(sData[,paste0('Spartner',1:5)], 1, function(x) FUN=cbind(meanSRM(x),maxSRM(x))))
colnames(sPtnr)=c('SmeanPtnrSndr', 'SmaxPtnrSndr')
sColm=t(apply(sData[,paste0('Scolmean',1:5)], 1, function(x) FUN=cbind(meanSRM(x),maxSRM(x))))
colnames(sColm)=c('SmeanColmSndr', 'SmaxColmSndr')
aData=cbind(aData, sActor, sPtnr, sColm)

# Incorporating dyadic effect
sVars=paste('sender',1:5,'_ccode',sep='')
vars=c('year',sVars)
varsT=c('year','targetstate',sVars)
senders <- aData[,varsT]

SuDataRST=netMelt(senders, 'targetstate', 'year', Sueffect)
SuData=netMelt(senders, 'targetstate', 'year', Sueffect, rst=FALSE)
Sueffect2=lapply(Sueffect, function(x) FUN=t(x)) # flipping pos of sen and rec
SuData2=netMelt(senders, 'targetstate', 'year', Sueffect2, rst=FALSE)
aData=cbind(aData, SuDataRST, SuData, SuData2)
###############################################################

###############################################################
# Incorporating dyadic  data from other sources
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
# Var Mods
aData$polity2 = aData$polity2 + abs(min(aData$polity2, na.rm=T))

# Lagging variables
aData$tyear = numSM(aData$tyear)

doNotLag=c('id','tyear','caseid','targetstate','slength','year',
	'compliance', 'start', 'stop', 'primarysender_ccode',
	paste0('sender',1:5,'_ccode'), 'startyear', 'endyear', 
	'endyear2')
lagVars=which(!names(aData) %in% doNotLag & !substr(names(aData),0,5) %in% 'lag1_')

# Make sure all are numeric
sum(apply(aData[,lagVars],2,class)=='numeric')/length(lagVars)

aData = lagDataSM(aData,'tyear','targetstate',names(aData)[lagVars],1)
###############################################################

###############################################################
# Only data up until 2009
aData = aData[aData$year<=2009,]
###############################################################

###############################################################
# Var mods 2
aData$lag1_polity2sq=aData$lag1_polity2^2
aData$lag1_ldomsum=log(aData$lag1_domSUM+1)

# SRM measures
minNA=function(x){min(x,na.rm=T)}
srmVars=c('actor','partner','colmean',
	'meanActorSndr','meanPtnrSndr','meanColmSndr',
	'maxActorSndr','maxPtnrSndr','maxColmSndr',
	'uDataRST','uData',
	'SuData2', 'Sactor', 'Spartner')

# scaling above zero
aData$actor = aData$actor + abs(minNA(aData$actor))
aData$partner = aData$partner + abs(minNA(aData$partner))
aData$meanActorSndr = aData$meanActorSndr + abs(minNA(aData$meanActorSndr))
aData$meanPtnrSndr = aData$meanPtnrSndr + abs(minNA(aData$meanPtnrSndr))
aData$maxActorSndr = aData$maxActorSndr + abs(minNA(aData$maxActorSndr))
aData$maxPtnrSndr = aData$maxPtnrSndr + abs(minNA(aData$maxPtnrSndr))
aData$uData = aData$uData + abs(minNA(aData$uData))

aData$SuData2 = aData$SuData2 + abs(minNA(aData$SuData2))
aData$Sactor = aData$Sactor + abs(minNA(aData$Sactor))
aData$Spartner = aData$Spartner + abs(minNA(aData$Spartner))
###############################################################

###############################################################
setwd(pathData)

# save(aData, file='durDataEcon.rda')
save(aData, file='durDataEconImp_SancOnly.rda')

# save(aData, file='durDataEcon2.rda')
# save(aData, file='durDataAll.rda')
###############################################################