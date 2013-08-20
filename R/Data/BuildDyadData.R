#Purpose: This file adds country names to the cow dataset
#Date: 07/11/2013

#Setup
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
load('/Users/janus829/Desktop/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
# source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')
# load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda')

###############################################################
# Load Data
# setwd(paste(pathData, '/Components/COW_Trade_3.0', sep=''))
# trade <- read.csv('dyadic_trade_3.0.csv')
# setwd(paste(pathData, '/Components/COW_IGO', sep=''))
# igo <- read.dta('IGO_dyadunit_stata_v2.3.dta')
# setwd(paste(pathData, '/Components/COW_Religion', sep=''))
# religion <- read.csv('WRD_national.csv')
# setwd(paste(pathData, '/Components/PRIO_ArmedConflict', sep=''))
# war <- read.csv('ucdp.prio.armed.conflict.v4.2013.csv')
# setwd(paste(pathData, '/Components/COW_Alliances/version4.1_stata', sep=''))
# alliance <- read.dta('alliance_v4.1_by_directed_yearly.dta')
setwd(pathData)
# save(trade, igo, religion, war, alliance, file='megaData.rda')
load('megaData.rda')
###############################################################

###############################################################
# Clean Trade [extends from 1860 to 2009]
trade2 <- trade[,c('importer1', 'importer2', 'year', 'flow1', 'flow2')]
colnames(trade2) <- c('state_name1', 'state_name2', 'year', 'imports', 'exports')
trade2 <- trade2[trade2$year>=1970,]

trade2$state_name1 <- as.character(trade2$state_name1)
trade2$state_name2 <- as.character(trade2$state_name2)

trade2$imports[trade2$imports==-9] <- 0 # setting missing to 0
trade2$exports[trade2$exports==-9] <- 0 # setting missing to 0

trade2$state_name1[trade2$state_name1=='Democratic Republic of t'] <- 'Congo, Democratic Republic of'
trade2$state_name2[trade2$state_name2=='Democratic Republic of t'] <- 'Congo, Democratic Republic of'

trade2$state_name1[trade2$state_name1=='Democratic Republic of the Con'] <- 'Congo, Democratic Republic of'
trade2$state_name2[trade2$state_name2=='Democratic Republic of the Con'] <- 'Congo, Democratic Republic of'

trade2$state_name1[trade2$state_name1=='Federated States of Micr'] <- 'Micronesia'
trade2$state_name2[trade2$state_name2=='Federated States of Micr'] <- 'Micronesia'

trade2 <- trade2[trade2$state_name1!="Yemen People's Republic",]
trade2 <- trade2[trade2$state_name2!="Yemen People's Republic",]

states <- unique(append(trade2$state_name1, trade2$state_name2))
temp <- data.frame(cbind(
	states, cname=countrycode(states, 'country.name', 'country.name')))
temp$cname <- as.character(temp$cname)
temp$cname[temp$cname=='Yugoslavia'] <- 'SERBIA'
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp$ccode <- panel$ccode[match(temp$cname,panel$cname)]

trade2$cname_1 <- temp$cname[match(trade2$state_name1,temp$states)]
trade2$cname_2 <- temp$cname[match(trade2$state_name2,temp$states)]

trade2$ccode_1 <- temp$ccode[match(trade2$state_name1,temp$states)]
trade2$ccode_2 <- temp$ccode[match(trade2$state_name2,temp$states)]

trade2 <- trade2[!is.na(trade2$ccode_1),]
trade2 <- trade2[!is.na(trade2$ccode_2),]

trade2$cyear_1 <- as.numeric(as.character(paste(trade2$ccode_1, trade2$year, sep='')))
trade2$cyear_2 <- as.numeric(as.character(paste(trade2$ccode_2, trade2$year, sep='')))

# Removing duplicates	
trade2$drop <- 0
# trade2[trade2$state_name1=='German Federal Republic' & trade2$year==1990,]
trade2[trade2$state_name1=='Germany' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name2=='Germany' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name1=='Yemen Arab Republic' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name2=='Yemen Arab Republic' & trade2$year==1990, 'drop'] <- 1
trade2 <- trade2[trade2$drop!=1,]; trade2 <- trade2[,c(1:(ncol(trade2)-1))]

# Time [trimming to 1960 to 2005]
trade2 <- trade2[trade2$year>=1960 & trade2$year<=2005,]

### ASIDE
# Create a separate export & import dataset
temp1 <- trade2[,c('ccode_1','ccode_2','year','exports')]
colnames(temp1) <- c('ccode_1','ccode_2','year','exports')
temp2 <- trade2[,c('ccode_2','ccode_1','year','imports')]
colnames(temp2) <- c('ccode_1','ccode_2','year','exports')
exports <- rbind(temp1, temp2)
exports$exports <- exports$exports*1000000

temp1 <- trade2[,c('ccode_1','ccode_2','year','imports')]
colnames(temp1) <- c('ccode_1','ccode_2','year','imports')
temp2 <- trade2[,c('ccode_2','ccode_1','year','exports')]
colnames(temp2) <- c('ccode_1','ccode_2','year','imports')
imports <- rbind(temp1, temp2)
imports$imports <- imports$imports*1000000

trade3 <- cbind(imports[,1:3], trade=imports[,4]+exports[,4])
trade3$cyear_1 <- paste(trade3$ccode_1, trade3$year, sep='')
trade3$cyear_2 <- paste(trade3$ccode_2, trade3$year, sep='')
trade3$cname_1 <- panel$cname[match(trade3$ccode_1, panel$ccode)]
trade3$cname_2 <- panel$cname[match(trade3$ccode_2, panel$ccode)]

# Subsetting to relevant vars
tradeTot <- trade3[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'trade')]
###############################################################

###############################################################
# Clean alliance data [extends from 1816 to 2012]
alliance$ccode1<-as.numeric(as.character(alliance$ccode1))
alliance$ccode2<-as.numeric(as.character(alliance$ccode2))

ctyNameA<-(countrycode(alliance$ccode1, "cown", "country.name"))
ctyNameB<-(countrycode(alliance$ccode2, "cown", "country.name"))

sancIDs<-data.frame(unique(cbind(alliance$ccode1, alliance$ccode2, ctyNameA, ctyNameB)))

sancIDs$V1<- as.numeric(as.character(sancIDs$V1))
sancIDs$V2 <- as.numeric(as.character(sancIDs$V2))
sancIDs$ctyNameA <-as.character(sancIDs$ctyNameA)
sancIDs$ctyNameB <-as.character(sancIDs$ctyNameB)

sancIDs2 <- unique(
	data.frame(cbind(
			rbind(cowcode=t(t(sancIDs[,c(1)])), cowcode=t(t(sancIDs[,c(2)]))),
			rbind(country=t(t(sancIDs[,c(3)])), country=t(t(sancIDs[,c(4)]))) ) ) )
names(sancIDs2) <- c('cowcode', 'country')

sancIDs2$cowcode <- as.numeric(as.character(sancIDs2$cowcode))
sancIDs2$country <- as.character(sancIDs2$country)

#fix time
sancIDs2[sancIDs2$cowcode==245,'country'] <- 'BAVARIA'
sancIDs2[sancIDs2$cowcode==267,'country'] <- 'BADEN'
sancIDs2[sancIDs2$cowcode==300,'country'] <- 'AUSTRIA-HUNGARY'
sancIDs2[sancIDs2$cowcode==730,'country'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs2[sancIDs2$cowcode==731,'country'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs2[sancIDs2$cowcode==678,'country'] <- 'YEMEN'
sancIDs2[sancIDs2$cowcode==680,'country'] <- 'S. YEMEN' 
sancIDs2[sancIDs2$cowcode==817,'country'] <- 'S. VIETNAM'
sancIDs2[sancIDs2$cowcode==260,'country'] <- 'GERMANY'
sancIDs2[sancIDs2$cowcode==345,'country'] <- 'SERBIA'
sancIDs2[sancIDs2$cowcode==315,'country'] <- 'CZECH REPUBLIC'

# Add in the data from the panel
sancIDs2$ccode <- panel$ccode[match(sancIDs2$country, panel$cname)]
sancIDs2$cname <- panel$cname[match(sancIDs2$country, panel$cname)]

sancIDs2[is.na(sancIDs2$ccode),]	# Checks for NAs
sancIDs2[is.na(sancIDs2$cname),] 

# Add back to alliance
alliance2 <- alliance[,c('ccode1', 'ccode2', 'state_name1', 'state_name2','year')]
colnames(alliance2)[1:2] <- c('cowcode1', 'cowcode2')

alliance2$ccode_1 <- sancIDs2$ccode[match(alliance2$cowcode1, sancIDs2$cowcode)]
alliance2$ccode_2 <- sancIDs2$ccode[match(alliance2$cowcode2, sancIDs2$cowcode)]

alliance2$cname_1 <- sancIDs2$cname[match(alliance2$cowcode1, sancIDs2$cowcode)]
alliance2$cname_2 <- sancIDs2$cname[match(alliance2$cowcode2, sancIDs2$cowcode)]

allianceFINAL <- na.omit(alliance2)
allianceFINAL$ally <- 1
###############################################################


###############################################################
# Clean PRIO War Data [first rec'd war in 1946 last in 2012]
war2 <- war[war$Type==2,]
war2 <- unique(war2[,c('ID','SideA', 'SideA2nd', 'SideB',  'SideB2nd', 'YEAR')])
war2 <- war2[1:(nrow(war2)-1),]

war2$SideA_All <- ifelse(trim(war2$SideA2nd)!='',
	as.character(paste(war2$SideA, war2$SideA2nd, sep=',')),
	as.character(war2$SideA))

war2$SideB_All <- ifelse(trim(war2$SideB2nd)!='',
	as.character(paste(war2$SideB, war2$SideB2nd, sep=',')),
	as.character(war2$SideB))

war2 <- data.frame(war2, row.names=NULL)

# Arranging to panel format and breaking rows with 
# multiple countries listed into separate rows
war3 <- NULL
for(ii in 1:nrow(war2)){
	wSlice <- war2[ii, c('SideA_All','SideB_All','YEAR')]
	Acnts <- trim(unlist(strsplit(wSlice$SideA_All,',')))
	Bcnts <- trim(unlist(strsplit(wSlice$SideB_All,',')))
	lAcnts <- length(Acnts)
	lBcnts <- length(Bcnts)
	if(lAcnts>1 | lBcnts>1){
		wSlice2 <- NULL
		for(jj in 1:lBcnts){
			wSlice1_5 <- cbind(t(t(Acnts)), t(t(Bcnts))[jj], wSlice[,'YEAR'])
			colnames(wSlice1_5) <- c('SideA_All','SideB_All','YEAR')
			wSlice2 <- rbind(wSlice2, wSlice1_5)
		}
		war3 <- rbind(war3, wSlice2)
	} else{
		wSlice2 <- wSlice
		war3 <- rbind(war3, wSlice2)
	}
}

# Adding in cname and ccode
war3$YEAR <- as.numeric(as.character(war3$YEAR))
colnames(war3) <- c('state_name1','state_name2','year')

war3 <- war3[war3$state_name1!='Hyderabad',]
war3 <- war3[war3$state_name2!='Hyderabad',]

war3$state_name1[war3$state_name1=='United Arab Emirate'] <- 'United Arab Emirates'
war3$state_name2[war3$state_name2=='United Arab Emirate'] <- 'United Arab Emirates'

states <- unique(append(war3$state_name1, war3$state_name2))
temp <- data.frame(cbind(
	states, cname=countrycode(states, 'country.name', 'country.name')))
temp$cname <- as.character(temp$cname)
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp$ccode <- panel$ccode[match(temp$cname,panel$cname)]

war3$cname_1 <- temp$cname[match(war3$state_name1,temp$states)]
war3$cname_2 <- temp$cname[match(war3$state_name2,temp$states)]

war3$ccode_1 <- temp$ccode[match(war3$state_name1,temp$states)]
war3$ccode_2 <- temp$ccode[match(war3$state_name2,temp$states)]

war3 <- war3[!is.na(war3$ccode_1),]
war3 <- war3[!is.na(war3$ccode_2),]

warFINAL <- war3
warFINAL$war <- 1
###############################################################

###############################################################
# matrix builder for undirected dyad data from monadic data
years <- 1960:2005
cntryList <- lapply(years, function(x) FUN=panel[panel$year==x,'ccode'])
names(cntryList) <- years

exportMats <- DyadBuild(variable='exports', dyadData=exports, 
	time=years, countryList=cntryList, directed=TRUE)

tradeTotMats <- DyadBuild(variable='trade', dyadData=tradeTot, 
	time=years, countryList=cntryList, directed=TRUE)

allyMats <- DyadBuild(variable='ally', dyadData=allianceFINAL, 
	time=years, countryList=cntryList, directed=FALSE)

warMats <- DyadBuild(variable='war', dyadData=warFINAL, 
	time=years, countryList=cntryList, directed=FALSE)
###############################################################

###############################################################
# Clean IGO data [extends from 1820 to 2005]
igo$ccode1<-as.numeric(as.character(igo$ccode1))
igo$ccode2<-as.numeric(as.character(igo$ccode2))

ctyNameA<-(countrycode(igo$ccode1, "cown", "country.name"))
ctyNameB<-(countrycode(igo$ccode2, "cown", "country.name"))

sancIDs<-data.frame(unique(cbind(igo$ccode1, igo$ccode2, ctyNameA, ctyNameB)))

sancIDs$V1<- as.numeric(as.character(sancIDs$V1))
sancIDs$V2 <- as.numeric(as.character(sancIDs$V2))
sancIDs$ctyNameA <-as.character(sancIDs$ctyNameA)
sancIDs$ctyNameB <-as.character(sancIDs$ctyNameB)

#fix time
sancIDs[sancIDs$V1==260,'ctyNameA'] <- 'GERMANY'
sancIDs[sancIDs$V2==260,'ctyNameB'] <- 'GERMANY'
sancIDs[sancIDs$V1==731,'ctyNameA'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==731,'ctyNameB'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V1==678,'ctyNameA'] <- 'YEMEN'
sancIDs[sancIDs$V2==678,'ctyNameB'] <- 'YEMEN'
sancIDs[sancIDs$V1==680,'ctyNameA'] <- 'S. YEMEN' 
sancIDs[sancIDs$V2==680,'ctyNameB'] <- 'S. YEMEN' 
sancIDs[sancIDs$V1==817,'ctyNameA'] <- 'S. VIETNAM'
sancIDs[sancIDs$V2==817,'ctyNameB'] <- 'S. VIETNAM'
sancIDs[sancIDs$V1==345,'ctyNameA'] <- 'SERBIA'
sancIDs[sancIDs$V2==345,'ctyNameB'] <- 'SERBIA'
sancIDs[sancIDs$V1==315,'ctyNameA'] <- 'CZECH REPUBLIC'
sancIDs[sancIDs$V2==315,'ctyNameB'] <- 'CZECH REPUBLIC'
sancIDs[sancIDs$V1==730,'ctyNameA'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==730,'ctyNameB'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"

sancIDs2 <- unique(
	data.frame(cbind(
			rbind(cowcode=t(t(sancIDs[,c(1)])), cowcode=t(t(sancIDs[,c(2)]))),
			rbind(country=t(t(sancIDs[,c(3)])), country=t(t(sancIDs[,c(4)]))) ) ) )
names(sancIDs2) <- c('cowcode', 'country')

sancIDs2$cowcode <- as.numeric(as.character(sancIDs2$cowcode))
sancIDs2$country <- as.character(sancIDs2$country)

# Add in the data from the panel
sancIDs2$ccode <- panel$ccode[match(sancIDs2$country, panel$cname)]
sancIDs2$cname <- panel$cname[match(sancIDs2$country, panel$cname)]

sancIDs2[is.na(sancIDs2$ccode),]	# Checks for NAs
sancIDs2[is.na(sancIDs2$cname),] 

# Add back into igo
igo2 <- igo
names(igo2)[1] <- 'cowcode1'
names(igo2)[3] <- 'cowcode2'

igo2$ccode_1 <- sancIDs2$ccode[match(igo2$cowcode1, sancIDs2$cowcode)]
igo2$ccode_2 <- sancIDs2$ccode[match(igo2$cowcode2, sancIDs2$cowcode)]


igo2$cname_1 <- sancIDs2$cname[match(igo2$cowcode1, sancIDs2$cowcode)]
igo2$cname_2 <- sancIDs2$cname[match(igo2$cowcode2, sancIDs2$cowcode)]

# Finalize IGO dataset
igoFINAL <- igo2
igoFINAL <- igoFINAL[igoFINAL$year>=1960,c(534:535,5,6:533)]
igoFINAL <- data.matrix(igoFINAL)

# Set all igo codes of 3, -9, and -1 for IGO membership
## to 0 and for igo codes of 1 and 2 set to 1
drop <- c(3, -9, -1, 0)
years <- c(1960,1965:2005)
igoData <- NULL
for(ii in 1:length(years)){
	slice <- igoFINAL[which(igoFINAL[,'year']==years[ii]),]
	sList <- lapply(4:ncol(slice), function(x) FUN=slice[,c(1:3,x)])
	sList2 <- lapply(sList, function(x) FUN=x[which(!x[,4] %in% drop),])
	sList3 <- sList2[which(numSM(summary(sList2)[,1])>0)]
	sList4 <- lapply(sList3, function(x){
		temp <- matrix(x, ncol=4); paste(temp[,1],temp[,2],sep='_') })
	yearIGOs <- t(t(table( unlist(sList4) )))
	yearIGOs <- cbind(yearIGOs, year=years[ii])
	igoData <- rbind(igoData, yearIGOs)
	print(years[ii])
}

# Cleaning
igoDataFINAL <- data.frame(cbind(rownames(igoData), igoData), row.names=NULL)
colnames(igoDataFINAL) <- c('ccodes', 'igo', 'year')
ccodes <- matrix(
	unlist(strsplit(as.character(igoDataFINAL[,'ccodes']), '_')) 
	,ncol=2,byrow=T)
colnames(ccodes) <- c('ccode_1','ccode_2')
igoDataFINAL <- cbind(ccodes, igoDataFINAL[,c('year','igo')])
igoDataFINAL <- data.frame(apply(igoDataFINAL,2,numSM))

igoMats <- DyadBuild(variable='igo', dyadData=igoDataFINAL, 
	time=years, countryList=cntryList, directed=FALSE)
###############################################################

###############################################################
# Clean religion data [extends from 1945 to 2010]
religion$state<-as.numeric(as.character(religion$state))
ctyNameA<-(countrycode(religion$state, "cown", "country.name"))
sancIDs<-data.frame(unique(cbind(religion$state, ctyNameA)))
sancIDs$V1<- as.numeric(as.character(sancIDs$V1))
sancIDs$ctyNameA <-as.character(sancIDs$ctyNameA)

sancIDs2 <- sancIDs
names(sancIDs2) <- c('cowcode', 'country')

sancIDs2$cowcode <- as.numeric(as.character(sancIDs2$cowcode))
sancIDs2$country <- as.character(sancIDs2$country)

#fix time (same as the others! How consistent)
sancIDs2[sancIDs2$cowcode==260,'country'] <- 'GERMANY'
sancIDs2[sancIDs2$cowcode==731,'country'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs2[sancIDs2$cowcode==678,'country'] <- 'YEMEN'
sancIDs2[sancIDs2$cowcode==680,'country'] <- 'S. YEMEN' 
sancIDs2[sancIDs2$cowcode==817,'country'] <- 'S. VIETNAM'
sancIDs2[sancIDs2$cowcode==345,'country'] <- 'SERBIA'
sancIDs2[sancIDs2$cowcode==315,'country'] <- 'CZECH REPUBLIC'

# Add in the data from the panel
sancIDs2$ccode <- panel$ccode[match(sancIDs2$country, panel$cname)]
sancIDs2$cname <- panel$cname[match(sancIDs2$country, panel$cname)]

sancIDs2[is.na(sancIDs2$ccode),]	# Checks for NAs
sancIDs2[is.na(sancIDs2$cname),] 

# Add back to religion
religion$ccode <- sancIDs2$ccode[match(religion$state, sancIDs2$cowcode)]
religion$cname <- sancIDs2$cname[match(religion$state, sancIDs2$cowcode)]

religionData <- religion[,c('year','ccode',
	names(religion)[which(substrRight(names(religion),6)=='genpct')]
	)]
majorRelig <- apply(religionData, 1, function(x)
	FUN=names(religionData)[which(x == max(x[3:ncol(religionData)]))] )
majorRelig[[1481]] <- 'noMajor'
majorRelig <- unlist(majorRelig)
religionFINAL <- cbind(religionData[,1:2], majRelig=majorRelig)
religionFINAL <- religionFINAL[religionFINAL$year>=1960 & religionFINAL$year<=2005,]

years <- seq(1960,2005,5)
cntryList <- lapply(years, function(x) FUN=religionFINAL[religionFINAL$year==x,'ccode'])
names(cntryList) <- years

religMats <- DyadBuild_fMonad(variable='majRelig', oper='same',
	monadData=religionFINAL, time=years, countryList=cntryList)
###############################################################

###############################################################
#saving cleaned data
setwd(pathData)
save(exportMats, tradeTotMats, allyMats, warMats, igoMats, religMats
	,file='dyadMats.rda')
###############################################################