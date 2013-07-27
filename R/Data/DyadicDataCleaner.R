#Purpose: This file adds country names to the cow dataset
#Author: CD
#Date: 07/11/2013

#Setup
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
load('/Users/janus829/Desktop/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')

# source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')
# load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda')

# Data
# setwd(paste(pathData, '/Components/COW_Trade_3.0', sep=''))
# trade <- read.csv('dyadic_trade_3.0vSM.csv')

# setwd(paste(pathData, '/Components/COW_IGO', sep=''))
# igo <- read.dta('IGO_dyadunit_stata_v2.3.dta')

# setwd(paste(pathData, '/Components/COW_Religion', sep=''))
# religion <- read.csv('WRD_national.csv')

# setwd(paste(pathData, '/Components/PRIO_ArmedConflict', sep=''))
# war <- read.csv('ucdp.prio.armed.conflict.v4.2013.csv')

# setwd(paste(pathData, '/Components/COW_Alliances/version4.1_stata', sep=''))
# alliance <- read.dta('alliance_v4.1_by_directed_yearly.dta')

setwd(paste(pathData, '/Components',sep=''))
# save(trade, igo, religion, war, alliance, file='megaData.rda')
load('megaData.rda')

#-------
#trade
#-------
trade$ccode1<-as.numeric(as.character(trade$ccode1))
trade$ccode2<-as.numeric(as.character(trade$ccode2))

ctyNameA<-(countrycode(trade$ccode1, "cown", "country.name"))
ctyNameB<-(countrycode(trade$ccode2, "cown", "country.name"))

sancIDs<-data.frame(unique(cbind(trade$ccode1, trade$ccode2, ctyNameA, ctyNameB)))

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

# Add back into trade
trade2 <- trade
names(trade2)[1:2] <- c('cowcode1','cowcode2')

trade2$ccode_1 <- sancIDs2$ccode[match(trade2$cowcode1, sancIDs2$cowcode)]
trade2$ccode_2 <- sancIDs2$ccode[match(trade2$cowcode2, sancIDs2$cowcode)]

trade2$cname_1 <- sancIDs2$cname[match(trade2$cowcode1, sancIDs2$cowcode)]
trade2$cname_2 <- sancIDs2$cname[match(trade2$cowcode2, sancIDs2$cowcode)]

tradeFINAL <- trade2

#-------
#igo
#-------
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

igoFINAL <- igo2

#-------
#religion
#-------
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

religionFINAL <- religion

#-------
#alliance
#-------

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


###############################################################
# PRIO war
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
colnames(war3) <- c('state_name1','state_name2','Year')

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
###############################################################

#-------
#saving cleaned cow data
#-------
setwd(pathData)
save(tradeFINAL, igoFINAL, allianceFINAL, religionFINAL, warFINAL,
	file='megaDataClean.rda')