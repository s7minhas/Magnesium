#Purpose: This file adds country names to the cow dataset
#Author: CD
#Date: 07/11/2013

#Setup
#source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
#load('/Users/janus829/Desktop/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')

source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')
load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda')

# Data
setwd(paste(pathData, '/Components/COW_Trade_3.0', sep=''))
trade <- read.csv('dyadic_trade_3.0vSM.csv')

setwd(paste(pathData, '/Components/COW_IGO', sep=''))
igo <- read.dta('IGO_dyadunit_stata_v2.3.dta')

setwd(paste(pathData, '/Components/COW_Religion', sep=''))
religion <- read.csv('WRD_national.csv')

setwd(paste(pathData, '/Components/COW_War', sep=''))
war <- read.csv('MIDDyadic_v3.10.csv')

setwd(paste(pathData, '/Components/COW_Alliances/version4.1_stata', sep=''))
alliance <- read.dta('alliance_v4.1_by_directed_yearly.dta')

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

sancIDs[!complete.cases(sancIDs),]

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

sancIDs[!complete.cases(sancIDs),]

#-------
#religion
#-------
religion$state<-as.numeric(as.character(religion$state))
ctyNameA<-(countrycode(religion$state, "cown", "country.name"))
sancIDs<-data.frame(unique(cbind(religion$state, ctyNameA)))
sancIDs$V1<- as.numeric(as.character(sancIDs$V1))
sancIDs$ctyNameA <-as.character(sancIDs$ctyNameA)

#fix time (same as the others! How consistent)
sancIDs[sancIDs$V1==260,'ctyNameA'] <- 'GERMANY'
sancIDs[sancIDs$V1==731,'ctyNameA'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V1==678,'ctyNameA'] <- 'YEMEN'
sancIDs[sancIDs$V1==680,'ctyNameA'] <- 'S. YEMEN' 
sancIDs[sancIDs$V1==817,'ctyNameA'] <- 'S. VIETNAM'

sancIDs[!complete.cases(sancIDs),]

#-------
#war
#-------
war$CCodeA<-as.numeric(as.character(war$CCodeA))
war$CCodeB<-as.numeric(as.character(war$CCodeB))

ctyNameA<-(countrycode(war$CCodeA, "cown", "country.name"))
ctyNameB<-(countrycode(war$CCodeB, "cown", "country.name"))

sancIDs<-data.frame(unique(cbind(war$CCodeA, war$CCodeB, ctyNameA, ctyNameB)))

sancIDs$V1<- as.numeric(as.character(sancIDs$V1))
sancIDs$V2 <- as.numeric(as.character(sancIDs$V2))
sancIDs$ctyNameA <-as.character(sancIDs$ctyNameA)
sancIDs$ctyNameB <-as.character(sancIDs$ctyNameB)

#fix time
sancIDs[sancIDs$V1==731,'ctyNameA'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==731,'ctyNameB'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"

#check some other stuff?
sancIDs[sancIDs$V1==260,] #hm no germany?
which(sancIDs$ctyNameA=='GERMANY') #germany is coded as 255 only

sancIDs[!complete.cases(sancIDs),]

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

#fix time
sancIDs[sancIDs$V1==245,'ctyNameA'] <- 'BAVARIA'
sancIDs[sancIDs$V2==245,'ctyNameB'] <- 'BAVARIA'
sancIDs[sancIDs$V1==267,'ctyNameA'] <- 'BADEN'
sancIDs[sancIDs$V2==267,'ctyNameB'] <- 'BADEN'
sancIDs[sancIDs$V1==300,'ctyNameA'] <- 'AUSTRIA-HUNGARY'
sancIDs[sancIDs$V2==300,'ctyNameB'] <- 'AUSTRIA-HUNGARY'
sancIDs[sancIDs$V1==730,'ctyNameA'] <- 'NORTH KOREA'
sancIDs[sancIDs$V2==730,'ctyNameB'] <- 'NORTH KOREA'
sancIDs[sancIDs$V1==731,'ctyNameA'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==731,'ctyNameB'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V1==678,'ctyNameA'] <- 'YEMEN'
sancIDs[sancIDs$V2==678,'ctyNameB'] <- 'YEMEN'
sancIDs[sancIDs$V1==680,'ctyNameA'] <- 'S. YEMEN' 
sancIDs[sancIDs$V2==680,'ctyNameB'] <- 'S. YEMEN' 
sancIDs[sancIDs$V1==817,'ctyNameA'] <- 'S. VIETNAM'
sancIDs[sancIDs$V2==817,'ctyNameB'] <- 'S. VIETNAM'
sancIDs[sancIDs$V1==260,'ctyNameA'] <- 'GERMAN FEDERAL REPUBLIC'
sancIDs[sancIDs$V2==260,'ctyNameB'] <- 'GERMAN FEDERAL REPUBLIC'

#MYSTERY NUMBER
sancIDs[sancIDs$V1==626,'ctyNameA'] <- ''
sancIDs[sancIDs$V2==626,'ctyNameB'] <- ''

#check some other stuff?
which(sancIDs$ctyNameA=='GERMANY') #germany is coded as 255 only

sancIDs[!complete.cases(sancIDs),]

