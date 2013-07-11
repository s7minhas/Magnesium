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
alliance <- read.dta('')

#trade
to.changeA<-(trade$ccode1)
to.changeB<-(trade$ccode2)

ctyNameA<-(countrycode(to.changeA, "cown", "country.name"))
ctyNameB<-(countrycode(to.changeB, "cown", "country.name"))

sancIDs<-data.frame(unique(cbind(to.changeA, to.changeB, ctyNameA, ctyNameB)))
sancIDs[!complete.cases(sancIDs),]
sancIDs[is.na(sancIDs$ctyNameB),] #Ah shit. 