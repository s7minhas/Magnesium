#Purpose: This file adds country names to the Sanctions dataset
#Author: CD
#Date: 07/08/2013

#Setup
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
load('/Users/janus829/Desktop/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')

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