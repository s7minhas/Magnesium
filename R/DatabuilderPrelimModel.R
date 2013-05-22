# Dataset for prelim analysis

####################################################################################
# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data";
	pathDataC="~/Dropbox/Research/Magnesium/Data/Components"}

# Libraries and functions
require(countrycode)
####################################################################################	

####################################################################################	
# Dataset frame
setwd(pathData)
load('dataStructure.rda')

# These are all temporary fixes they are needed because these countries are
# listed as having sanctions before they are classified as being existing
# by the COW codes in the cshapes package
# Lithuania Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=368, year=c(1990,1991)))
# Latvia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=367, year=c(1990,1991)))
# Estonia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=366, year=c(1990,1991)))
# Slovenia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=349, year=c(1990,1991)))
# Azerbaijan Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=373, year=c(1991)))
# Armenia Fix
cowPanel <- rbind(cowPanel, cbind(cowcode=371, year=c(1991)))
####################################################################################	

####################################################################################	
# DV = banks conflict index or internal stability from PRS group (Govt stability or internal conflict)

####################################################################################	

####################################################################################
# Key IV sanction weighted by trade dependence between target and receivers 
# sanction sources: UNC sanction dataset (1971-2000)
setwd(pathData)
load('sanction.rda')
# Changing position of senders and receivers...now receivers on rows and senders on columns
TsanctionDyadData <- lapply(sanctionDyadData, function(x) FUN=t(x))

## trade dependence = (x + m)/GDP; COW trade data, World Bank GDP data
# tradeData <- read.csv('dyadic_trade_3.0vSM.csv',header=T)
# save(tradeData, file='trade.rda')
load('trade.rda')

####################################################################################

####################################################################################
# Other IVs: World Bank GDP per capita, polity score

####################################################################################