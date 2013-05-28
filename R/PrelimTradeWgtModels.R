# Prelim models using sanction weighted trade

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
require(plm)
require(lme4)
####################################################################################	

####################################################################################	
# Load data
setwd(pathData)
load('dataForAnalysisV1.rda')
modelData <- na.omit(analysisData)
modelData <- modelData[,c(2,6:11)]
modelData <- pdata.frame(modelData, c('ccode', 'year'))
####################################################################################	

####################################################################################	
# Running model
results1 <- lmer(int_conflict ~ polity + log(gdp_capita) + sancTrade +
 (1 | ccode), data=analysisData)
summary(results1)

results2 <- lmer(govt_stab ~ polity + log(gdp_capita) + sancTrade +
 (1 | ccode), data=analysisData)
summary(results2)
####################################################################################