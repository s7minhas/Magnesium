#Purpose: This file adds country names to the Sanctions dataset
#Author: CD
#Date: 07/08/2013

#Setup
rm(list=ls()) 

source()
setwd(pathData)

require(cshapes)
require(countrycode)

#data
sanctionData <- read.csv('SanctionsDataV3.5vSM_GermanyFix.csv')

#add country names
to.change<-sanctionData$targetstate
to.change1<-sanctionData$sender1
to.change2<-sanctionData$sender2
to.change3<-sanctionData$sender3
to.change4<-sanctionData$sender4
ctyNameTar<-(countrycode(to.change,"cown", "country.name"))
ctyNameSend1<-(countrycode(to.change1, "cown", "country.name"))
ctyNameSend2<-(countrycode(to.change2, "cown", "country.name"))
ctyNameSend3<-(countrycode(to.change3, "cown", "country.name"))
ctyNameSend4<-(countrycode(to.change4, "cown", "country.name"))

sanctionData<-cbind(sanctionData,ctyNameTar, ctyNameSend1,ctyNameSend2, ctyNameSend3, ctyNameSend4)


