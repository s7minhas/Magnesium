#Purpose: This file adds country names to the Sanctions dataset

#Setup
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R')
load('~/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
}

###############################################################
setwd(paste(pathData, '/Components', sep=''))
#data [sanction data extends from 1946 to 2012]
sanctionData <- read.csv('TIESv4.csv')
###############################################################

###############################################################
# Fix errors for five cases, see http://www.unc.edu/~bapat/TIESCorrections.do
sanctionData[which(sanctionData$caseid ==1990999901), 'threat']=1
sanctionData[which(sanctionData$caseid ==1983011301), 'imposition']=1
sanctionData[which(sanctionData$caseid ==1973070501), 'startmonth']=1
sanctionData[which(sanctionData$caseid ==1973070501), 'startday']=9
sanctionData[which(sanctionData$caseid ==1973070501), 'sancimpositionstartmonth']=1
sanctionData[which(sanctionData$caseid ==1973070501), 'sancimpositionstartday']=9
sanctionData[which(sanctionData$caseid ==1973070501), 'finaloutcome']=8
sanctionData[which(sanctionData$caseid ==2002041106), 'endyear']=2004
sanctionData[which(sanctionData$caseid ==2002041103), 'targetstate']=490
###############################################################

###############################################################
#add country names
to.change<-t(t(sanctionData$targetstate))
to.changeps<-t(t(sanctionData$primarysender))
to.change1<-t(t(sanctionData$sender1))
to.change2<-t(t(sanctionData$sender2))
to.change3<-t(t(sanctionData$sender3))
to.change4<-t(t(sanctionData$sender4))
to.change5<-t(t(sanctionData$sender5))

ctyNameTar<-t(t((countrycode(to.change,"cown", "country.name"))))
ctyNameSendps<-t(t((countrycode(to.changeps, "cown", "country.name"))))
ctyNameSend1<-t(t((countrycode(to.change1, "cown", "country.name"))))
ctyNameSend2<-t(t((countrycode(to.change2, "cown", "country.name"))))
ctyNameSend3<-t(t((countrycode(to.change3, "cown", "country.name"))))
ctyNameSend4<-t(t((countrycode(to.change4, "cown", "country.name"))))
ctyNameSend5<-t(t((countrycode(to.change5, "cown", "country.name"))))

sancIDs<-data.frame(unique(cbind(
	rbind(to.change, to.changeps, to.change1,to.change2, to.change3, to.change4, to.change5),
	rbind(ctyNameTar, ctyNameSendps, ctyNameSend1,ctyNameSend2, ctyNameSend3, ctyNameSend4, ctyNameSend5)) ) )

sancIDs[is.na(sancIDs$X2),]

sancIDs <- sancIDs[!is.na(sancIDs$X1),]

sancIDs$X1 <- as.numeric(as.character(sancIDs$X1))
sancIDs$X2 <- as.character(sancIDs$X2)

sancIDs[sancIDs$X1==731,'X2'] <- "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$X1==260,'X2'] <- 'GERMANY'
sancIDs[sancIDs$X1==678,'X2'] <- 'YEMEN'
sancIDs[sancIDs$X1==680,'X2'] <- 'S. YEMEN' 
sancIDs[sancIDs$X1==817,'X2'] <- 'S. VIETNAM'
sancIDs[sancIDs$X1==1000,'X2'] <- 'EU'
names(sancIDs) <- c('cowcodeTIES', 'cname')

sancIDs$cname[sancIDs$cname=='Yugoslavia'] <- 'SERBIA'
sancIDs$cname[sancIDs$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
sancIDs$ccode <- panel$ccode[match(sancIDs$cname, panel$cname)]
sancIDs[sancIDs$cname=='EU', 'ccode'] <- 1000
###############################################################

###############################################################
addMatcher <- function(newVarFromPanel, matchFromData, matchFromPanel){
	newVarFromPanel[match(matchFromData, matchFromPanel)] }

vars <- c('targetstate', 'primarysender', 'sender1', 'sender2', 'sender3', 'sender4', 'sender5')
temp1 <- apply(sanctionData[,vars], 2, function(x) FUN=addMatcher(sancIDs$cname, x, sancIDs$cowcodeTIES))
colnames(temp1) <- paste(vars, '_cname', sep='')

temp2 <- apply(sanctionData[,vars], 2, function(x) FUN=addMatcher(sancIDs$ccode, x, sancIDs$cowcodeTIES))
colnames(temp2) <- paste(vars, '_ccode', sep='')

sanctionData <- cbind(sanctionData, temp1, temp2)
###############################################################

###############################################################
# Subsetting to economic sanctions
econ <- c(4, 13, 14)
# econ <- c(4, 8, 12, 13, 14)
sanctionData$issue1[is.na(sanctionData$issue1)] <- 0
sanctionData$issue2[is.na(sanctionData$issue2)] <- 0
sanctionData$issue3[is.na(sanctionData$issue3)] <- 0
sanctionData <- sanctionData[which(sanctionData$issue1%in%econ |
	sanctionData$issue2%in%econ |
	sanctionData$issue3%in%econ), ]

# Sanctions covering all issues
# sanctionData=sanctionData

# Only include cases that involve the imposition of sanctions
sanctionData = sanctionData[sanctionData$imposition==1,]
###############################################################

setwd(pathData)
save(sanctionData, file='sanctionData.rda')