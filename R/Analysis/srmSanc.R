##################################################
# Purpose: create a list of matrices to measure reciprocity
# CD & SM
# Load data, create matrices, run SRM, pull out useful measures, plot.
###################################################


# Load sanction & compliance network Data
setwd(pathData)
load('sanctionData.rda')
load('sanctionNet.rda') #smatList
load('complianceNet.rda') #cmatList
sendIDs=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate_ccode',sendIDs,'startyear','endyear','caseid')]


# srm
source("SRM.R")
#out<-lapply(smatList, function(x), FUN=dyads(x))

out2<-list()
year <- 1960
		for (i in smatList){
			print(year)
			out2 <- c(out2,list(dyads(i)))
			names(out2)[length(out2)] <- paste("year",year, sep="_")
			year <- year+1
		}
out2 <-out2

# pull out all of the individual SRM stats for all the years
actor.effect.i <- lapply(out2, function(year) year$actor.effect.i)
partner.effect.i <- lapply(out2, funcouttion(year) year$partner.effect.i)
unique.effect.ij <- lapply(out2, function(year) year$unique.effect.ij) #mat
unique.variance<- lapply(out2, function(year) year$unique.variance) #1/yr
relationship.covariance<-lapply(out2, function(year) year$relationship.covariance) 
actor.variance<- lapply(out2, function(year) year$actor.variance) #1/yr
partner.variance<-lapply(out2, function(year) year$partner.variance) #1/yr
actor.partner.covariance<-lapply(out2, function(year) year$actor.partner.covariance)
colmeans <- lapply(out2, function(year) year$colmeans)

# run on compliance matrix
outComp<-list()
year <- 1960
		for (i in cmatList){
			print(year)
			outComp <- c(outComp,list(dyads(i)))
			names(outComp)[length(outComp)] <- paste("year",year, sep="_")
			year <- year+1
		}
outComp <-outComp

# pull out all of the individual SRM stats for all the years
actor.effect.i <- lapply(outComp, function(year) year$actor.effect.i)
partner.effect.i <- lapply(outComp, funcouttion(year) year$partner.effect.i)
unique.effect.ij <- lapply(outComp, function(year) year$unique.effect.ij) #mat
unique.variance<- lapply(outComp, function(year) year$unique.variance) #1/yr
relationship.covariance<-lapply(outComp, function(year) year$relationship.covariance) 
actor.variance<- lapply(outComp, function(year) year$actor.variance) #1/yr
partner.variance<-lapply(outComp, function(year) year$partner.variance) #1/yr
actor.partner.covariance<-lapply(outComp, function(year) year$actor.partner.covariance)
colmeans <- lapply(outComp, function(year) year$colmeans)


# grahpics
# reciprocity figure sanc
uniq.net<-as.matrix(out2$year_1990$unique.effect.ij)
diag(uniq.net)<-0
plot.network(network(uniq.net, directed=T, usearrows=T,edge.col=8, vertex.col="darkblue",label.col="black", label.pos=1, label.cex=.75, edge.lwd=.1) 

# reciprocity figure sanc 2
library(qgraph)
qgraph(uniq.net, minimum=-min(out2$year_1990$unique.effect.ij), maximum=  max(out2$year_1990$unique.effect.ij), labels=rownames(uniq.net), asize= .1, arrows=FALSE, label.scale= TRUE, diag=FALSE)
par(fig=c(.8,1,.8,1),new=T)
qgraph(smatList$"1990", weighted=TRUE, labels=rownames(smatList))
par(fig=c(0,1,0,1))
