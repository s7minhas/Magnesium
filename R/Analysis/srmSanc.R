##################################################
# Purpose: create a list of matrices to measure reciprocity
# CD & SM
# Load data, create matrices, run SRM, pull out useful measures, plot.
###################################################
if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

# Load sanction & compliance network Data
setwd(pathData)
load('sanctionNet.rda') #smatList
load('complianceNet.rda') #cmatList

# srm
if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Analysis/SRM.R')}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Analysis/SRM.R')}

# source("SRM.R")

#out<-lapply(smatList, function(x), FUN=dyads(x))

outSanc<-list()
year <- 1960
		for (i in smatList){
			print(year)
			outSanc <- c(outSanc,list(dyads(i)))
			names(outSanc)[length(outSanc)] <- paste("year",year, sep="_")
			year <- year+1
		}
outSanc <-outSanc 

# pull out all of the individual SRM stats for all the years
actor.effect.i <- lapply(outSanc, function(year) year$actor.effect.i)
partner.effect.i <- lapply(outSanc, function(year) year$partner.effect.i)
unique.effect.ij <- lapply(outSanc, function(year) year$unique.effect.ij) #mat
unique.variance<- lapply(outSanc, function(year) year$unique.variance) #1/yr
relationship.covariance<-lapply(outSanc, function(year) year$relationship.covariance) 
actor.variance<- lapply(outSanc, function(year) year$actor.variance) #1/yr
partner.variance<-lapply(outSanc, function(year) year$partner.variance) #1/yr
actor.partner.covariance<-lapply(outSanc, function(year) year$actor.partner.covariance)
colmeans <- lapply(outSanc, function(year) year$colmeans)

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
partner.effect.i <- lapply(outComp, function(year) year$partner.effect.i)
unique.effect.ij <- lapply(outComp, function(year) year$unique.effect.ij) #mat
unique.variance<- lapply(outComp, function(year) year$unique.variance) #1/yr
relationship.covariance<-lapply(outComp, function(year) year$relationship.covariance) 
actor.variance<- lapply(outComp, function(year) year$actor.variance) #1/yr
partner.variance<-lapply(outComp, function(year) year$partner.variance) #1/yr
actor.partner.covariance<-lapply(outComp, function(year) year$actor.partner.covariance)
colmeans <- lapply(outComp, function(year) year$colmeans)


# grahpics
# reciprocity figure sanc
uniq.net<-as.matrix(outSanc$year_1990$unique.effect.ij)
diag(uniq.net)<-0
plot.network(network(uniq.net, directed=T, usearrows=T,edge.col=8, vertex.col="darkblue",label.col="black", label.pos=1, label.cex=.75, edge.lwd=.1) 

# reciprocity figure sanc 2
library(qgraph)
qgraph(uniq.net, minimum=-min(outSanc$year_1990$unique.effect.ij), maximum=  max(outSanc$year_1990$unique.effect.ij), labels=rownames(uniq.net), asize= .1, arrows=FALSE, label.scale= TRUE, diag=FALSE)
par(fig=c(.8,1,.8,1),new=T)
qgraph(smatList$"1990", weighted=TRUE, labels=rownames(smatList))
par(fig=c(0,1,0,1))

#covariance over time
plot(seq(1960,2005,1), lapply(relationship.covariance,(log))
plot(seq(1960,2005,1), lapply(actor.partner.covariance,log))


## NOTES
# relationship/unique variance- measuring the degree to which compliance is unique to particular parings/partners; reflects variance due to the interaction between actor and partner 

# actor-partner covariance is the extent to which i's compliance, in general, is correlated with partners (js') compliance with i. 

# The relationship covariance can be thought of as the association between how country i complies specifically with country j and how partner j complies specifically with country i.
