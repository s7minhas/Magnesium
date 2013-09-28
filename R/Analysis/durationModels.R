# Purpose: Some duration modeling attempts for magnesium 

#source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')

setwd(pathData)
load('durData.rda')
ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)
############################################################
# Frailty models
cmodel1 = coxph(Surv(start, stop, compliance) ~ 
	lag1_noS + lag1_polity + lag1_noS:lag1_polity
	+ lag1_lgdpCAP + lag1_Internal.Conflict
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel1)
plot(survfit(cmodel1))

cmodel2 = coxph(Surv(start, stop, compliance) ~ 
	lag1_noS + lag1_polity + lag1_distdata 
	+ lag1_lgdpCAP + lag1_Internal.Conflict
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel2)
plot(survfit(cmodel2))

cmodel3 = coxph(Surv(start, stop, compliance) ~ 
	lag1_noS + lag1_polity + lag1_distdata 
	+ lag1_lgdpCAP + lag1_Internal.Conflict + lag1_edata
	+ lag1_allydata + lag1_igodata
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel3)
plot(survfit(cmodel3))

duration ~ Internal.Conflict + gdpCAP + polity,
	c ~ noS + distdata + sancRecCnt + edata + allydata + igodata +religdata,
############################################################


############################################################
# Creating duration dataset for spdur function
aData$date=paste(aData$year,'1-1',sep='-')
aData$ccode=aData$caseid
spdurList=buildDuration(data=aData, y='compliance',
	trainingend='1990-01-01', teststart='1991-01-01',dataend='2005-01-01')
############################################################

############################################################
# Time varying models w/ spdur, an example
full=spdurList$'full'
train=spdurList$'training'
test=spdurList$'test'
pred=spdurList$'predData'

model = spdur(duration ~ noS,
	c ~ noS,
	last=full$end.spell, data=full, test=full, distr='weibull', iter=300)

predProbs <- predictSPD(model, pred)

pr.nc.in <- cbind(full$compliance,predProbs$pr.in$n.cure.t.in)
pr.ht.in <- cbind(full$compliance,predProbs$pr.in$pr.c.h.in)

separationplot(pr.nc.in[,2], pr.nc.in[,1], 
	shuffle=T, heading="Pr(Non immunity)", 
	show.expected=T, newplot=F)
separationplot(pr.ht.in[,2], pr.ht.in[,1], 
	shuffle=T, heading="Pr(Compliance at t | Non immunity)", 
	show.expected=T, newplot=F)
############################################################

############################################################
# Time varing models, network effects

model1 = spdur(duration ~ lgdpCAP, 
	c ~ lgdpCAP,
	last=full$end.spell, data=full, test=full, distr='weibull', iter=300)

model2 = spdur(duration ~ Internal.Conflict + gdpCAP + polity,
	c ~ noS + distdata + sancRecCnt + edata + allydata + igodata +religdata,
	last=full$end.spell, data=full, test=full, distr='weibull', iter=300)

predProbs <- predictSPD(model1, pred)

pr.nc.in <- cbind(full$compliance,predProbs$pr.in$n.cure.t.in)
pr.ht.in <- cbind(full$compliance,predProbs$pr.in$pr.c.h.in)

separationplot(pr.nc.in[,2], pr.nc.in[,1], 
	shuffle=T, heading="Pr(Non immunity)", 
	show.expected=T, newplot=F)
separationplot(pr.ht.in[,2], pr.ht.in[,1], 
	shuffle=T, heading="Pr(Compliance at t | Non immunity)", 
	show.expected=T, newplot=F)
############################################################