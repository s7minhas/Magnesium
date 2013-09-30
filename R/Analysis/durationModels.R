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
	+ sancRecCnt
	+ lag1_Ddistdata
	# + lag1_lgdpCAP + lag1_gdpGR 
	+ lag1_civwar
	+ lag1_gdpGR + lag1_gdpCAP
	# + frailty.gaussian(targetstate,sparse=FALSE)
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

# lagging most things
cmodel3 = coxph(Surv(start, stop, compliance) ~ 
	lag1_noS + lag1_polity + lag1_distdata 
	+ lag1_lgdpCAP + lag1_Internal.Conflict + lag1_tdata
	+ lag1_allydata + lag1_igodata + lag1_religdata + sancRecCnt
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel3)

# not lagging everything
# incudes: controls + senders & distance hypo + net hypo
# ***pretty interesting results
cmodel4 = coxph(Surv(start, stop, compliance) ~ 
	noS + lag1_polity + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + religdata
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel4)

# includes: controls + senders & distance hypo + net hypo
	# diff religious measure, also influential but not big change
cmodel5 = coxph(Surv(start, stop, compliance) ~ 
	noS + lag1_polity + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel5)

#controls + senders & distance hypo + net hypos 
	# + diff polity measures (polcom, xropen, do not change much)
	# *** but polconii does!
cmodel6 = coxph(Surv(start, stop, compliance) ~ 
	noS  + distdata + polconiii
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel6)

#controls + senders & distance hypo + net hypo 
	# diff conflict indicators, domestic9 & civ war didnt do much
cmodel7 = coxph(Surv(start, stop, compliance) ~ 
	noS  + distdata + polconiii
	+ lag1_lgdpCAP + domestic9 + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel7)
plot(survfit(cmodel7))

#same as model 6 but with interaction polconiii:noS
	#also tried with polity interaction but wasn't super interesting
	#and neither is this interaction
cmodel8 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ polconiii:noS
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel8)
plot(survfit(cmodel8))

#same as model 6 but with interaction internalconflict:noS
cmodel9 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + religdata
	+ Internal.Conflict:noS
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel9)
plot(survfit(cmodel9))

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
	c ~ noS + distdata + sancRecCnt + tdata + allydata + igodata +religdata,
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