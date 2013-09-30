source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
# source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')

setwd(pathData)
load('durData.rda')
ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)

# not lagging everything
# incudes: controls + senders & distance hypo + net hypo
# ***pretty interesting results
cmodel4 = coxph(Surv(start, stop, compliance) ~ 
	noS + lag1_polity + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + religdata
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
# summary(cmodel4)

#same as model 6 but with interaction polconiii:noS
	#also tried with polity interaction but wasn't super interesting
	#and neither is this interaction but it does test our story
# cmodel8 = coxph(Surv(start, stop, compliance) ~ 
# 	noS + polconiii + distdata 
# 	+ lag1_lgdpCAP + Internal.Conflict + tdata
# 	+ allydata + igodata + sancRecCnt + Creligdata
# 	+ interaction
# 	+ frailty.gaussian(caseid,sparse=FALSE)
# 	, data=aData)
# summary(cmodel8)
# plot(survfit(cmodel8))

aData$interaction=aData$polconiii*aData$noS
cmodel8 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ interaction
	, data=aData)
summary(cmodel8)

mTab=summary(cmodel8)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(mtab)=c('Number of senders',
	'Distance', 'Constraints',
	'GDP per Capita (lagged)',
	'Internal Conflcit',
	'Trade', 'Ally',
	'IGO', "Rec'd Sanctions",
	'Religion', 'Senders*Constraints')
save(xtable(mTab), file='mod8.tex')

fform <- formula(Surv(start, stop, compliance) ~ 
		noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ interaction
	+ cluster(caseid))

fmodel <- frailtyPenal(fform,
	data=aData,
	Frail=TRUE,
	n.knots=20,
	kappa1=100000,
	recurrentAG=TRUE)

attach(aData)
aDataSim <- data.frame(
	noS=c(1), polconiii=rep(mean(polconiii,na.rm=T),2), 
	distdata=rep(mean(distdata,na.rm=T),2),
	lag1_lgdpCAP=rep(mean(lag1_lgdpCAP,na.rm=T),2), 
	Internal.Conflict=rep(mean(Internal.Conflict,na.rm=T),2),
	tdata=rep(mean(tdata,na.rm=T),2), allydata=rep(mean(allydata,na.rm=T),2),
	igodata=rep(mean(igodata,na.rm=T),2), 
	sancRecCnt=rep(mean(sancRecCnt,na.rm=T),2),
	Creligdata=rep(mean(Creligdata,na.rm=T),2), 
	interaction=rep(mean(interaction,na.rm=T),2))
detach(aData)

# Comparing fit of time-fixed and varying
plot(survfit(cmodel8))
plot(survfit(cmodel8, newdata=aDataSim), conf.int=T, lty=c(1,2))