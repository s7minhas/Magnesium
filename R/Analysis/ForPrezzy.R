#source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')

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
summary(cmodel4)

#same as model 6 but with interaction polconiii:noS
	#also tried with polity interaction but wasn't super interesting
	#and neither is this interaction but it does test our story
cmodel8 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ polconiii:noS
	+ frailty.gaussian(caseid,sparse=FALSE)
	, data=aData)
summary(cmodel8)
plot(survfit(cmodel8))
