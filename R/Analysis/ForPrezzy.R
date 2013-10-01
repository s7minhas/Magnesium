if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

setwd(pathData)
load('durData.rda')
ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)

# not lagging everything
# incudes: controls + senders & distance hypo + net hypo
# ***pretty interesting results
aData$Creligdata=aData$Creligdata+abs(min(aData$Creligdata,na.rm=T))
cmodel4 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	, data=aData)
summary(cmodel4)

aData$interaction=aData$polconiii*aData$noS
cmodel8 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ interaction
	, data=aData)
summary(cmodel8)

m1Tab=summary(cmodel4)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(m1Tab)=c('Number of senders',
	'Constraints', 'Distance',
	'GDP per Capita (lagged)',
	'Internal Conflcit',
	'Trade', 'Ally',
	'IGO', "Rec'd Sanctions",
	'Religion')
m1Tab=xtable(m1Tab)
setwd(pathGraphics)
# save(m1Tab, file='mod1.tex')

m2Tab=summary(cmodel8)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(m2Tab)=c('Number of senders',
	'Constraints', 'Distance', 
	'GDP per Capita (lagged)',
	'Internal Conflcit',
	'Trade', 'Ally',
	'IGO', "Rec'd Sanctions",
	'Religion', 'Senders*Constraints')
m2Tab=xtable(m2Tab)
setwd(pathGraphics)
# save(m2Tab, file='mod2.tex')

###
# Vars to generate survival plots for:
	# noS, distance, ally, igo, religion
simModel=cmodel8
pcolors=brewer.pal(9,'Blues')[c(7,9)]
vrfn=function(x){c(min(x,na.rm=T),max(x,na.rm=T))}

plot(survfit(simModel, 
		scenBuild(vi='noS', vRange=c(1,2),
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=T, col=pcolors, las=1)

plot(survfit(simModel, 
		scenBuild(vi='distdata', vRange=vrfn(aData$distdata),
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=T, col=pcolors, las=1)

plot(survfit(simModel, 
		scenBuild(vi='allydata', vRange=vrfn(aData$allydata),
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=T, col=pcolors, las=1)

plot(survfit(simModel, 
		scenBuild(vi='igodata', vRange=vrfn(aData$igodata),
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=T, col=pcolors, las=1)

plot(survfit(simModel, 
		scenBuild(vi='Creligdata', vRange=vrfn(aData$Creligdata),
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=T, col=pcolors, las=1)