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
cmodel4 = coxph(Surv(start, stop, compliance) ~ 
	noS + lag1_polity + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + religdata
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
plot(survfit(cmodel4, scenBuild(vi='noS', vRange=1:3,
	vars=names(cmodel4$coefficients), 
	ostat=mean, simData=aData)),
	conf.int=T, col=c('red','blue','green') )