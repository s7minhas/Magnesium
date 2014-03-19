if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

setwd(pathData)
load('durDataEcon.rda')

ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)

# Var mods
aData$interaction=aData$lag1_polconiii*aData$noS

# Variable key
varDef = cbind (  
	c( 'noS', 'interaction', 'distdata', 'tdata', 'allydata', 'igodata', 'Creligdata',
	 'lag1_sancRecCnt', 'lag1_polconiii', 'lag1_lgdpCAP', 'lag1_Internal.Conflict'),
	c( 'Number of Senders$_{j,t}$', 'Senders$_{j,t}$*Constraints$_{i,t-1}$',
	'Distance$_{j,t}$', 'Trade$_{j,t}$', 'Ally$_{j,t}$', 'IGOs$_{j,t}$', 
	'Religion$_{j,t}$', "Sanc. Rec'd$_{i,t}$", 'Constraints$_{i,t}$',
	'Ln(GDP per capita)$_{i,t}$', 'Internal Stability$_{i,t}$' )
	)

# not lagging everything
# incudes: controls + senders & distance hypo + net hypo
# ***pretty interesting results
cmodel4 = coxph(Surv(start, stop, compliance) ~ 
	noS + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancRecCnt + lag1_polconiii + lag1_lgdpCAP + lag1_Internal.Conflict
	, data=aData)
temp=na.omit(aData[,c('caseid',names(cmodel4$coefficients))])
length(unique(temp$caseid))

cmodel8 = coxph(Surv(start, stop, compliance) ~ 
	noS + interaction + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancRecCnt + lag1_polconiii + lag1_lgdpCAP + lag1_Internal.Conflict
	, data=aData)

m1Tab=summary(cmodel4)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(m1Tab)=varDef[ match(rownames(m1Tab),varDef[,1]) , 2 ]
m1Tab=xtable(m1Tab)
setwd(pathGraphics)
# save(m1Tab, file='mod1.tex')

m2Tab=summary(cmodel8)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(m2Tab)=varDef[ match(rownames(m2Tab),varDef[,1]) , 2 ]
m2Tab=xtable(m2Tab)
setwd(pathGraphics)
# save(m2Tab, file='mod2.tex')

###
# Vars to generate survival plots for:
	# noS, distance, ally, igo, religion
simModel=cmodel8
pcolors=append(brewer.pal(9,'Reds')[8],brewer.pal(9,'Blues')[8])
vrfn=function(x){c(min(x,na.rm=T),max(x,na.rm=T))}

setwd(pathGraphics)
pdf(file='nosSurv.pdf', height=4, width=6)
plot(survfit(simModel, 
	scenBuild(vi='noS', vRange=c(1,2),
	vars=names(simModel$coefficients), 
	ostat=mean, simData=aData) ),
	conf.int=F, col=pcolors, las=1,
	# main='Number of Senders', 
	main='', 
	ylim=c(0.4,1), xlim=c(0,30), 
	ylab='Survival Probability', xlab='Time (Years)', bty='n')
legend('topright', c("Few Senders", "Many Senders"), 
	lty = 1, col=pcolors, bty='n')
dev.off()

pdf(file='oNet.pdf', height=7, width=10)
coefs=c('distdata','allydata','igodata','Creligdata')
cnames=c('Distance','Ally', 'IGO', 'Religion')
par(mfrow=c(2,2))
for(ii in 1:length(coefs)){
	coef=coefs[ii]
	if (coef=='distdata') { crange=c(0.001,0.005)
		} else { crange=vrfn(aData[,coef]) }	
	plot(survfit(simModel, 
		scenBuild(vi=coef, vRange=crange,
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=F, col=pcolors, las=1,
	main=cnames[ii], ylim=c(0.4,1), xlim=c(0,30))
	if(ii%%2){title(ylab='Survival Prob.')} 
	if(ii==3|ii==4){title(xlab='Time (Years)') } }
dev.off(); par(mfrow=c(1,1))