if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

setwd(pathData)
load('durDataEcon.rda')

ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)

# Variable key
varDef = cbind (  
	c( 'noS', 'distdata', 'tdata', 'allydata', 'igodata', 'Creligdata',
	 'lag1_sancSenCnt', 'lag1_sancRecCnt', 
	 'lag1_polconiii', 'lag1_lgdpCAP', 'lag1_Internal.Conflict'),
	c( 'Number of Senders$_{j,t}$', 
	'Distance$_{j,t}$', 'Trade$_{j,t}$', 'Ally$_{j,t}$', 'IGOs$_{j,t}$', 
	'Religion$_{j,t}$', 
	"Sanc. Sent$_{j,t-1}$", "Sanc. Rec'd$_{i,t-1}$",
	'Constraints$_{i,t-1}$',
	'Ln(GDP per capita)$_{i,t-1}$', 'Internal Stability$_{i,t-1}$' )
	)

# Only state-specific non-network/rel. measures
model1 = coxph(Surv(start, stop, compliance) ~ 
	lag1_polconiii + lag1_Internal.Conflict
	+ lag1_lgdpCAP + lag1_gdpGR
	, data=aData)

# Only network/rel. measures
model2 = coxph(Surv(start, stop, compliance) ~ 
	noS + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancSenCnt + lag1_sancRecCnt
	, data=aData)

# "network/relational" variables unlagged
model3 = coxph(Surv(start, stop, compliance) ~ 
	noS + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancSenCnt + lag1_sancRecCnt
	+ lag1_polconiii + lag1_Internal.Conflict
	+ lag1_lgdpCAP + lag1_gdpGR
	, data=aData)

setwd(pathTex)
durTables=durTable(list(model1, model2, model3), varDef)
print.xtable( xtable(durTables, align='llccc', 
	caption='Results of duration models with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
	),
	include.rownames=FALSE, sanitize.text.function=identity,
	hline.after=c(0,0,12,16,nrow(varDef)*2, nrow(varDef)*2+3,nrow(varDef)*2+3),
	size='normalsize',
	file='durModelResults.tex'
	)

###
# Vars to generate survival plots for:
	# noS, distance, ally, igo, religion
simModel=model3
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