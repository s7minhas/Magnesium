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
aData$lag1_polity2=aData$lag1_polity^2	

# Variable key
varDef = cbind (  
	c( 'noS', 'distdata', 'tdata', 'allydata', 'igodata', 'Creligdata',
	 'lag1_sancSenCnt', 'lag1_sancRecCnt', 
	 'lag1_polity', 'lag1_polity2',
	 'lag1_lgdpCAP', 'lag1_gdpGR','lag1_Internal.Conflict'),
	c( 'Number of Senders$_{j,t}$', 
	'Distance$_{j,t}$', 'Trade$_{j,t}$', 'Ally$_{j,t}$', 'IGOs$_{j,t}$', 
	'Religion$_{j,t}$', 
	"Sanc. Sent$_{j,t-1}$", "Sanc. Rec'd$_{i,t-1}$",
	'Polity$_{i,t-1}$', 'Polity$^{2}_{i,t-1}$',
	'Ln(GDP per capita)$_{i,t-1}$', 'GDP Growth$_{i,t-1}$','Internal Stability$_{i,t-1}$' )
	)

# No imputation
modData=aData

# Impute missing values
# sbgcopTimeSR <- system.time(
#   sbgcopFitSR <- sbgcop.mcmc(aData, 
#   	nsamp=2000, seed=123455, verb=TRUE) ) # default odens = nsamp/1000  


# Only state-specific non-network/rel. measures
model1 = coxph(Surv(start, stop, compliance) ~ 
	# + lag1_polity + lag1_polity2
	+ lag1_polconiii
	+ lag1_lgdpCAP + lag1_gdpGR
	+ lag1_Internal.Conflict
	, data=modData)

# Only network/rel. measures
model2 = coxph(Surv(start, stop, compliance) ~ 
	noS + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancSenCnt + lag1_sancRecCnt
	, data=modData)

# "network/relational" variables unlagged
model3 = coxph(Surv(start, stop, compliance) ~ 
	noS + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancSenCnt + lag1_sancRecCnt
	# + lag1_polity + lag1_polity2
	+ lag1_polconiii
	+ lag1_lgdpCAP + lag1_gdpGR
	+ lag1_Internal.Conflict
	, data=modData)

# To add frailty term
# frailty.gamma(as.factor(caseid), sparse=FALSE)

# Nonlinearity in continuous covariates
# pspline

# Testing proportionality assumption, 99% CI
cox.zph(model3)
par(mfrow=c(3,4))
plot(cox.zph(model3, transform='identity'))
par(mfrow=c(1,1))

# Evid of nonprop then interact with log.time
# modData$distStop=I(modData$distdata*log(modData$stop))
modData$igoStop=I(modData$igodata*log(modData$stop))
modData$CreligStop=I(modData$Creligdata*log(modData$stop))
modData$gdpGRStop=I(modData$lag1_gdpGR*log(modData$stop))

model3v2 = coxph(Surv(start, stop, compliance) ~ 
	noS + distdata + tdata + allydata + igodata + Creligdata 
	+ lag1_sancSenCnt + lag1_sancRecCnt
	# + lag1_polity + lag1_polity2
	+ lag1_polconiii
	+ lag1_lgdpCAP + lag1_gdpGR
	+ lag1_Internal.Conflict
	# + distStop 
	+ igoStop 
	+ CreligStop
	+ gdpGRStop
	, data=modData)
cox.zph(model3v2) 

# Compare
summary(model3)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
summary(model3v2)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]

# Table for TeX
setwd(pathTex)
# durTables=durTable(list(model1, model2, model3v2), varDef)
# print.xtable( xtable(durTables, align='llccc', 
durTables=durTable(list(model3v2), varDef)	
print.xtable( xtable(durTables, align='llc', 	
	caption='Duration model with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
	),
	include.rownames=FALSE, sanitize.text.function=identity,
	hline.after=c(0,0,12,16,nrow(varDef)*2, nrow(varDef)*2+3,nrow(varDef)*2+3),
	size='normalsize',
	file='durModelResults.tex'
	)

# Risk ratios
riskVars=c('noS', 'distdata', 'allydata', 'Creligdata', 
	'lag1_sancSenCnt', 'lag1_sancRecCnt', 
	'lag1_polconiii')
riskRatios=t(mapply(x=riskVars, function(x) FUN=riskRatio(1000, model3v2, modData, x)))

###
# Vars to generate survival plots for:
	# noS, distance, ally, igo, religion
simModel=model3v2
pcolors=append(brewer.pal(9,'Reds')[8],brewer.pal(9,'Blues')[8])
vrfn=function(x){c(min(x,na.rm=T),max(x,na.rm=T))}

setwd(pathTex)
# pdf(file='nosSurv.pdf', height=4, width=6)
tikz(file='nosSurv.tex', height=4, width=6, standAlone=F)
plot(survfit(simModel, 
	scenBuild(vi='noS', vRange=c(1,5),
	vars=names(simModel$coefficients), 
	ostat=mean, simData=modData) ),
	conf.int=F, col=pcolors, las=1,
	# main='Number of Senders', 
	main='', 
	ylim=c(0,1), xlim=c(0,30), 
	ylab='Survival Probability', xlab='Time (Years)', bty='n')
legend('topright', c("Few Senders", "Many Senders"), 
	lty = 1, col=pcolors, bty='n')
dev.off()

# pdf(file='oNet.pdf', height=7, width=10)
tikz(file='oNet.tex', height=3, width=8, standAlone=F)
coefs=c('distdata','allydata','Creligdata')
cnames=varDef[match(coefs, varDef[,1]), 2]
cnames=c('Distance','Ally', 'Religion')
par(mfrow=c(1,3))
for(ii in 1:length(coefs)){
	coef=coefs[ii]
	if (coef=='distdata') { crange=c(0.001,0.005)
		} else { crange=vrfn(aData[,coef]) }	
	plot(survfit(simModel, 
		scenBuild(vi=coef, vRange=crange,
		vars=names(simModel$coefficients), 
		ostat=mean, simData=modData) ),
	conf.int=F, col=pcolors, las=1,
	main=cnames[ii], ylim=c(0.4,1), xlim=c(0,30))
	if(ii==1){title(ylab='Survival Prob.')} 
	title(xlab='Time (Years)')  }
dev.off()
par(mfrow=c(1,1))

###
tikz(file='oNet2.tex', height=3, width=8, standAlone=F)
coefs=c('lag1_sancSenCnt','lag1_sancRecCnt')
cnames=varDef[match(coefs, varDef[,1]), 2]
par(mfrow=c(1,2))
for(ii in 1:length(coefs)){
	coef=coefs[ii]
	if (coef=='distdata') { crange=c(0.001,0.005)
		} else { crange=vrfn(aData[,coef]) }	
	plot(survfit(simModel, 
		scenBuild(vi=coef, vRange=crange,
		vars=names(simModel$coefficients), 
		ostat=mean, simData=modData) ),
	conf.int=F, col=pcolors, las=1,
	main=cnames[ii], ylim=c(0.4,1), xlim=c(0,30))
	if(ii==1){title(ylab='Survival Prob.')} 
	title(xlab='Time (Years)') } 
dev.off()
par(mfrow=c(1,1))