if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('~/ProjectsGit/Magnesium/R/Setup.R')}

# Gen tikz
genTikz=T

###############################################################
setwd(pathData)
# load('durDataEcon.rda'); tableName='durModelResultsNoImp.tex'; label='tab:regResultsNoImp'; caption='Duration model on unimputed data with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
# load('durDataEconImp.rda'); tableName='durModelResults_Sanction.tex'; label='tab:regResults'; caption = 'Duration model with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
load('durDataEconImp_SancOnly.rda'); tableName='durModelResults_Sanction.tex'; label='tab:regResults'; caption = 'Duration model with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'

ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)
###############################################################

###############################################################
# Add info about sanction imposition
load('sanctionData.rda')
aData$imposition = sanctionDataFinal$imposition[match(aData$caseid, sanctionDataFinal$caseid)]

# Split results by imposition and not
aData = aData[aData$imposition == 1,]
###############################################################

###############################################################
# Variable key
varDef = cbind (  
	c( 'lag1_uData', 'lag1_SuData2'
		,'lag1_actor', 'lag1_Spartner'
		,'lag1_SmeanActorSndr', 'lag1_meanPtnrSndr'
		,'noS', 'Ddistdata', 'lag1_tdata', 'lag1_allydata'
	 ,'lag1_polity2'
	 ,'lag1_lgdpCAP', 'lag1_gdpGR'
	 ,'lag1_lpopulation'	 
	 ,'lag1_domSUM'
	 ),
	c( 'Compliance Reciprocity$_{j,t-1}$', 'Sanction Reciprocity$_{j,t-1}$'
		,'Compliant Countries', 'Freq. Sanctioned' 
		,'Freq. Sanct Senders', 'Effective Sanctioners'
	,'Number of Senders$_{j,t}$', 'Distance$_{j,t}$', 'Trade$_{j,t}$', 'Ally$_{j,t}$'
	,'Polity$_{i,t-1}$'
	,'Ln(GDP per capita)$_{i,t-1}$', 'GDP Growth$_{i,t-1}$'
	,'Population$_{i,t-1}$'	
	,'Internal Conflict$_{i,t-1}$' 
	)
	)

# Subsetting to model data
# aData = aData[aData$year <=2005, ]
idVars=names(aData)[1:19]
modData=aData[, c( idVars, varDef[,1] )]
###############################################################

###############################################################
# Missingness check
if(tableName=='durModelResultsNoImp.tex'){
	aVars=c('start','stop','compliance',varDef[,1])
	omitComp=na.omit(data.matrix(modData[,aVars]))[,'compliance']
	fullComp=data.matrix(modData)[,'compliance']
	table(omitComp); table(fullComp)
}
###############################################################

###############################################################
# Only state-specific measures
model1 = coxph(Surv(start, stop, compliance) ~ 
	+ lag1_polity2 
	+ lag1_lgdpCAP + lag1_gdpGR	+ lag1_lpopulation	 
	+ lag1_domSUM
	, data=modData)
summary(model1)

# State-specific and rel. measures
model2 = coxph(Surv(start, stop, compliance) ~ 
	+ noS + Ddistdata + lag1_tdata + lag1_allydata
	+ lag1_polity2 
	+ lag1_lgdpCAP + lag1_gdpGR	+ lag1_lpopulation	 
	+ lag1_domSUM
	, data=modData)
summary(model2)

# Incorp reciprocity measure
modelFinal=coxph(Surv(start,stop,compliance) ~
	lag1_uData + lag1_SuData2 
	# + lag1_actor 
	+ noS + Ddistdata + lag1_tdata + lag1_allydata
	+ lag1_polity2 
	+ lag1_lgdpCAP + lag1_gdpGR	+ lag1_lpopulation	 
	+ lag1_domSUM
	# + frailty.gamma(as.factor(targetstate), sparse=FALSE)
	, data=modData)
summary(modelFinal)
###############################################################

############################################################### 
# Diagnostics

# Nonlinearity in continuous covariates
# pspline

# # Testing proportionality assumption, 99% CI
# cox.zph(modelFinal)
# par(mfrow=c(3,4))
# plot(cox.zph(modelFinal, transform='identity'))
# par(mfrow=c(1,1))

# No Evid of nonprop, if there was then would interact with log.time
# modData$gdpGRStop=I(modData$lag1_gdpGR*log(modData$stop))
############################################################### 

############################################################### 
# Table for TeX
setwd(pathTex)
durTables=durTable(list(model1, model2, modelFinal), varDef)	
if(genTikz){ print.xtable( xtable(durTables, align='llccc', 	
	caption=caption,
	label=label
	),
	include.rownames=FALSE, sanitize.text.function=identity,
	hline.after=c(0,0,4,12,nrow(varDef)*2, nrow(varDef)*2+3,nrow(varDef)*2+3),
	size='normalsize',
	file=tableName
	) }
############################################################### 

############################################################### 
# Risk ratios
riskVars=c('lag1_uData', 'lag1_SuData2',
	'noS', 'Ddistdata', 'lag1_tdata')
riskRatios=t(mapply(x=riskVars, 
	function(x) FUN=riskRatio(1000, modelFinal, modData, x)))
riskRatios
############################################################### 

############################################################### 
###
# Vars to generate survival plots for:
	# noS, distance, ally, igo, religion
simModel=modelFinal
pcolors=append(brewer.pal(9,'Reds')[8],brewer.pal(9,'Blues')[8])
pcolors=c('darkgrey','black')
vrfn=function(x,lo=0,hi=1){numSM(quantile(x,probs=c(lo,hi),na.rm=T))}

setwd(pathTex)
# pdf(file='nosSurv.pdf', height=4, width=6)
if(genTikz){tikz(file='nosSurv.tex', height=4, width=6, standAlone=F)}
plot(
	survfit(simModel, 
		scenBuild(vi='noS', vRange=vrfn(aData[,'noS']),
		vars=names(simModel$coefficients), 
		ostat=mean, simData=modData) ),
	conf.int=T, col=pcolors, las=1, 
	# main='Number of Senders', 
	main='', 
	ylim=c(0.2,1), xlim=c(0,30), 
	ylab='Survival Probability', xlab='Time (Years)', bty='n')
legend('topright', c("Few Senders", "Many Senders"), 
	lty = 1, col=pcolors, bty='n')
if(genTikz){dev.off()}

# pdf(file='oNet.pdf', height=7, width=10)
if(genTikz){tikz(file='oNet.tex', height=3, width=8, standAlone=F)}
coefs=c('Ddistdata','lag1_tdata')
cnames=varDef[match(coefs, varDef[,1]), 2]
cnames=c('Distance','Trade')
par(mfrow=c(1,2))
for(ii in 1:length(coefs)){
	coef=coefs[ii]
	if (coef=='distdata') { crange=c(0.001,0.005)
		} else { crange=vrfn(aData[,coef]) }	
	plot(
		survfit(simModel, 
			scenBuild(vi=coef, vRange=crange,
			vars=names(simModel$coefficients), 
			ostat=mean, simData=modData) ),
	conf.int=F, col=pcolors, las=1,
	main=cnames[ii], ylim=c(0,1), xlim=c(0,30))
	if(ii==1){title(ylab='Survival Prob.')} 
	title(xlab='Time (Years)')  }
if(genTikz){dev.off()}
par(mfrow=c(1,1))

# pdf(file='oNet2.pdf', height=7, width=10)
if(genTikz){tikz(file='oNet2.tex', height=3, width=8, standAlone=F)}
coefs=c('lag1_uData','lag1_SuData2')
cnames=varDef[match(coefs, varDef[,1]), 2]
cnames=c('Compliance Reciprocity','Sanction Reciprocity')
par(mfrow=c(1,2))
for(ii in 1:length(coefs)){
	coef=coefs[ii]
	crange=vrfn(aData[,coef],lo=0,hi=0.9)
	plot(
		survfit(simModel, 
			scenBuild(vi=coef, vRange=crange,
			vars=names(simModel$coefficients), 
			ostat=mean, simData=modData) ),
	conf.int=F, col=pcolors, las=1,
	main=cnames[ii], ylim=c(0,1), xlim=c(0,30))
	if(ii==1){title(ylab='Survival Prob.')} 
	title(xlab='Time (Years)')  }
if(genTikz){dev.off()}
par(mfrow=c(1,1))
############################################################### 