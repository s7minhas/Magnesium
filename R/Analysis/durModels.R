if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('~/ProjectsGit/Magnesium/R/Setup.R')}

# Gen tikz
genTikz=FALSE

# for(imputeLogical in c(TRUE, FALSE)){
# Use imputed data
# impute=imputeLogical
impute=TRUE
###############################################################
setwd(pathData)
if(!impute){load('durDataEcon_SancOnly.rda'); tableName='durModelResultsNoImp.tex'; label='tab:regResultsNoImp'; caption='Duration model on unimputed data with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'}
if(impute){load('durDataEconImp_SancOnly.rda'); tableName='durModelResults.tex'; label='tab:regResults'; caption = 'Duration model with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'}

ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)
length(unique(aData$caseid))
###############################################################

###############################################################
# Variable key
varDef = cbind (  
	c( 'lag1_uData', 'lag1_SuData2'
	,'noS', 'Ddistdata', 'lag1_tdata', 'lag1_allydata'
	,'lag1_polity2'
	,'lag1_lgdpCAP', 'lag1_gdpGR'
	,'lag1_lpopulation'	 
	,'lag1_domSUM'
	 ),
	c( 'Compliance Reciprocity$_{j,t-1}$', 'Sanction Reciprocity$_{j,t-1}$'
	,'Number of Senders$_{j,t}$', 'Distance$_{j,t}$', 'Trade$_{j,t}$', 'Ally$_{j,t}$'
	,'Polity$_{i,t-1}$'
	,'Ln(GDP per capita)$_{i,t-1}$', 'GDP Growth$_{i,t-1}$'
	,'Population$_{i,t-1}$'	
	,'Internal Conflict$_{i,t-1}$' 
	)
	)

# Subsetting to model data
# 2005 is the year the TIES database stops tracking compliance events
aData = aData[aData$year <= 2005, ]
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
# Summary Statistics
summStat = function(x){ c(length(x), mean(x), median(x), sd(x), min(x), max(x) ) }

setwd(pathTex)
if(impute){ summTable='summStatsImp.tex'; summCaption='Summary statistics of parameters included in duration model using imputed data.'; summLabel='tab:summImp' } 
if(!impute){ summTable='summStatsNoImp.tex'; summCaption='Summary statistics of parameters included in duration model using original data.'; summLabel='tab:summNoImp' } 
aVars=c('compliance',varDef[,1])
summData=na.omit(data.matrix(modData[,aVars]))
summ = apply(summData, 2, summStat)
summ[1,] = round(summ[1,], 0)
summ[2:nrow(summ),] = round(summ[2:nrow(summ),], 2)
rownames(summ) = c('N', 'Mean', 'Median', 'Std. Dev.', 'Min.', 'Max.')
summ = cbind(Variable=c('Compliance', varDef[,2]), t(summ))

if(genTikz){
	print.xtable( xtable(summ, align='llcccccc',
		caption=summCaption,
		label=summLabel),
		include.rownames=FALSE, sanitize.text.function=identity,
		hline.after=c(0,0,nrow(summ),nrow(summ)),
		size='normalsize', file=summTable
	)
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
	'noS', 'Ddistdata', 'lag1_polity2', 'lag1_lgdpCAP')
riskRatios=t(mapply(x=riskVars, 
	function(x) FUN=riskRatio(1000, modelFinal, modData, x)))
riskRatios
############################################################### 

############################################################### 
# Survival plots
vrfn=function(x,lo=.1,hi=.9){numSM(quantile(x,probs=c(lo,hi),na.rm=T))}
survPlot=function(
	model=modelFinal, coef, data=modData, cRange=vrfn(data[,coef]), 
	confInts = c(.95, .9), timeOut = 20, 
	savePlot=genTikz & impute, pheight=4, pwidth=6, plotName){
	
	# Create predictions using survfit
	surv = lapply(confInts, function(ci){
		survfit(model, 
			scenBuild(vi=coef, vRange=cRange, 
				vars=names(model$coefficients), 
				ostat=mean, simData=modData),
			conf.int=ci ) } )
	
	# Organize data
	survData = do.call('cbind', lapply(surv, function(s){
		survData=cbind(melt(s$surv)[,2:3], lower=melt(s$lower)[,3], upper=melt(s$upper)[,3])
		survData$time = rep(s$time, 2)
		return(survData[survData$time <=timeOut, ])
		}) )
	survData=survData[,c(1:4,8:10)]
	names(survData) = c('scen','surv','lo95','up95','lo90','up90','Time')
	survData$scen=factor(survData$scen, levels=1:2)

	# Plot
	gg=ggplot(survData, aes(x=Time, y=surv, fill=scen, color=scen))
	gg=gg + geom_line() + xlab('Time (Years)') + ylab('Survival Probability')
	gg=gg + geom_ribbon(aes(ymin=lo95,ymax=up95),alpha=0.15,color=NA)
	gg=gg + geom_ribbon(aes(ymin=lo90,ymax=up90),alpha=0.2,color=NA)
	gg=gg + theme(legend.position='none', legend.title=element_blank(),
		    axis.ticks=element_blank(), panel.grid.major=element_blank(),
		    panel.grid.minor=element_blank(), panel.border = element_blank(),
		    axis.line = element_line(color = 'black'))
	if(savePlot){
		tikz(file=plotName, height=pheight, width=pwidth, standAlone=F)
		print(gg); dev.off() } else { print( gg ) }
}

setwd(pathTex)
survPlot(coef='noS', plotName='nosSurv.tex', 
	cRange=vrfn(aData[,'noS'],lo=0,hi=1)) # sig | T
survPlot(coef='Ddistdata', plotName='distSurv.tex') # sig | T
survPlot(coef='lag1_polity2', plotName='polSurv.tex') # sig | T
survPlot(coef='lag1_lgdpCAP', plotName='gdpSurv.tex') # not sig
survPlot(coef='lag1_uData', plotName='compRecSurv.tex') # sig | T
survPlot(coef='lag1_SuData2', plotName='sancRecSurv.tex') # sig | T
###############################################################
}