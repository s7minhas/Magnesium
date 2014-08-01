if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

# Gen tikz
genTikz=F

###############################################################
setwd(pathData)
load('durDataEconImp.rda'); tableName='durModelResults.tex'; label='tab:regResults'; caption = 'Duration model with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'

ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)
###############################################################

###############################################################
# Splitting up dataframe into random subsets of cases
caseid=unique(aData$caseid)
set.seed(6886); rands=sample(c(rep(1:5, 130),1:3))
idsRand=data.frame(cbind(caseid, rands))
aData=merge(aData, idsRand, by='caseid', all.x=T)
###############################################################

###############################################################
# Variable key
varDef = cbind (  
	c( 'lag1_uData', 'lag1_SuData2'
		,'lag1_actor','lag1_partner'
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
# aData = aData[aData$year <=2005, ]
idVars=c(names(aData)[1:19],'rands')
modData=aData[, c( idVars, varDef[,1] )]
###############################################################

###############################################################
# Run models on subsets
models=list()
for( ii in 1:length(unique(modData$rands)) ){
	slice=modData[which(modData$rands %in% rands[ii]),]
	modelFinal=coxph(Surv(start,stop,compliance) ~
		lag1_uData + lag1_SuData2 
		+ lag1_actor + lag1_partner
		+ noS + Ddistdata + lag1_tdata + lag1_allydata
		+ lag1_polity2 
		+ lag1_lgdpCAP + lag1_gdpGR	+ lag1_lpopulation	 
		+ lag1_domSUM
		# + frailty.gamma(as.factor(targetstate), sparse=FALSE)
		, data=slice)
	models[[ii]]=modelFinal
}
###############################################################

###############################################################
# Generate coefficient plot for each model

###############################################################