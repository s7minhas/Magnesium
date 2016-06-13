if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('~/ProjectsGit/Magnesium/R/Setup.R')}

# Gen tikz
genTikz=TRUE

###############################################################
# Variable key
varDef = cbind (  
	c( 'lag1_uData', 'lag1_SuData2'
	,'noS', 'Ddistdata', 'lag1_tdata', 'lag1_allydata'
	,'lag1_polity2'
	,'lag1_lgdpCAP', 'lag1_gdpGR'
	,'lag1_lpopulation'	 
	,'lag1_domSUM' ),
	c( 'Compliance Reciprocity$_{j,t-1}$', 'Sanction Reciprocity$_{j,t-1}$'
	,'Number of Senders$_{j}$', 'Distance$_{j}$', 'Trade$_{j,t-1}$', 'Ally$_{j,t-1}$'
	,'Polity$_{i,t-1}$'
	,'Ln(GDP per capita)$_{i,t-1}$', 'GDP Growth$_{i,t-1}$'
	,'Population$_{i,t-1}$'	
	,'Internal Conflict$_{i,t-1}$' ) )

# Load data and run models
securDataPath = paste0(pathData, c('/durData_SancOnly_secur.rda', '/durDataImp_SancOnly_secur.rda') )

mods = lapply(securDataPath, function(x){
	load(x)
	ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
	names(ids)=c('targetstate','fcode')
	aData=merge(aData,ids,by='targetstate',all.x=T)

	# Subsetting to model data
	# 2005 is the year the TIES database stops tracking compliance events
	aData = aData[aData$year <= 2005, ]
	idVars=names(aData)[1:19]
	modData=aData[, c( idVars, varDef[,1] )]

	mod=coxph(Surv(start,stop,compliance) ~
		lag1_uData + lag1_SuData2 
		+ noS + Ddistdata + lag1_tdata + lag1_allydata
		+ lag1_polity2 
		+ lag1_lgdpCAP + lag1_gdpGR	+ lag1_lpopulation	 
		+ lag1_domSUM
		, data=modData)
	return(mod)
	})
############################################################### 

############################################################### 
# Table for TeX
durTables=durTable(mods, varDef)
tableName=paste0(pathTex, '/durModelResultsNoImp_secur.tex')
label='tab:regResultsNonEconSanctions'
caption='Here we focus on predicting the time until compliance for sanctions not related to economic issues. The first column shows duration model results on unimputed data with time varying covariates estimated using Cox Proportional Hazards, and the second on imputed data. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
if(genTikz){ print.xtable( xtable(durTables, align='llcc', 	
	caption=caption,
	label=label
	),
	include.rownames=FALSE, sanitize.text.function=identity,
	hline.after=c(0,0,4,12,nrow(varDef)*2, nrow(varDef)*2+3,nrow(varDef)*2+3),
	size='normalsize',
	file=tableName
	) }
############################################################### 