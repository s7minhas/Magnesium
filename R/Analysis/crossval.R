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
set.seed(2312); rands=sample(rep(1:3, 1000)[1:length(caseid)])
idsRand=data.frame(cbind(caseid, rands))
aData=merge(aData, idsRand, by='caseid', all.x=T)
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
	,'Number of Senders$_{j,t}$', 'Distance$_{j,t}$', 
	'Trade$_{j,t-1}$', 'Ally$_{j,t-1}$'
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
		+ noS + Ddistdata + lag1_tdata + lag1_allydata
		+ lag1_polity2 
		+ lag1_lgdpCAP + lag1_gdpGR	+ lag1_lpopulation	 
		+ lag1_domSUM
		# + frailty.gamma(as.factor(targetstate), sparse=FALSE)
		, data=slice)
	models[[ii]]=modelFinal
}

# Pull out results from each model into one dataframe
coefData=NULL
for(ii in 1:length(models)){
	cdata=coeftest(models[[ii]])[,1:2]
	cdata=cbind(cdata, model=ii)
	coefData=rbind(coefData,cdata)
}
coefData=cbind(var=rownames(coefData), coefData)
coefData=data.frame(coefData, row.names=NULL)
coefData$Estimate=numSM(coefData$Estimate)
coefData$Std..Error=numSM(coefData$Std..Error)
coefData$model=numSM(coefData$model)
###############################################################

###############################################################
# Generate coefficient plot for each model
coefData$upper95=coefData$Estimate+qnorm(.975)*coefData$Std..Error
coefData$lower95=coefData$Estimate-qnorm(.975)*coefData$Std..Error
coefData$upper90=coefData$Estimate+qnorm(.95)*coefData$Std..Error
coefData$lower90=coefData$Estimate-qnorm(.95)*coefData$Std..Error

# Add in variable for colors
coefData$sig = NULL
coefData$sig[coefData$lower90 > 0 & coefData$lower95 < 0] = "Positive at 90"
coefData$sig[coefData$lower95 > 0] = "Positive"
coefData$sig[coefData$upper90 < 0 & coefData$upper95 > 0] = "Negative at 90"
coefData$sig[coefData$upper95 < 0] = "Negative"
coefData$sig[coefData$lower90 < 0 & coefData$upper90 > 0] = "Insig"
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
                "Negative"= rgb(222, 45, 38, maxColorValue=255),
                "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
                "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
                "Insig" = rgb(150, 150, 150, maxColorValue=255))

coefData=coefData[which(coefData$var %in% c('lag1_uData','lag1_SuData2')),]
coefData$varName=varDef[,2][match(coefData$var, varDef[,1])]

coefp = ggplot(coefData, aes(as.factor(model), Estimate, color = sig))
coefp = coefp + geom_linerange(aes(ymin=lower95, ymax=upper95), 
	alpha = .3, size = 0.3)
coefp = coefp + geom_linerange(aes(ymin=lower90, ymax=upper90),
	alpha = 1, size = 1)
coefp = coefp + geom_hline(aes(yintercept=0), linetype=2, color = "black")
coefp = coefp + geom_point(aes(as.factor(model),Estimate), size=4, shape=20)
coefp = coefp + geom_errorbar(aes(ymin=lower95,ymax=upper95),
	linetype = 1,width = 0.1)
coefp = coefp + scale_colour_manual(values = coefp_colors)
coefp = coefp + xlab("") + ylab("") 
coefp = coefp + facet_wrap(~varName, scales="free_y", nrow=1,ncol=2)
coefp = coefp + scale_x_discrete(labels=paste0('Fold ',1:3))
coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
    axis.ticks=element_blank(), panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())
coefp
###############################################################