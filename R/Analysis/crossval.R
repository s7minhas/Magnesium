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
set.seed(6886); rands=sample(rep(1:6, 1000)[1:length(caseid)])
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
# Inputs
times=seq(1,16,1)
idVars=idVars[c(1:10,17:19)]
m1V=which(varDef[,1] =='lag1_polity2')
m2V=which(varDef[,1]=='noS')
m1F=formula(Surv(start, stop, compliance) ~ lag1_polity2 + lag1_lgdpCAP + lag1_gdpGR + lag1_lpopulation + lag1_domSUM)
m2F=formula(Surv(start, stop, compliance) ~ noS + Ddistdata + lag1_tdata + lag1_allydata + lag1_polity2 + lag1_lgdpCAP + lag1_gdpGR + lag1_lpopulation + lag1_domSUM)
mFF=formula(Surv(start,stop,compliance) ~ lag1_uData + lag1_SuData2 + noS + Ddistdata + lag1_tdata + lag1_allydata + lag1_polity2 + lag1_lgdpCAP + lag1_gdpGR + lag1_lpopulation + lag1_domSUM)

# Run k-fold cross val with validation
ggAUC=NULL
for(ii in 1:length(unique(modData$rands))){
	train=modData[which(!modData$rands %in% ii),]
	test=modData[which(modData$rands %in% ii),]

	m1DataTrain=na.omit(train[,c(idVars,varDef[m1V:nrow(varDef),1]) ])
	m2DataTrain=na.omit(train[,c(idVars,varDef[m2V:nrow(varDef),1]) ])
	mFDataTrain=na.omit(train[,c(idVars,varDef[,1]) ])

	m1DataTest=na.omit(test[,c(idVars,varDef[m1V:nrow(varDef),1]) ])
	m2DataTest=na.omit(test[,c(idVars,varDef[m2V:nrow(varDef),1]) ])
	mFDataTest=na.omit(test[,c(idVars,varDef[,1]) ])

	modTrain1 = coxph(m1F, data=m1DataTrain)
	modTrain2 = coxph(m2F, data=m2DataTrain)
	modTrainF=coxph(mFF, data=mFDataTrain)

	aucM1=AUC.cd(
		Surv(m1DataTrain$start, m1DataTrain$stop, m1DataTrain$compliance), 
		Surv(m1DataTest$start, m1DataTest$stop, m1DataTest$compliance), 
		predict(modTrain1),
		predict(modTrain1, newdata=m1DataTest),
		times=times
		)

	aucM2=AUC.cd(
		Surv(m2DataTrain$start, m2DataTrain$stop, m2DataTrain$compliance), 
		Surv(m2DataTest$start, m2DataTest$stop, m2DataTest$compliance), 
		predict(modTrain2),
		predict(modTrain2, newdata=m2DataTest),
		times=times
		)

	aucMF=AUC.cd(
		Surv(mFDataTrain$start, mFDataTrain$stop, mFDataTrain$compliance), 
		Surv(mFDataTest$start, mFDataTest$stop, mFDataTest$compliance), 
		predict(modTrainF),
		predict(modTrainF, newdata=mFDataTest),
		times=times
		)

	aucData=data.frame( rbind(
		cbind(aucM1$times, aucM1$auc, 'Model 1\\qquad\\qquad'),
		cbind(aucM2$times, aucM2$auc, 'Model 2\\qquad\\qquad'),
		cbind(aucMF$times, aucMF$auc, 'Model 3')
		) )
	aucData$X1=numSM(aucData$X1); aucData$X2=numSM(aucData$X2)

	ggAUC=rbind(ggAUC,cbind(aucData, Fold=ii))
}
###############################################################

###############################################################
# Plotting
ggAUC$Fold=paste0('Fold ', ggAUC$Fold)

pgg=ggplot(ggAUC, aes(x=X1, y=X2, color=X3, group=X3))
pgg=pgg+geom_line(aes(linetype=X3),lwd=1,color='black')
pgg=pgg+facet_wrap(~Fold, scales="free_y")
pgg=pgg+scale_x_continuous(breaks=seq(0,15,3),limits=c(0,16))
pgg=pgg+scale_y_continuous(breaks=seq(0.6,1,0.1),limits=c(0.59,1.01))
pgg=pgg+xlab('Time (years)')+ylab('Time-dependent AUC')
pgg=pgg+theme(legend.position='top', legend.title=element_blank(),
      axis.ticks=element_blank(), panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(), panel.border = element_blank(),
      axis.line = element_line(color = 'black'),
      axis.title.y=element_text(vjust=1))
pgg
setwd(pathTex)
tikz(file='crossvalPerf.tex', height=5, width=7, standAlone=F)
pgg
dev.off()
###############################################################

###############################################################
# Run models on subsets
models=list()
for( ii in 1:length(unique(modData$rands)) ){
	slice=modData[which(!modData$rands %in% ii),]
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
# coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
#                 "Negative"= rgb(222, 45, 38, maxColorValue=255),
#                 "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
#                 "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
#                 "Insig" = rgb(150, 150, 150, maxColorValue=255))
coefp_colors = c("Positive"='black', 
                "Negative"='black',
                "Positive at 90"='black', 
                "Negative at 90"= 'black',
                "Insig" = 'grey')

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
coefp = coefp + facet_wrap(~varName, scales="free_y")#, nrow=1,ncol=2)
coefp = coefp + scale_x_discrete(labels=paste0('Fold ',1:10))
coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
    axis.ticks=element_blank(), panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(), 
    axis.text.x=element_text(angle=45,hjust=1))
coefp
setwd(pathTex)
tikz(file='crossvalCoef.tex', height=3, width=7, standAlone=F)
coefp
dev.off()
##############################################################