if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

###############################################################
setwd(pathData)
load('durDataEconImp.rda')

ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)
###############################################################

###############################################################
# Variable key
varDef = cbind (  
	c( 'lag1_uData', 'lag1_SuData2'
		# ,'lag1_actor'
		,'noS', 'Ddistdata', 'lag1_tdata', 'lag1_allydata'
	 ,'lag1_polity2'
	 ,'lag1_lgdpCAP', 'lag1_gdpGR'
	 ,'lag1_lpopulation'	 
	 ,'lag1_domSUM'
	 ),
	c( 'Compliance Reciprocity$_{j,t-1}$', 'Sanction Reciprocity$_{j,t-1}$'
		# ,'Actor Effect'
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
# Train/test
caseid=unique(modData$caseid)
set.seed(2312); train=rbinom(length(caseid), 1,0.75)
idsTrain=data.frame(cbind(caseid, train))
modData=merge(modData, idsTrain, by='caseid', all.x=T)

train=modData[which(modData$train %in% 1),]
test=modData[which(modData$train %in% 0),]
###############################################################

###############################################################
# Inputs
idVars=idVars[c(1:10,17:19)]
m1Data=na.omit(modData[,c(idVars,varDef[7:nrow(varDef),1]) ])
m2Data=na.omit(modData[,c(idVars,varDef[3:nrow(varDef),1]) ])
mFData=na.omit(modData[,c(idVars,varDef[,1]) ])
times=seq(1,26,5)

predM1 <- predict(model1, type = "risk")
predM2 <- predict(model2, type = "risk")
predMF <- predict(modelFinal, type = "risk")
###############################################################

###############################################################
# Cumulative AUC
aucM1=AUC.cd(
	Surv(m1Data$start, m1Data$stop, m1Data$compliance), 
	Surv(m1Data$start, m1Data$stop, m1Data$compliance), 
	predict(model1),
	predict(model1),
	times=times
	)

aucM2=AUC.cd(
	Surv(m2Data$start, m2Data$stop, m2Data$compliance), 
	Surv(m2Data$start, m2Data$stop, m2Data$compliance), 
	predict(model2),
	predict(model2),
	times=times
	)

aucMF=AUC.cd(
	Surv(mFData$start, mFData$stop, mFData$compliance), 
	Surv(mFData$start, mFData$stop, mFData$compliance), 
	predict(modelFinal),
	predict(modelFinal),
	times=times
	)

print(aucM1$iauc); print(aucM2$iauc); print(aucMF$iauc)

ggAUC=data.frame( rbind(
	cbind(aucM1$times, aucM1$auc, 'Model 1\\qquad\\qquad'),
	cbind(aucM2$times, aucM2$auc, 'Model 2\\qquad\\qquad'),
	cbind(aucMF$times, aucMF$auc, 'Model 3')
	) )
ggAUC$X1=numSM(ggAUC$X1); ggAUC$X2=numSM(ggAUC$X2)

pgg=ggplot(ggAUC, aes(x=X1, y=X2, color=X3, group=X3))
pgg=pgg+geom_line(aes(linetype=X3),lwd=1,color='black')
pgg=pgg+scale_x_continuous(breaks=seq(0,15,3),limits=c(0,16))
pgg=pgg+xlab('Time (years)')+ylab('Time-dependent AUC')
pgg=pgg+theme(legend.position='top', legend.title=element_blank(),
      axis.ticks=element_blank(), panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(), panel.border = element_blank(),
      axis.line = element_line(color = 'black'),
      axis.title.y=element_text(vjust=1))
pgg
# setwd(pathTex)
# tikz(file='cumulAUC.tex', height=4, width=7, standAlone=F)
# pgg
# dev.off()
###############################################################

###############################################################
# Concordance index
conc1 = concordance.index(x = predM1, surv.time = m1Data[, "slength"],
	surv.event = m1Data[, "compliance"], method = "noether", na.rm = TRUE)
conc2 = concordance.index(x = predM2, surv.time = m2Data[, "slength"],
	surv.event = m2Data[, "compliance"], method = "noether", na.rm = TRUE)
concF = concordance.index(x = predMF, surv.time = mFData[, "slength"],
	surv.event = mFData[, "compliance"], method = "noether", na.rm = TRUE)

print( cbind(conc1[1:5], conc2[1:5], concF[1:5] ) ) 

# ROC plots with survcomp
sTime = 10

perfMod1 <- tdrocc(x = predM1, surv.time = m1Data[, "slength"],
	surv.event = m1Data[, "compliance"], time = sTime, na.rm = TRUE)
perfMod2 <- tdrocc(x = predM2, surv.time = m2Data[, "slength"],
	surv.event = m2Data[, "compliance"], time = sTime, na.rm = TRUE)
perfFinal <- tdrocc(x = predMF, surv.time = mFData[, "slength"],
	surv.event = mFData[, "compliance"], time = sTime, na.rm = TRUE)

print(perfMod1$AUC); print(perfMod2$AUC); print(perfFinal$AUC)

plot(x = 1 - perfFinal$spec, y = perfFinal$sens,
	xlab = "False Positive Rate", ylab = "True Positive Rate", 
	xlim = c(0,1), ylim = c(0, 1), type = "l", las=1,
	main = paste0("Time-dependent ROC\ncurve at ", sTime, " years"))
lines(x = 1 - perfMod1$spec, y = perfMod1$sens, col='darkblue')
lines(x = 1 - perfMod2$spec, y = perfMod2$sens, col='darkgreen')
lines(x = c(0, 1), y = c(0, 1), lty = 3, col = "red")
###############################################################

###############################################################
AUCs=matrix(NA,ncol=3,nrow=length(times))
for(ii in 1:length(times)){
	auctM1=survivalROC(Stime=m1Data[,'slength'], status=m1Data[,'compliance'],
		marker=predM1, predict.time=times[ii], method='KM')$AUC
	auctM2=survivalROC(Stime=m2Data[,'slength'], status=m2Data[,'compliance'],
		marker=predM2, predict.time=times[ii], method='KM')$AUC
	auctMF=survivalROC(Stime=mFData[,'slength'], status=mFData[,'compliance'],
		marker=predMF, predict.time=times[ii], method='KM')$AUC
	AUCs[ii,]=c(auctM1, auctM2, auctMF)
	print(paste0('AUC for ', times[ii], ' calculated...'))
}

plot(times, AUCs[,1], col='darkblue', type='l', ylim=c(0.5, 0.8))
lines(times, AUCs[,2], col='darkgreen')
lines(times, AUCs[,3])
###############################################################