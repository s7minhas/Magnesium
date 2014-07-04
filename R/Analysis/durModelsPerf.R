if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

###############################################################
setwd(pathData)
load('durModels.rda')
###############################################################

###############################################################
# Inputs
idVars=idVars[c(1:10,17:19)]
m1Data=na.omit(modData[,c(idVars,varDef[7:nrow(varDef),1]) ])
m2Data=na.omit(modData[,c(idVars,varDef[3:nrow(varDef),1]) ])
mFData=na.omit(modData[,c(idVars,varDef[,1]) ])
times=seq(1,max(modData$slength),5)

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

aucFinal=AUC.cd(
	Surv(mFData$start, mFData$stop, mFData$compliance), 
	Surv(mFData$start, mFData$stop, mFData$compliance), 
	predict(modelFinal),
	predict(modelFinal),
	times=times
	)

print(aucM1$iauc); print(aucM2$iauc); print(aucFinal$iauc)

plot(aucFinal, col='black')
lines(aucM1$times, aucM1$auc, col='darkgreen')
lines(aucM2$times, aucM2$auc, col='darkblue')
###############################################################

###############################################################
# Concordance index
conc1 = concordance.index(x = predM1, surv.time = m1Data[, "slength"],
	surv.event = m1Data[, "compliance"], method = "noether", na.rm = TRUE)
conc2 = concordance.index(x = predM2, surv.time = m2Data[, "slength"],
	surv.event = m2Data[, "compliance"], method = "noether", na.rm = TRUE)
concF = concordance.index(x = predMF, surv.time = mFData[, "slength"],
	surv.event = mFData[, "compliance"], method = "noether", na.rm = TRUE)

print(cbind(conc1[1:5])); print(conc2[1:5]); print(concF[1:5])

# ROC plots with survcomp
sTime = mean(unique(modData$slength))

perfMod1 <- tdrocc(x = predM1, surv.time = m1Data[, "slength"],
	surv.event = m1Data[, "compliance"], time = sTime, na.rm = TRUE)
perfMod2 <- tdrocc(x = predM2, surv.time = m2Data[, "slength"],
	surv.event = m2Data[, "compliance"], time = sTime, na.rm = TRUE)
perfFinal <- tdrocc(x = predMF, surv.time = mFData[, "slength"],
	surv.event = mFData[, "compliance"], time = sTime, na.rm = TRUE)

print(perfMod1$AUC); print(perfMod2$AUC); print(perfFinal$AUC)

plot(x = 1 - perfFinal$spec, y = perfFinal$sens, type = "l",
	xlab = "1 - specificity", ylab = "sensitivity", xlim = c(0,1),
	 ylim = c(0, 1), main = "Time-dependent ROC curve\nat 24 years")
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