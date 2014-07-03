if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

###############################################################
setwd(pathData)
load('durModels.rda')
###############################################################

###############################################################
# Performance
idVars=idVars[c(1:10,17:19)]
M1Data=na.omit(modData[,c(idVars,varDef[7:nrow(varDef),1]) ])
M2Data=na.omit(modData[,c(idVars,varDef[3:nrow(varDef),1]) ])
MfinData=na.omit(modData[,c(idVars,varDef[,1]) ])

aucM1=AUC.cd(
	Surv(M1Data$start, M1Data$stop, M1Data$compliance), 
	Surv(M1Data$start, M1Data$stop, M1Data$compliance), 
	predict(model1),
	predict(model1),
	times=seq(5,50,5)
	)

aucM2=AUC.cd(
	Surv(M2Data$start, M2Data$stop, M2Data$compliance), 
	Surv(M2Data$start, M2Data$stop, M2Data$compliance), 
	predict(model2),
	predict(model2),
	times=seq(5,50,5)
	)

aucFinal=AUC.cd(
	Surv(MfinData$start, MfinData$stop, MfinData$compliance), 
	Surv(MfinData$start, MfinData$stop, MfinData$compliance), 
	predict(modelFinal),
	predict(modelFinal),
	times=seq(5,50,5)
	)

print(aucM1)
print(aucM2)
print(aucFinal)

plot(aucFinal, col='black')
lines(aucM1$times, aucM1$auc, col='red')
lines(aucM2$times, aucM2$auc, col='blue')

# ROC plots
predts <- predict(model1, type = "risk")
perfMod1 <- tdrocc(x = predts, surv.time = M1Data[, "slength"],
	surv.event = M1Data[, "compliance"], time = 5, na.rm = TRUE)
predts <- predict(model2, type = "risk")
perfMod2 <- tdrocc(x = predts, surv.time = M2Data[, "slength"],
	surv.event = M2Data[, "compliance"], time = 5, na.rm = TRUE)
predts <- predict(modelFinal, type = "risk")
perfFinal <- tdrocc(x = predts, surv.time = MfinData[, "slength"],
	surv.event = MfinData[, "compliance"], time = 5, na.rm = TRUE)

plot(x = 1 - perfFinal$spec, y = perfFinal$sens, type = "l",
	xlab = "1 - specificity", ylab = "sensitivity", xlim = c(0,1),
	 ylim = c(0, 1), main = "Time-dependent ROC curve\nat 5 years")
lines(x = 1 - perfMod1$spec, y = perfMod1$sens, col='green')
lines(x = 1 - perfMod2$spec, y = perfMod2$sens, col='darkgreen')
lines(x = c(0, 1), y = c(0, 1), lty = 3, col = "red")
###############################################################