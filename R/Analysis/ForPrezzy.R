if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

setwd(pathData)
load('durData.rda')
ids=data.frame(cbind(unique(aData$targetstate),1:length(unique(aData$targetstate))))
names(ids)=c('targetstate','fcode')
aData=merge(aData,ids,by='targetstate',all.x=T)

# not lagging everything
# incudes: controls + senders & distance hypo + net hypo
# ***pretty interesting results
aData$Creligdata=aData$Creligdata+abs(min(aData$Creligdata,na.rm=T))
cmodel4 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	, data=aData)
summary(cmodel4)
temp=na.omit(aData[,c('caseid',names(cmodel4$coefficients))])
length(unique(temp$caseid))

aData$interaction=aData$polconiii*aData$noS
cmodel8 = coxph(Surv(start, stop, compliance) ~ 
	noS + polconiii + distdata 
	+ lag1_lgdpCAP + Internal.Conflict + tdata
	+ allydata + igodata + sancRecCnt + Creligdata
	+ interaction
	, data=aData)
summary(cmodel8)

m1Tab=summary(cmodel4)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(m1Tab)=c('Number of senders',
	'Constraints', 'Distance',
	'GDP per Capita (lagged)',
	'Internal Conflcit',
	'Trade', 'Ally',
	'IGO', "Rec'd Sanctions",
	'Religion')
m1Tab=xtable(m1Tab)
setwd(pathGraphics)
# save(m1Tab, file='mod1.tex')

m2Tab=summary(cmodel8)$coefficients[,c('coef','se(coef)','Pr(>|z|)')]
rownames(m2Tab)=c('Number of senders',
	'Constraints', 'Distance', 
	'GDP per Capita (lagged)',
	'Internal Conflcit',
	'Trade', 'Ally',
	'IGO', "Rec'd Sanctions",
	'Religion', 'Senders*Constraints')
m2Tab=xtable(m2Tab)
setwd(pathGraphics)
# save(m2Tab, file='mod2.tex')

###
# Vars to generate survival plots for:
	# noS, distance, ally, igo, religion
simModel=cmodel8
pcolors=append(brewer.pal(9,'Reds')[8],brewer.pal(9,'Blues')[8])
vrfn=function(x){c(min(x,na.rm=T),max(x,na.rm=T))}

setwd(pathGraphics)
pdf(file='nosSurv.pdf', height=4, width=6)
plot(survfit(simModel, 
	scenBuild(vi='noS', vRange=c(1,2),
	vars=names(simModel$coefficients), 
	ostat=mean, simData=aData) ),
	conf.int=F, col=pcolors, las=1,
	# main='Number of Senders', 
	main='', 
	ylim=c(0.4,1), xlim=c(0,30), 
	ylab='Survival Probability', xlab='Time (Years)', bty='n')
legend('topright', c("Few Senders", "Many Senders"), 
	lty = 1, col=pcolors, bty='n')
dev.off()

pdf(file='oNet.pdf', height=7, width=10)
coefs=c('distdata','allydata','igodata','Creligdata')
cnames=c('Distance','Ally', 'IGO', 'Religion')
par(mfrow=c(2,2))
for(ii in 1:length(coefs)){
	coef=coefs[ii]
	if (coef=='distdata') { crange=c(0.001,0.005)
		} else { crange=vrfn(aData[,coef]) }	
	plot(survfit(simModel, 
		scenBuild(vi=coef, vRange=crange,
		vars=names(simModel$coefficients), 
		ostat=mean, simData=aData) ),
	conf.int=F, col=pcolors, las=1,
	main=cnames[ii], ylim=c(0.4,1), xlim=c(0,30))
	if(ii%%2){title(ylab='Survival Prob.')} 
	if(ii==3|ii==4){title(xlab='Time (Years)') } }
dev.off(); par(mfrow=c(1,1))