##################################################
# Purpose: create a recip network plot
# CD & SM

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R');
source('~/Research/Magnesium/R/Data/SRM.R');
load('~/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R');
load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda');
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/SRM.R')
}
###################################################

###################################################
# Load SRM network data

# If compliance = TRUE then compliance recip network plots are generated
## if false then sanction recip network plots
compliance=TRUE

# To print plots set to true
printPlot=TRUE

setwd(pathData)
load('compSRM.rda')
load('sancSRM.rda')
ueffect = ueffect[-length(ueffect)]
Sueffect = Sueffect[-length(Sueffect)]
###################################################

###################################################
# melts
compScore = melt(ueffect)
names(compScore) = c('ccodeS','ccodeR','value','year')
compScore$cnameS = panel$cname[match(compScore$ccodeS, panel$ccode)] ; compScore$cnameR = panel$cname[match(compScore$ccodeR, panel$ccode)]
compScore$id = paste(compScore$cnameS, compScore$cnameR, compScore$year, sep='_') ; compScore$idR = paste(compScore$cnameR, compScore$cnameS, compScore$year, sep='_')
sancScore = melt(Sueffect)
names(sancScore) = c('ccodeS','ccodeR','value','year')
sancScore$cnameS = panel$cname[match(sancScore$ccodeS, panel$ccode)] ; sancScore$cnameR = panel$cname[match(sancScore$ccodeR, panel$ccode)]
sancScore$id = paste(sancScore$cnameS, sancScore$cnameR, sancScore$year, sep='_') ; sancScore$idR = paste(sancScore$cnameR, sancScore$cnameS, sancScore$year, sep='_')
###################################################

###################################################
# Turn into dyadic dataframe
dat = do.call('rbind', lapply(ueffect, melt) )
dat$year = rownames(dat) %>% numSM(.) %>% round(.,0)
dat = dat[which(dat$X1 != dat$X2),]
names(dat)[1:2] = c('ccodeS','ccodeR') ; rownames(dat) = NULL

# Add abbreviation column to panel dataset
panel$abb = countrycode(panel$cname, 'country.name', 'iso3c')
panel$abb[panel$cname=='German Democratic Republic'] = 'GDR'
panel$abb[panel$cname=='Zanzibar'] = 'ZNZ'
panel$abb[panel$cname=='KOSOVO'] = 'KOS'

# Add abbreviation labels to dat
dat$abbS = panel$abb[match(dat$ccodeS, panel$ccode)]
dat$abbR = panel$abb[match(dat$ccodeR, panel$ccode)]
dat$label = paste(dat$abbS, dat$abbR, sep=' & ')
###################################################

###################################################
quantile(dat$value, probs=seq(0,1,.1))

ggplot(dat, aes(x=value)) + geom_histogram(binwidth=.001)

dat$tmp = log( dat$value + abs(min(dat$value, na.rm=TRUE)) + 1 )
###################################################

###################################################
load('sanctionNet.rda') #smatList, #csmatList (cumulative)
load('complianceNet.rda') #cmatList, #ccmatList (cumulative)

getDyadSumm = function(matList){
	dyadCnt = matList[[length(matList)-1]] %>% melt()
	names(dyadCnt)[1:2] = c('ccodeS', 'ccodeR')
	dyadCnt = dyadCnt[dyadCnt$value!=0,]
	dyadCnt = dyadCnt[order(dyadCnt$value,decreasing=TRUE),]
	dyadCnt$cnameS = panel$cname[match(dyadCnt$ccodeS, panel$ccode)]
	dyadCnt$cnameR = panel$cname[match(dyadCnt$ccodeR, panel$ccode)]
	dyadCnt$id = paste(dyadCnt$cnameS, dyadCnt$cnameR, sep='_')
	dyadCnt$idR = paste(dyadCnt$cnameR, dyadCnt$cnameS, sep='_')
	return(dyadCnt)
}

# Merge together
sancCnt = getDyadSumm(csmatList)
compCnt = getDyadSumm(ccmatList)
sancCnt$compFromReceiver = compCnt$value[match(sancCnt$id, compCnt$idR)]
sancCnt$compFromSender = compCnt$value[match(sancCnt$id, compCnt$id)]

sancCnt[1:10,c('cnameS','cnameR','value','compFromReceiver','compFromSender')]
sancCnt[which(sancCnt$ccodeS==740 & sancCnt$ccodeR==2),c('cnameS','cnameR','value','compFromReceiver','compFromSender')]
###################################################

###################################################
# Melt full sanction and compliance lists
sancData = melt(smatList[1:(length(smatList)-1)])
names(sancData) = c('ccodeS','ccodeR','value','year')
sancData$cnameS = panel$cname[match(sancData$ccodeS, panel$ccode)] ; sancData$cnameR = panel$cname[match(sancData$ccodeR, panel$ccode)]
sancData$id = paste(sancData$cnameS, sancData$cnameR, sancData$year, sep='_') ; sancData$idR = paste(sancData$cnameR, sancData$cnameS, sancData$year, sep='_')
compData = melt(cmatList[1:(length(cmatList)-1)])
names(compData) = c('ccodeS','ccodeR','value','year')
compData$cnameS = panel$cname[match(compData$ccodeS, panel$ccode)] ; compData$cnameR = panel$cname[match(compData$ccodeR, panel$ccode)]
compData$id = paste(compData$cnameS, compData$cnameR, compData$year, sep='_') ; compData$idR = paste(compData$cnameR, compData$cnameS, compData$year, sep='_')

# Merge together
sancData$sancFromReceiver = sancData$value[match(sancData$id, sancData$idR)]
sancData$compFromReceiver = compData$value[match(sancData$id, compData$idR)]
sancData$compFromSender = compData$value[match(sancData$id, compData$id)]
names(sancData)[3] = 'sancFromSender'
###################################################

###################################################
sancSender = 2
sancReceiver = 560 # 740, 666, 560
sendName = panel$cname[match(sancSender, panel$ccode)]
recName = panel$cname[match(sancReceiver, panel$ccode)]

slice = sancData[
	which(sancData$ccodeS == sancSender & sancData$ccodeR == sancReceiver),
	c('cnameS','cnameR','year', 'id', 'idR',
		'sancFromSender','sancFromReceiver','compFromReceiver','compFromSender'
		)
	]
ggData = melt(slice, id=names(slice)[1:5])
ggData$sanc = ifelse(grepl('sanc', ggData$variable)*1==1, 'Sanction Cases', 'Compliance Cases') %>% factor()
ggData$sanc = factor(ggData$sanc, levels=rev(levels(ggData$sanc)))
ggData$sender = ifelse(grepl('Sender', ggData$variable)*1==1, paste0('from ', sendName), paste0('from ', recName))

theme_set(theme_grey())
ggActual=ggplot(ggData, aes(x=year, y=factor(value), color=factor(sender), group=variable, shape=factor(sender), alpha=factor(sender) )) +
	xlab('') + ylab('') +
	geom_point(size=3) +
	facet_wrap(~sanc) + 
	scale_alpha_manual(values=c(.8,.6)) +
	theme(
		axis.text.x=element_text(angle=45,hjust=1,size=5),
		legend.title=element_blank(),
		legend.key=element_blank(),
		legend.position='top',
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)

# Add in recip scores
sancData$compRecipReceiver = compScore$value[match(sancData$id, compScore$idR)]
sancData$compRecipSender = compScore$value[match(sancData$id, compScore$id)]
sancData$sancRecipReceiver = sancScore$value[match(sancData$id, sancScore$idR)]
sancData$sancRecipSender = sancScore$value[match(sancData$id, sancScore$id)]
slice = sancData[
	which(sancData$ccodeS == sancSender & sancData$ccodeR == sancReceiver),
	c('cnameS','cnameR','year', 'id', 'idR',
		'compRecipReceiver', 'compRecipSender', 'sancRecipReceiver', 'sancRecipSender'
		)
	]
ggData = melt(slice, id=names(slice)[1:5])
ggData$sanc = ifelse(grepl('sanc', ggData$variable)*1==1, 'Sanction Reciprocity Score', 'Compliance Reciprocity Score') %>% factor()
ggData$sanc = factor(ggData$sanc, levels=rev(levels(ggData$sanc)))
ggData$sender = ifelse(grepl('Sender', ggData$variable)*1==1, paste0('from ', sendName), paste0('from ', recName))

ggScores=ggplot(ggData, aes(x=year, y=value, color=factor(sender), group=variable, shape=factor(sender), alpha=factor(sender) )) +
	xlab('') + ylab('') +
	geom_line() +
	facet_wrap(~sanc) + 
	scale_alpha_manual(values=c(.8,.6)) +
	theme(
		axis.text.x=element_text(angle=45,hjust=1,size=5),
		legend.title=element_blank(),
		legend.key=element_blank(),
		legend.position='top',
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)

loadPkg('gridExtra')
grid.arrange(ggActual, ggScores, nrow=2)

compCnt[which(compCnt$ccodeR==2 & compCnt$ccodeS==666),]
sancCnt[which(sancCnt$ccodeS==2 & sancCnt$ccodeR==666),]
###################################################