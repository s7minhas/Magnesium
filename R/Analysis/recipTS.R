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
if(compliance){ 
	load('compSRM.rda') } else { 
	load('sancSRM.rda'); ueffect=Sueffect }
ueffect = ueffect[-length(ueffect)]
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
# Determine pairs to plot
dat$id = paste(dat$ccodeS, dat$ccodeR, sep='_')
datVar = summaryBy(value ~ id, FUN=sd, data=dat) %>% na.omit()
datVar = datVar[which(datVar$value.sd > 0.5),]
###################################################

###################################################
# Add recip rel
ggData = dat[which(dat$id %in% datVar$id),]
dat$idYr = paste(dat$id, dat$year, sep='_')
ggData$idRev = paste(ggData$ccodeR, ggData$ccodeS, sep='_')
ggData$idRevYr = paste(ggData$ccodeR, ggData$ccodeS, ggData$year, sep='_')
ggData$valueRev = dat$value[match(ggData$idRevYr, dat$idYr)]

# cleanup
ggData = ggData[which(!ggData$id %in% ggData$idRev),]
ggData = melt(ggData[,c('label','year','value','valueRev')], id=c('label','year'))
###################################################

###################################################
# Plot
ggplot(ggData, aes(x=year, y=value, color=variable)) + geom_line() + facet_wrap(~label)
###################################################