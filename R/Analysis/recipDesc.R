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

###################################################