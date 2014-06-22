##################################################
# Purpose: create a list of matrices to measure reciprocity
# CD & SM
# Load data, create matrices, run SRM, pull out useful measures, plot.
###################################################

if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}

############################################################
# Load sanction & compliance network Data
setwd(pathData)
load('sanctionNet.rda') #smatList, #csmatlist (cumulative)
load('complianceNet.rda') #cmatList, #ccmatlist (cumulative)
############################################################

############################################################
# srm
if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Analysis/SRM.R')}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Analysis/SRM.R')}
############################################################

############################################################
# Creating network measures for use in duration model
compNet=lapply(ccmatList, function(x) FUN=dyads(x))

# Pulling out effects
actorEffect=lapply(compNet, function(x) FUN=x$actor.effect.i)
rcvrEffect=lapply(compNet, function(x) FUN=x$partner.effect.i)
ueffect=lapply(compNet, function(x) FUN=x$unique.effect.ij)
colmeans=lapply(compNet, function(x) FUN=x$colmeans)

# Save data
setwd(pathData)
save(actorEffect, rcvrEffect, ueffect, colmeans, file='compSRM.rda')
############################################################

############################################################
# Creating network measures for use in duration model
sancNet=lapply(csmatList, function(x) FUN=dyads(x))

# Pulling out effects
SactorEffect=lapply(sancNet, function(x) FUN=x$actor.effect.i)
SrcvrEffect=lapply(sancNet, function(x) FUN=x$partner.effect.i)
Sueffect=lapply(sancNet, function(x) FUN=x$unique.effect.ij)
Scolmeans=lapply(sancNet, function(x) FUN=x$colmeans)

# Save data
setwd(pathData)
save(SactorEffect, SrcvrEffect, Sueffect, Scolmeans, file='sancSRM.rda')
############################################################