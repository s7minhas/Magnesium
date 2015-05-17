##################################################
# Purpose: create a list of matrices to measure reciprocity
# CD & SM
# Load data, create matrices, run SRM, pull out useful measures, plot.
###################################################

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R');
source('~/Research/Magnesium/R/Analysis/SRM.R');
load('~/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R');
load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda');
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Analysis/SRM.R')
}

############################################################
# Load sanction & compliance network Data
setwd(pathData)
load('sanctionNet.rda') #smatList, #csmatlist (cumulative)
load('complianceNet.rda') #cmatList, #ccmatlist (cumulative)
############################################################

############################################################
# Reciprocal compliance cases
events = lapply(cmatList[-length(cmatList)], function(tmp){
	rows=rownames(tmp)[which(tmp == 1, arr.ind=TRUE)[,1]]
	cols=colnames(tmp)[which(tmp == 1, arr.ind=TRUE)[,2]]
	ij = paste(rows, cols, sep='_')
	ji = paste(cols, rows, sep='_')
	return(list(ij=ij, ji=ji))
} ) 

compEvents = lapply(1:(length(events)-1), function(t){
	ji = events[[t]]$ji
	tPost = unlist(lapply(events[(t + 1 ):length(events)], function(x) x$ij))
	comp = intersect(ji, tPost)
	info = tPost[which(tPost %in% comp)]
	names(info) = substr(names(info), 1, 4)
	return( list(comp=comp, info=info) )
})
names(compEvents) = names(events)[1:(length(events)-1)]

# All events
aEvents = cbind(table(unlist( lapply(compEvents, function(x) x[[1]]) )))
aEvents = cbind(aEvents[order(aEvents[,1], decreasing=TRUE),])
i = unlist(lapply(strsplit(rownames(aEvents), '_'), function(x){ x[1] }))
j = unlist(lapply(strsplit(rownames(aEvents), '_'), function(x){ x[2] }))
aEvents = data.frame(aEvents, 
	i=panel$cname[match(i, panel$ccode)], j=panel$cname[match(j, panel$ccode)])

# Unique reciprocity events
ucompEvents = unique( unlist( lapply(compEvents, function(x) x[[1]]) ) )
tmp = strsplit(ucompEvents, '_')
result=lapply(tmp, function(event){
	i=panel$cname[match(event[1], panel$ccode)]
	j=panel$cname[match(event[2], panel$ccode)]
	if(!is.na(i) & !is.na(j)){
		return( paste0(i, ' ---> ', j) ) }
	})
print( do.call('rbind', result) )
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