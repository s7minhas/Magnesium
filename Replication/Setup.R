# Clearing workspace
rm(list=ls())
# Setting working directory
pathGraphics="Graphics/"
pathData="Data/";

# Loading libraries and functions
# General functions/libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

toLoad = c('network', 'igraph', 'ggplot2', 'RColorBrewer',
	'reshape', 'doBy', 'foreign', 'cshapes', 'countrycode',
	'survival', 'OIsurv', 'lmtest', 'eha', 'frailtypack', 
	'boot', 'sbgcop', 'survAUC', 'survcomp', 'survivalROC',
	'xtable', 'tikzDevice')
loadPkg(toLoad)

theme_set(theme_bw())

# Helper functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# turn variables into numeric
char = function(x){ as.character(x) }
numSM <- function(x){ as.numeric(char(x)) }

# Convert to cname
cname <- function(x){
	require(countrycode); x <- as.character(x)
	y <- countrycode(x, 'country.name', 'country.name') }

# Curve edges in igraph
# http://stackoverflow.com/questions/16875547/using-igraph-how-to-force-curvature-when-arrows-point-in-opposite-directions
autocurve.edges2 <-function (graph, start = 0.5)
{
    cm <- count.multiple(graph)
    mut <-is.mutual(graph)  #are connections mutual?
    el <- apply(get.edgelist(graph, names = FALSE), 1, paste,
        collapse = ":")
    ord <- order(el)
    res <- numeric(length(ord))
    p <- 1
    while (p <= length(res)) {
        m <- cm[ord[p]]
        mut.obs <-mut[ord[p]] #are the connections mutual for this point?
        idx <- p:(p + m - 1)
        if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
            r <- 0
        }
        else {
            r <- seq(-start, start, length = m)
        }
        res[ord[idx]] <- r
        p <- p + m
    }
    res
}

# Function to create APSR tables for duration object
# modelResults= list object of models
durTable = function(modResults, varDef, digs=3){
	modSumm=lapply(modResults, 
		function(x) FUN=summary(x)$coefficients[,c('coef','se(coef)','Pr(>|z|)')])
	noModels=length(modSumm)

	varsTable=varDef[,1]
	tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)
	tableResults[,1]=rep(varsTable,2)
	colnames(tableResults) = c('Variable',paste('Model',1:noModels))

	for(ii in 2:ncol(tableResults)){
		temp = modSumm[[ii-1]]
		n = modResults[[ii-1]]$n-length(modResults[[ii-1]]$coefficients)
		temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
		estims = temp[1:length(varsTable),'coef']
		estims = round(as.numeric(as.character(estims)),digs)
		pvals = abs(temp[1:length(varsTable),'Pr(>|z|)'])
		pvals = round(as.numeric(as.character(pvals)),digs)
		estims = ifelse(pvals<=0.10 & !is.na(pvals) & pvals>0.05, 
			paste('$', estims,'^{\\ast}$',sep=''), estims)
		estims = ifelse(pvals<0.10 & !is.na(pvals) & pvals<=0.05, 
			paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
		estims = ifelse(is.na(estims),'',estims)
		tableResults[1:length(varsTable),ii] = estims
		serrors = temp[(length(varsTable)+1):nrow(tableResults),'se(coef)']
		serrors = round(as.numeric(as.character(serrors)),digs)
		serrors = paste('(',serrors,')',sep='')
		serrors = ifelse(serrors=='(NA)','',serrors)
		tableResults[(length(varsTable)+1):nrow(tableResults),ii] = serrors
	}

	# Reorganizing rows and variable labels
	tableFinal = NULL
	for(ii in 1:length(varsTable)){
	temp = cbind('', t(tableResults[ii+length(varsTable),2:ncol(tableResults)]))
	tableFinal = rbind(tableFinal, tableResults[ii,], temp) }

	# Adding other info
	sSize = cbind('n', t(as.vector(mapply(x=modResults, 
		function(x) FUN=x$n))))
	events = cbind('Events', t(as.vector(mapply(x=modResults, 
		function(x) FUN=x$nevent))))
	logtest = cbind('Likelihood ratio test', 
		t(as.vector( 
			mapply(x=modResults, function(x) 
				FUN=paste(
					round(summary(x)$logtest[1], digs-1),
					paste0('(',round(summary(x)$logtest[3], digs-1),')')
				 ) 
				)
			) ) )

	tableFinal = rbind(tableFinal, sSize, events, logtest)

	temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
	temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
	tableFinal[,'Variable']=temp
	tableFinal
}

# Function to set up scenarios for prediction
scenBuild=function(vi, vRange, vars, ostat, simData){
	if(is.null(vRange)){ vRange=quantile(simData[,vi], probs=c(0.05,0.95), na.rm=T) }	
	scenCol = length(vars); scenRow = length(vRange)
	scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
	colnames(scenario) = c(vars)
	scenario[,vi] = vRange

	viPos = which(vi==vars)
	ovals = apply(simData[,vars[-viPos]], 2, ostat, na.rm=T)
	scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
	data.frame(scenario)
}

# country id dataset
load( paste0(pathData, 'panel.rda') ) 