# Clearing workspace
rm(list=ls())
# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathTex="~/Desktop/Research/Magnesium/LaTeX/TeXoutput"
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data";
	pathPData="~/Desktop/Research/Magnesium/R/Data/BuildingPanelData"}

if(Sys.info()["user"]=="cassydorff")
{pathMain="~/ProjectsGit/Magnesium/R";
	pathTex="/ProjectsGit/Magnesium/LaTeX/TeXoutput"
	pathGraphics="~/Dropbox/Research/Magnesium";
	pathData="~/Dropbox/Research/Magnesium/Data";
	pathPData="~/ProjectsGit/Magnesium/R/Data/BuildingPanelData"}

# R default base params
# par(mar=c(4, 4, 2, 0.5), oma=c(2,2,2,2), mfrow=c(1,1), mgp=c(2,.7,0))

# Loading libraries and functions
# require(igraph)
# # layout for igraph plot
# layout.svd3 = function (graph, d = shortest.paths(graph), ...)
# {
#   if (!is.igraph(graph)) {
#     stop("Not a graph object")
#   }
#   l = svd(d, 3)$u
#   l[, 1] = l[, 1]/dist(range(l[, 1]))
#   l[, 2] = l[, 2]/dist(range(l[, 2]))
#   l[, 3] = l[, 3]/dist(range(l[, 3]))
#   l
# }
require(network)
require(igraph)
require(ggplot2)
theme_set(theme_bw())
require(RColorBrewer)
require(reshape)
require(doBy)
require(foreign)
require(cshapes)
require(countrycode)
require(CRISP)
require(survival)
require(OIsurv)
require(eha)
require(frailtypack)
require(boot)
require(pROC)
require(sbgcop)
require(xtable)
require(tikzDevice)

# Helper functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Log transformations for vars with negative values
logNeg <- function(z){
	x <- z[!is.na(z)]; y <- x
	y[x>0] <- log(x[x>0]); y[x<0] <- -log(abs(x[x<0])); y[x==0] <- 0
	z[!is.na(z)] <- y; z
}

# Rescaling variables
rescale <- function(x,new_max,new_min){
 xResc <- (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

# turn variables into numeric
numSM <- function(x){ as.numeric(as.character(x)) }

# Convert to cname
cname <- function(x){
	require(countrycode); x <- as.character(x)
	y <- countrycode(x, 'country.name', 'country.name') }

### Fx for Melting/Cleaning WB Data for Merge
cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] <- var
	mdata$year <-  as.numeric(as.character(substring(mdata$variable,2)))
	mdata <- mdata[,c(1,2,5,4)]

	# Remove non-country observations and small islands/territories
	drop <- c('Arab World', 'Caribbean small states', 
		'East Asia & Pacific (all income levels)', 
		'East Asia & Pacific (developing only)', 'Euro area', 
		'Europe & Central Asia (all income levels)', 
		'Europe & Central Asia (developing only)', 
		'European Union', 'Heavily indebted poor countries (HIPC)', 
		'High income', 'High income: nonOECD', 'High income: OECD', 
		'Latin America & Caribbean (all income levels)', 
		'Latin America & Caribbean (developing only)', 
		'Least developed countries: UN classification', 
		'Low & middle income', 'Low income', 'Lower middle income', 
		'Middle East & North Africa (all income levels)', 
		'Middle East & North Africa (developing only)', 'Middle income', 
		'North America', 'Not classified', 'OECD members', 
		'Other small states', 'Pacific island small states', 
		'Small states', 'South Asia', 
		'Sub-Saharan Africa (all income levels)', 
		'Sub-Saharan Africa (developing only)', 'Upper middle income', 
		'World',
		 "American Samoa",            "Aruba",                    
		 "Bermuda",                   "Cayman Islands", "Channel Islands",          
		 "Curacao",                   "Faeroe Islands",           
		 "French Polynesia",          "Greenland",                
		 "Guam",                      "Hong Kong SAR, China",     
		 "Isle of Man",               "Macao SAR, China",         
		 "New Caledonia",             "Northern Mariana Islands", 
		 "Puerto Rico",               "Sint Maarten (Dutch part)",
		 "St. Martin (French part)",  "Turks and Caicos Islands", 
		 "Virgin Islands (U.S.)",     "West Bank and Gaza")
	mdata <- mdata[which(!mdata$Country.Name %in% drop),]

	# Setting standardized countryname for WB data
	mdata$Country.Name <- as.character(mdata$Country.Name)
	mdata$Country.Name[mdata$Country.Name=='Korea, Dem. Rep.'] <- 'North Korea' 
	mdata$Country.Name[mdata$Country.Name=='Korea, Rep.'] <- 'South Korea' 
	mdata$cname <- countrycode(mdata$Country.Name, 'country.name', 'country.name')
	mdata$cnameYear <- paste(mdata$cname, mdata$year, sep='')
	
	# Adding in codes from panel
	mdata$ccode <- panel$ccode[match(mdata$cname,panel$cname)]
	mdata$cyear <- paste(mdata$ccode, mdata$year, sep='')
	mdata }

# The function to use is lagDataSM
# This takes a dataset, a variable country year which is 
# a concatenation of the country identifier and year
# Then a country variable
# The varsTOlag should be inputted as a vector
# And lag is just a numeric specifying how many years to lag the variable
lagTS <- function(x,l){
  cuts <- (length(x)-(l-1)):length(x)
  c(rep(NA,l), x[ -cuts ] )
}

lagDataSM <- function(data, country_year, country, varsTOlag, lag=1)
{
  data[,country_year] = numSM(data[,country_year])
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2, 
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) ) 
    } )
  colnames(lagData) <- paste('lag', lag, '_', varsTOlag, sep='')
  cbind(data, lagData)
}

# Calculate cumulative sum of var
cumulTS <- function(
	data=combData, cntry_var='cname', time_var='year', key='cyear', 
		start=1960, end=2013, variable){
	print(paste('Progress in calculating cumulative sum for ', variable))
	cum_var <- paste('c',variable,sep='')
	temp <- data[data[,time_var]>=start & data[,time_var]<end,c(key,cntry_var,time_var,variable)]
	temp <- cbind(temp, 0)
	names(temp) <- c('cyear', 'cntry', 'year', variable, cum_var)
	
	countries <- unique(data[,cntry_var]); years <- start:end; fullData <- NULL

	for(ii in 1:length(countries)){
		slice <- temp[temp$cntry==countries[ii],]
		years <- min(slice$year):max(slice$year)
			for(jj in 2:length(years)){
				slice[slice$year==years[jj],cum_var] <- 
					slice[slice$year==years[jj],variable] + 
						slice[slice$year==(years[jj]-1),cum_var] }
			fullData <- rbind(fullData, slice) 
			if(ii==1 | ii%%20==0 | ii==length(countries)){
				cat(paste(round(100*ii/length(countries),0),'% ',sep=''))}
		}
	print(' Completed '); fullData[,c(key, variable, cum_var)]
}

# Build adjacency matrices from dyadic data
# Dyad data must identify countries by variables  
# ccode_1 & ccode_2 and the time aspect by a variable called year
# time is a simple vector of years
# panel is a dataset with country codes
DyadBuild <- function(variable, dyadData, time, panel=panel, directed=FALSE){

	countryList <- lapply(time, function(x) FUN=panel[panel$year==x,'ccode'])
	names(countryList) <- time

	Mats <- list()
	for(ii in 1:length(time)){
	  countries <- countryList[[ii]]
	  yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
	  rownames(yearMatrix) <- colnames(yearMatrix) <- countries
	  
	  dyadData <- dyadData[,c('ccode_1','ccode_2','year',variable)]
	  dyadData <- data.matrix(dyadData)
	  data <- matrix(dyadData[which(dyadData[,'year'] %in% time[ii]),], ncol=4, 
	                 dimnames=list(NULL, c('ccode_1','ccode_2','year',variable)))
	  
	  for(jj in 1:nrow(yearMatrix)){
	    slice <- matrix(data[which(data[,'ccode_1'] %in% countries[jj]), c('ccode_2',variable)], ncol=2, 
	                    dimnames=list(NULL, c('ccode_2',variable)))
	    rownames(slice) <- slice[,'ccode_2']
	    x <- intersect(countries, as.vector(slice[,'ccode_2']))
	    slice2 <- matrix(slice[as.character(x),], ncol=2, 
	                     dimnames=list(NULL, c('ccode_2',variable)))
	    rownames(slice2) <- slice2[,'ccode_2']
	    
	    yearMatrix[as.character(countries[jj]), rownames(slice2)] <- slice2[,variable]
	    if(directed==FALSE){yearMatrix[rownames(slice2), as.character(countries[jj])] <- slice2[,variable]}
	  }
	  
	  Mats[[ii]] <- yearMatrix
	  print(time[ii])
	}

	names(Mats) <- time
	Mats
}

# Build dyad dataset from monadic data
# This only works if the units listed in countrylist exactly
# match the units listed in the monad dataset
# Monad data must identify country by a variable called 
# ccode and the time aspect by a variable called by year
# time is a simple vector of years
# countryList is a list containing ccodes for each year
DyadBuild_fMonad <- function(variable, oper,
	monadData, time, countryList){
	monadData <- monadData[,c('ccode','year',variable)]
	monadData <- data.matrix(monadData)
	rownames(monadData) <- monadData[,'ccode']

	undirectMats <- list()

	for(ii in 1:length(time)){
		countries <- countryList[[ii]]
		yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
		rownames(yearMatrix) <- colnames(yearMatrix) <- countries

		data <- monadData[which(monadData[,'year'] %in% time[ii]), ]

		for(jj in 1:nrow(yearMatrix)){
			cntryRating <- data[as.character(countries[jj]),variable]
			others <- data[as.character(countries),variable]
			if(oper=='absdiff'){relates <- abs(cntryRating-others)}
			if(oper=='same'){relates <- as.numeric(cntryRating==others)}
			yearMatrix[jj,] <- relates
		}

		diag(yearMatrix) <- 0
		undirectMats[[ii]] <- yearMatrix
		print(time[ii]) 
	}
		names(undirectMats) <- time
		undirectMats
}

# Create spatially weighted variables
# Requires access to SM created panel dataset
# In the dataset with the variables to be weighted it is 
# necessary to have country identifier given by ccode and
# time identifier given by year
spatialBuild <- function(spatList, varData, years, variable, sp_suffix, invert=FALSE){
	varData <- varData
	spatData <- NULL

	for(i in 1:length(years)){
		spatMat <- spatList[[i]]
		# rownames for matrices
		distNames <- as.numeric(rownames(spatMat))
		ndistNames <- panel$ccode[match(distNames, panel$GWCODE)]
		rownames(spatMat) <- ndistNames; colnames(spatMat) <- ndistNames

		# Invert
		if(invert){spatMat <- 1/spatMat; spatMat[spatMat==Inf] <- 0}

		# Applying row standardized weights
		dmatDenom <- apply(spatMat,1,sum)
		dmatDenom[dmatDenom==0] <- 1
		spatMat_rowst <- spatMat/dmatDenom
		
		# Bringing in fdi dataset
		spat_vars <- c('ccode', variable)
		dataYear <- varData[varData$year==years[i], spat_vars]
		dataYear <- dataYear[which(dataYear$ccode %in% ndistNames),]
		o <- as.character(dataYear$ccode)
		
		spatMat_rowst <- spatMat_rowst[o,o]
		# data rows with NAs that are in distance matrix
		# this is equivalent to just dropping them from teh 
		# spatial variable calculation
		dataYear[is.na(dataYear)] <- 0
		
		for(j in 1:nrow(spatMat_rowst)){
			row_weights <- NULL
			row_weights <- t(t(dataYear[,c(2:ncol(dataYear))]) %*%  spatMat_rowst[j,])
			row_weights2 <- NULL
			row_weights2 <- cbind(row_weights, years[i], dataYear$ccode[j])
			spatData <- rbind(spatData, row_weights2)
		}
	print(years[i])}
	spatData <- data.frame(spatData, row.names=NULL)

	names(spatData) <- c(
		paste(sp_suffix,names(spatData)[1:(length(spat_vars)-1)],sep=''),
		'year','ccode')
	spatData$cyear <- paste(spatData$ccode, spatData$year, sep='') 
	spatData
}

# Melts network data down to use for sanction case analyis
netMelt <- function(meltData, meltID, meltYr, netList, rst=TRUE, netStat=function(x){mean(x)}){
	ndata <- NULL
	for(ii in 1:nrow(meltData)){
		slice <- meltData[ii,]
		sen <- as.character(t(slice[,3:7]))
		sen <- sen[!is.na(sen)]
		tar <- as.character(slice[,meltID])

		if(length(intersect(names(netList),slice[,meltYr]))!=0){
				ddata <- netList[[as.character(slice[,meltYr])]]
		
				if(rst==TRUE){
						# Row standardize
						matDenom <- apply(ddata, 1, sum); matDenom[matDenom==0] <- 1
						ddata <- ddata/matDenom}
		
				if(!is.na(sum(match(c(tar,sen),rownames(ddata))))){
					# Row/Col Rel.	
					ddata <- ddata[tar, sen]
					# Network measure
					ddata <- netStat(ddata) } else {
						ddata <- NA
					}
				} else {
					ddata <- NA
				}

		# Combine
		ndata <- rbind(ndata, ddata)

		# Progress
		if(ii==1 | ii%%1000==0 | ii==nrow(meltData)){
			cat(paste(round(100*ii/nrow(meltData),0),'% ',sep=''))}
	}
	print(' Completed '); ndata
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

# Function for risk ratios
riskRatio=function(sims, model, data, var, seed=6886){
	set.seed(seed)
	coefDist=mvrnorm (sims, coef (model), vcov (model))
	scen=scenBuild(vi=var, vRange=NULL,
		vars=names(model$coefficients), ostat=mean, simData=data)
	preds=coefDist%*%t(scen)

	hr=median(exp(preds[,2])/exp(preds[,1])) # Hazard ratio
	bse=sd (preds[,2] - preds[,1]) 	# Bootstrapped se
	ci=c( mean( exp( log( hr ) - 1.96*bse ) ), mean( exp( log( hr ) + 1.96*bse ) )) # Conf int
	matrix( c(hr, ci), ncol=3, dimnames=list(var,c('mean','lci','uci')) ) # Output
}