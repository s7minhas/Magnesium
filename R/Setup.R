# Clearing workspace
rm(list=ls())
# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data";
	pathPData="~/Desktop/Research/Magnesium/R/Data/BuildingPanelData"}

if(Sys.info()["user"]=="cassydorff")
{pathMain="~/ProjectsGit/Magnesium/R";
	pathGraphics="~/Dropbox/My Research/Magnesium";
	pathData="~/Dropbox/My Research/Magnesium/Data";
	pathPData="~/ProjectsGit/Magnesium/R/Data/BuildingPanelData"}

# Loading libraries and functions
require(ggplot2)
theme_set(theme_bw())
require(reshape)
require(doBy)
require(foreign)
require(cshapes)
require(countrycode)
require(CRISP)
require(survival)
require(OIsurv)
require(eha)

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

lagDataSM <- function(data, country_year, country, varsTOlag, lag)
{
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

# Build undirected dyad dataset from dyadic data
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
		if(ii==1 | ii%%100==0 | ii==nrow(meltData)){
			cat(paste(round(100*ii/nrow(meltData),0),'% ',sep=''))}
	}
	print(' Completed '); ndata
}

# Modified from CRISP pacakge to work in this case
buildDuration <- function (data, y, trainingend = NULL, teststart = NULL, dataend = NULL) 
{
    if (is.null(data)) 
        stop("No data supplied")
    if (is.null(teststart)) 
        stop("teststart not defined")
    if (is.null(trainingend)) 
        stop("testend not defined")
    if (is.null(dataend)) 
        stop("dataend (end of data available) is not defined")
    if (!y %in% names(data)) 
        stop("Var. name y not in provided data.")
    durationConvert <- function(data, y, lastdate = NULL) {
        lastdate <- as.Date(lastdate)
        data <- subset(data, date <= lastdate)
        data <- data[order(data$ccode, data$date), ]
        failure <- function(x) return(c(0, pmax(0, diff(x))))
        data$failure <- unlist(by(data[, y], data$ccode, failure))
        data <- subset(data, !(get(y) == 1 & failure == 0))
        data$end.spell <- ifelse(data$date == as.Date(lastdate), 
            1, 0)
        data$end.spell <- ifelse(data$failure == 1, 1, data$end.spell)
        data$spellID <- rev(cumsum(rev(data$end.spell)))
        failedspells <- data$spellID[data$failure == 1]
        helper <- cbind(failedspells, 1)
        colnames(helper) <- c("spellID", "c")
        data <- merge(data, helper, by = ("spellID"), all.x = TRUE)
        data$c[is.na(data$c)] <- 0
        helper <- rep(1, dim(data)[1])
        data <- data[order(data$spellID, data$date), ]
        data$duration <- unlist(by(helper, data$spellID, cumsum))
        data <- data[order(data$ccode, data$date), ]
        return(data)
    }
    training <- durationConvert(data, y, lastdate = trainingend)
    full <- durationConvert(data, y, lastdate = dataend)
    test <- subset(full, date >= as.Date(teststart))
    predData <- subset(test, date == as.Date(dataend))
    missing <- setdiff(unique(data$ccode), unique(predData$ccode))
    endData=data[data$date == as.Date(dataend), ]
    if(length(intersect(unique(endData$ccode), missing))>0){
        missing <- subset(endData, ccode %in% missing)
        missing$failure <- 0
        missing$end.spell <- NA
        missing$spellID <- NA
        missing$c <- 1
        missing$duration <- 1
        predData <- rbind(predData, missing)}
    training$t.0 <- training$duration - 1
    test$t.0 <- test$duration - 1
    predData$t.0 <- predData$duration - 1
    output <- list(full=full, training=training, test=test, predData=predData)
    return(output)
}