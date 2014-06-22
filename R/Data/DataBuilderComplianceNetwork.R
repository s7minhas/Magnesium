source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

####################################################################
# Load sanction network Data
setwd(pathData)
load('sanctionData.rda')
####################################################################

####################################################################
# Create compliance variable
comp <- c(1,2,5,6,7,10)
sanctionDataFinal$compliance <- 0
sanctionDataFinal$compliance[which(sanctionDataFinal$finaloutcome %in% comp)] <- 1
table(sanctionDataFinal$compliance)/nrow(sanctionDataFinal) # ~59% compliance

# Subset
sendIDs=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate_ccode',sendIDs,
	'startyear','endyear','caseid','compliance')]
####################################################################

####################################################################
# Setting up network frame
setwd(pathPData)
load('panel.rda')
# years=seq(1960, 2005, 1)
years=seq(1960, 2012, 1) # Compliance data extends to 2012
ctryYr=lapply(years, function(x) FUN=panel[panel$year==x,'ccode'])
sdata$endyear[is.na(sdata$endyear)]=2012

cmatList=list()
for(ii in 1:length(years)){
	slice=sdata[which(years[ii]>=sdata$startyear & years[ii]<=sdata$endyear),] 
	ctrs=ctryYr[[ii]] 
	cmatList[[ii]]=matrix(0,nrow=length(ctrs),ncol=length(ctrs),dimnames=list(ctrs,ctrs))

	for(jj in 1:nrow(slice)){
		sndrs=NULL; trgt=NULL
		sndrs=slice[jj,sendIDs]; sndrs=as.character(sndrs[!is.na(sndrs)])
		sndrs=sndrs[ which( sndrs %in% intersect( sndrs,rownames(cmatList[[ii]]) ) ) ]
		trgt=slice[jj,'targetstate_ccode']; trgt=as.character(trgt)
		if(length(setdiff(trgt,rownames(cmatList[[ii]])))==0){
				cmat2=matrix(0, nrow=length(ctrs), ncol=length(ctrs), dimnames=list(ctrs, ctrs))
				if(years[ii]==slice[jj,'endyear']){cmat2[trgt, sndrs]=slice[jj,'compliance']}
				cmatList[[ii]]=cmatList[[ii]] + cmat2 			
			}		
	}
	print(years[ii])
}

names(cmatList)=years
####################################################################

####################################################################
# Cumulative compliance matrices
ccmatList=list()
for(ii in 1:length(years)){
	slice=sdata[which(years[ii]>=sdata$startyear),] 
	ctrs=ctryYr[[ii]] 
	ccmatList[[ii]]=matrix(0,nrow=length(ctrs),ncol=length(ctrs),dimnames=list(ctrs,ctrs))

	for(jj in 1:nrow(slice)){
		sndrs=NULL; trgt=NULL
		sndrs=slice[jj,sendIDs]; sndrs=as.character(sndrs[!is.na(sndrs)])
		sndrs=sndrs[ which( sndrs %in% intersect( sndrs,rownames(ccmatList[[ii]]) ) ) ]
		trgt=slice[jj,'targetstate_ccode']; trgt=as.character(trgt)
		if(length(setdiff(trgt,rownames(ccmatList[[ii]])))==0){
				cmat2=matrix(0, nrow=length(ctrs), ncol=length(ctrs), dimnames=list(ctrs, ctrs))
				if(years[ii]>=slice[jj,'endyear']){cmat2[trgt, sndrs]=slice[jj,'compliance']}
				ccmatList[[ii]]=ccmatList[[ii]] + cmat2 			
			}		
	}
	print(years[ii])
}

names(ccmatList)=years
####################################################################

####################################################################
# setwd(pathData)
save(cmatList, ccmatList, file='complianceNet.rda')
####################################################################/