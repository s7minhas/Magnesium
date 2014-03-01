source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Load sanction network Data
setwd(pathData)
load('sanctionData.rda')

# Create compliance variable
comp <- c(1,2,5,6,7,10)
sanctionDataFinal$compliance <- 0
sanctionDataFinal$compliance[which(sanctionDataFinal$finaloutcome %in% comp)] <- 1
table(sanctionDataFinal$compliance)/nrow(sanctionDataFinal) # ~59% compliance

# Subset
sendIDs=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate_ccode',sendIDs,
	'startyear','endyear','caseid','compliance')]

# Setting up network frame
setwd(pathPData)
load('panel.rda')
years=seq(1960, 2005, 1)
ctryYr=lapply(years, function(x) FUN=panel[panel$year==x,'ccode'])
sdata$endyear[is.na(sdata$endyear)]=2012

cmatList=list()
for(ii in 1:length(years)){
	slice=sdata[which(years[ii]>=sdata$startyear & years[ii]<=sdata$endyear),] 
	ctrs=ctryYr[[ii]] 
	cmat=matrix(0,nrow=length(ctrs),ncol=length(ctrs),dimnames=list(ctrs,ctrs))

	for(jj in 1:nrow(slice)){
		sndrs=NULL; trgt=NULL
		sndrs=slice[jj,sendIDs]; sndrs=as.character(sndrs[!is.na(sndrs)])
		sndrs=sndrs[ which( sndrs %in% intersect( sndrs,rownames(cmat) ) ) ]
		trgt=slice[jj,'targetstate_ccode']; trgt=as.character(trgt)
		if(length(setdiff(trgt,rownames(cmat)))==0){
				cmat2=matrix(0, nrow=length(ctrs), ncol=length(ctrs), dimnames=list(ctrs, ctrs))
				cmat2[trgt, sndrs]=slice[jj,'compliance']
				cmat=cmat + cmat2 			
			}		
	}
	cmatList[[ii]]=cmat	; print(years[ii])
}
names(cmatList)=years
setwd(pathData)
save(cmatList, file='complianceNet.rda')