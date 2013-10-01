source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Load sanction network Data
setwd(pathData)
load('sanctionData.rda')
sendIDs=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate',sendIDs,'startyear','endyear','caseid')]

# Setting up list of country names in existence for time period of analysis
setwd(pathPData)
load('panel.rda')
years=seq(1960, 2005, 1)
ctryYr=lapply(years, function(x) FUN=panel[panel$year==x,'ccode'])

smatList=list()
for(ii in 1:length(years)){
	slice=sdata[which(years[ii]>=sdata$startyear & years[ii]<=sdata$endyear),]  
	
	ctrs=ctryYr[[ii]]
	smat=matrix(0, nrow=length(ctrs), ncol=length(ctrs), dimnames=list(ctrs, ctrs))

	for(jj in 1:nrow(slice)){
		sndrs=NULL; trgt=NULL
		sndrs=slice[jj,sendIDs]; sndrs=as.character(sndrs[!is.na(sndrs)])
		sndrs=sndrs[ which( sndrs %in% intersect( sndrs,rownames(smat) ) ) ]
		trgt=slice[jj,'targetstate']; trgt=as.character(trgt)
		if(length(setdiff(trgt,rownames(smat)))==0){
				smat2=matrix(0, nrow=length(ctrs), ncol=length(ctrs), dimnames=list(ctrs, ctrs))
				smat2[sndrs, trgt]=1
				smat=smat + smat2 
			}
	}
	smatList[[ii]]=smat
}
setwd(pathData)
save(smatList, file='sanctionNet.rda')