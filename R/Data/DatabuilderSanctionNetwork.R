source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Load sanction network Data
setwd(pathData)
load('sanctionData.rda')
senders=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate',senders,'startyear','endyear','caseid')]

# Setting up list of country names in existence for time period of analysis
setwd(pathPData)
load('panel.rda')
years=seq(1960, 2005, 1)
cntryYr=lapply(years, function(x) FUN=panel[panel$year==x,'ccode'])

sanctionDyadData=list()
for(ii in 1:length(years)){
	temp=sdata[years[ii]>=sdata$startyear & years[ii]<=sdata$endyear2,]  
	
	cntry=cntryYr[[ii]]
	temp3=matrix(0, nrow=length(cntry), ncol=length(cntry), dimnames=list(cntry, cntry))
	
	for(jj in 1:ncol(t(temp))){
		senders=as.character(as.vector(t(temp)[3:7,jj]))
		senderPrim=as.character(as.vector(t(temp)[8,jj]))
		target=as.character(as.vector(t(temp)[9,jj]))

		temp2=matrix(0, nrow=length(cntry), ncol=length(cntry), dimnames=list(cntry, cntry))
		temp2[senders, target]=1
		temp2[senderPrim, target]=1
		temp3=temp3 + temp2
	}

	sanctionDyadData[[ii]]=temp3
}

setwd(pathData)
save(sanctionDyadData, file='sanction.rda')