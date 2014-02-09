
# Load sanction network Data
setwd(pathData)
load('sanctionData.rda')
sendIDs=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate_ccode',sendIDs,'startyear','endyear','caseid')]
load('sanctionNet.rda')
setwd(pathPData)
load('panel.rda')
###################################################

###################################################
# Helper function to create adjacency matrices
creatAdj = function(mat, top=TRUE, mult=1.5){
	require(igraph)
	setwd(pathPData)
	load('panel.rda')
	ctrs=panel$CNTRY_NAME[match(rownames(mat),panel$ccode)]
	rownames(mat)=ctrs; colnames(mat)=ctrs
	# Dropping cases with no send/rec
	rows=rowSums(mat)==0
	cols=colSums(mat)==0
	both=rows*cols
	bothsub=both[both==0]
	mat=mat[match(names(bothsub),rownames(mat)),
		match(names(bothsub),colnames(mat))]

	matAdj=graph.adjacency(mat, mode='directed', weighted=T, diag=F)
	if(top==TRUE){V(matAdj)[degree(matAdj)<=mult*median(degree(matAdj))]$name=''}
	V(matAdj)$name[V(matAdj)$name=='Germany Federal Republic']='W. Germany'
	V(matAdj)$name[V(matAdj)$name=='Germany Democratic Republic']='E. Germany'
	V(matAdj)$name[V(matAdj)$name=='United Kingdom']='UK'
	V(matAdj)$name[V(matAdj)$name=='United States']='USA'
	V(matAdj)$name[V(matAdj)$name=='Saudi Arabia']='S. Arab.'
	matAdj
}
###################################################

###################################################
#srm
years = names(smatList)
source("SRM.R")
out<-dyads(smatList)

