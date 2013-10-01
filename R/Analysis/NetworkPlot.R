source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Load sanction network Data
setwd(pathData)
load('sanctionData.rda')
sendIDs=paste('sender',1:5,'_ccode',sep='')
sdata=sanctionDataFinal[,c('targetstate',sendIDs,'startyear','endyear','caseid')]
load('sanctionNet.rda')
setwd(pathPData)
load('panel.rda')
###################################################

###################################################
# Plotting networks
setwd(pathGraphics)
years = names(smatList)
pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
par(mfrow=c(6,5),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(smatList)){
	smat=smatList[[ii]]
	ctrs=panel$CNTRY_NAME[match(rownames(smat),panel$ccode)]
	rownames(smat)=ctrs; colnames(smat)=ctrs
	# Dropping cases with no send/rec
	rows=rowSums(smat)==0
	cols=colSums(smat)==0
	both=rows*cols
	bothsub=both[both==0]
	smat=smat[match(names(bothsub),rownames(smat)),
		match(names(bothsub),colnames(smat))]
	# Choosing labels
	topS=names(rowSums(smat)[rowSums(smat)>mean(rowSums(smat))])
	topC=names(colSums(smat)[colSums(smat)>mean(colSums(smat))])

	smatAdj=graph.adjacency(smat, mode='directed', weighted=T, diag=F)
	V(smatAdj)[degree(smatAdj)<=mean(degree(smatAdj))]$name=''

	# layout.kamada.kawai
	# layout.fruchterman.reingold
	plot(smatAdj, layout=layout.fruchterman.reingold, main=years[ii],
	          vertex.label=V(smatAdj)$name, vertex.size=4,
	          vertex.label.dist=0.5, vertex.label.cex=.74,
	          vertex.color="gray", vertex.label.color="black", 
	          edge.arrow.size=0.3, edge.color='deepskyblue3',
	          edge.width=E(smatAdj)$weight/5,
	          edge.curved=T)
	}
dev.off()
###################################################