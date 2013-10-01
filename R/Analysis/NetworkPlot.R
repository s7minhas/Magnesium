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
years = seq(1960, 2005, 1)
ii=1
# pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
# par(mfrow=c(6,5),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(smatList)){
	smat=smatList[[ii]]
	ctrs=panel$CNTRY_NAME[match(rownames(smat),panel$ccode)]
	rownames(smat)=ctrs; colnames(smat)=ctrs
	# Not accounting for sender prim
	rows=rowSums(smat)==0
	cols=colSums(smat)==0
	both=rows*cols
	bothsub=both[both==0]
	smat = smat[match(names(bothsub),rownames(smat)),
		match(names(bothsub),colnames(smat))]

	sanction.grW = graph.adjacency(smat, mode='directed', weighted=T, diag=F)

	# layout.kamada.kawai
	# layout.fruchterman.reingold
	plot(sanction.grW, layout=layout.fruchterman.reingold, main=years[ii],vertex.size=4,
	          vertex.label=V(sanction.grW)$name, vertex.label.dist=0.5,
	          vertex.color="gray", vertex.label.color="black", 
	          edge.arrow.size=0.3, edge.color='deepskyblue3',
	          edge.width=E(sanction.grW)$weight/5,
	          vertex.label.cex=.74,edge.curved=T, vertex.label.dist=0.5)


}
# dev.off()
###################################################