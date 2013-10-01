source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

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
# Helper function to create adjacency matrix for plot.igraph
creatAdj = function(mat, top=TRUE){
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
	# Choosing labels
	topS=names(rowSums(mat)[rowSums(mat)>mean(rowSums(mat))])
	topC=names(colSums(mat)[colSums(mat)>mean(colSums(mat))])

	matAdj=graph.adjacency(mat, mode='directed', weighted=T, diag=F)
	if(top==TRUE){V(matAdj)[degree(matAdj)<=median(degree(matAdj))]$name=''}
	V(matAdj)$name[V(matAdj)$name=='Germany Federal Republic']='W. Germany'
	V(matAdj)$name[V(matAdj)$name=='Germany Democratic Republic']='E. Germany'
	matAdj
}
###################################################

###################################################
# Plotting networks
setwd(pathGraphics)
years = names(smatList)
pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
par(mfrow=c(3,3),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(smatList)){
	smatAdj=creatAdj(smatList[[ii]])
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

#### Plots for preeze
###################################################
# Plot of sanction network for 1984
par(mar=c(1,0,2,0)+.1, oma=c(0,0,0,0))
years = names(smatList)
ii=which(years==1984)
smatAdj=creatAdj(smatList[[ii]])
plot(smatAdj, layout=layout.fruchterman.reingold, main=years[ii],
          vertex.label=V(smatAdj)$name, vertex.size=4,
          vertex.label.dist=0.5, vertex.label.cex=.74,
          vertex.color="gray", vertex.label.color="black", 
          edge.arrow.size=0.3, edge.color='deepskyblue3',
          edge.width=E(smatAdj)$weight/5,
          edge.curved=T)

# Plot of sanctions against south africa
safSlice=sdata[which(sdata$targetstate_ccode==560 & sdata$startyear<=1984),]
verts=melt(safSlice[,c('targetstate_ccode',sendIDs)])[,2]
verts=verts[!is.na(verts)]; verts=as.character(unique(verts))
setdiff(verts, rownames(smatList)[[ii]])
verts=intersect(verts, rownames(smatList[[ii]]))
safMat = smatList[[ii]][verts,verts]
safMat[,which(!colnames(safMat) == '560')]=0 # 560 is the code for S. Africa

safMatAdj=creatAdj(safMat, top=FALSE)
safSlice$color=brewer.pal(nrow(safSlice), 'Dark2')
edges=get.edgelist(safMatAdj)
edges[,1]=panel$ccode[match(edges[,1], panel$CNTRY_NAME)]
edges[,2]=panel$ccode[match(edges[,2], panel$CNTRY_NAME)]
E(safMatAdj)$color=rep(NA,nrow(edges))
for(ii in 1:nrow(edges)){
	if(edges[ii,2]==560){
		sii=numSM(edges[ii,1])
		siiP=which(safSlice[,sendIDs]==numSM(sii),arr.ind=T)
		E(safMatAdj)$color[ii]=safSlice[siiP[1,1], 'color']
	} else {
		E(safMatAdj)$color[ii]='black'
	}
}

par(mar=c(1,0.5,2.5,0.5)+.1, oma=c(0,0,0,0))
plot(safMatAdj, layout=layout.fruchterman.reingold, 
		  main=paste('South Africa Sanctions',years[ii]),
          vertex.label=V(safMatAdj)$name, vertex.size=2,
          vertex.label.dist=0.5, vertex.label.cex=.74,
          vertex.color="gray", vertex.label.color="black", 
          edge.arrow.size=0.5, 
          edge.color=E(safMatAdj)$color,
          edge.width=E(safMatAdj)$weight,
          edge.curved=T)
###################################################