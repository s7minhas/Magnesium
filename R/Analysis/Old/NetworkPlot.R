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
# Plotting networks
setwd(pathGraphics)
years = names(smatList)
set.seed(6886)
pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
par(mfrow=c(3,3),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(smatList)){
	smatAdj=creatAdj(smatList[[ii]])
	plot(smatAdj, layout=layout.fruchterman.reingold, main=years[ii],
	          vertex.label=V(smatAdj)$name, vertex.size=4,
	          vertex.label.dist=0.5, vertex.label.cex=1,
	          vertex.color="white", vertex.label.color="black", 
	          edge.arrow.size=0.3, edge.color='deepskyblue3',
	          edge.width=E(smatAdj)$weight/5,
	          edge.curved=T)
	}
dev.off()
###################################################

#### Plots for preeze
###################################################
# Plot of sanction network for 1984
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
years = names(smatList)
ii=which(years==1984)
smatAdj=creatAdj(smatList[[ii]], mult=5)
setwd(pathGraphics)
pdf(file='84net.pdf',height=10,width=15)
set.seed(6886)
plot(smatAdj, layout=layout.kamada.kawai, 
		  # main=paste(years[ii], 'Sanction Network'),
		  main='',
          vertex.label=V(smatAdj)$name, vertex.size=4,
          vertex.label.dist=0.5, vertex.label.cex=1.5,
          vertex.color="white", vertex.label.color="black", 
          edge.arrow.size=0.7, edge.color=brewer.pal(9,'Blues')[5],
          edge.width=E(smatAdj)$weight,
          edge.curved=T)
dev.off()
###################################################

###################################################
# Plot of all sanctions against south africa
years = names(smatList)
ii=which(years==1984)
safSlice=sdata[which(sdata$targetstate_ccode==560 & sdata$startyear<=years[ii]),]
verts=melt(safSlice[,c('targetstate_ccode',sendIDs)])[,2]
verts=verts[!is.na(verts)]; verts=as.character(unique(verts))
setdiff(verts, rownames(smatList)[[ii]])
verts=intersect(verts, rownames(smatList[[ii]]))
safMat = smatList[[ii]][verts,verts]
safMat[,which(!colnames(safMat) == '560')]=0 # 560 is the code for S. Africa

safMatAdj=creatAdj(safMat, top=FALSE)
safSlice$color=append(
	brewer.pal(9, 'Blues')[c(5,7,9)],
	brewer.pal(9, 'Reds')[c(5,7,9)])
edges=get.edgelist(safMatAdj)
edges[3,1]='United States'
edges[4,1]='Saudi Arabia'
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

setwd(pathGraphics)
pdf(file='sanet.pdf', height=7, width=10)
ii=which(years==1984)
par(mar=c(0,0.5,0,0.5), oma=c(0,0,0,0))
set.seed(6886)
plot(safMatAdj, layout=layout.fruchterman.reingold, 
		  main='',
		  # main=paste('South Africa Sanctions',years[ii]),
          vertex.label=V(safMatAdj)$name, vertex.size=4,
          vertex.label.dist=0.5, vertex.label.cex=1.5,
          vertex.color="white", vertex.label.color="black", 
          edge.arrow.size=0.7, edge.label='',
          edge.color=E(safMatAdj)$color,
          edge.width=E(safMatAdj)$weight,
          edge.curved=T)
dev.off()
###################################################

###################################################
# Plot of S. Africa sanctions case by case
setwd(pathGraphics)
pdf(file='saneti.pdf',height=7,width=10)
par(mar=c(0,1.9,0,2.5), oma=c(0,0,0,0), mfrow=c(2,3))
for(ii in 1:nrow(safSlice)){
	scol=safSlice[ii,'color']
	slice=melt(safSlice[ii,sendIDs])[,2]
	scnts=slice[!is.na(slice)]
	scnts=panel$CNTRY_NAME[match(scnts, panel$ccode)]

	edges=get.edgelist(safMatAdj)
	edges[3,1]='United States'
	edges[4,1]='Saudi Arabia'
	E(safMatAdj)$label=as.numeric(
		apply(edges, 1, 
			function(x){!is.na(match(x[1],scnts))}))

	subMat=subgraph.edges(safMatAdj, E(safMatAdj)[label==1])

	set.seed(6886)
	plot(subMat, layout=layout.fruchterman.reingold, 
		  main='',
		  # main=paste('South Africa Sanctions',years[ii]),
          vertex.label=V(subMat)$name, vertex.size=4,
          vertex.label.dist=0.5, vertex.label.cex=1.5,
          vertex.color="white", vertex.label.color="black", 
          edge.arrow.size=0.7, edge.label='',
          edge.color=scol, edge.curved=T)
}
dev.off(); par(mfrow=c(1,1))
###################################################8