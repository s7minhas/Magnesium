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
creatAdj = function(mat, top=TRUE, mult=1.5, shortName=TRUE){
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

	# matAdj=network(mat, directed=T)
	matAdj=graph.adjacency(mat, mode='directed')
	if(top==TRUE){V(matAdj)[degree(matAdj)<=mult*median(degree(matAdj))]$name=''}
	if(shortName==TRUE){
		V(matAdj)$name[V(matAdj)$name=='Germany Federal Republic']='W. Germany'
		V(matAdj)$name[V(matAdj)$name=='Germany Democratic Republic']='E. Germany'
		V(matAdj)$name[V(matAdj)$name=='United Kingdom']='UK'
		V(matAdj)$name[V(matAdj)$name=='United States']='USA'
		V(matAdj)$name[V(matAdj)$name=='Saudi Arabia']='S. Arabia' }
	matAdj
}
###################################################

# ###################################################
# # Plotting networks
# setwd(pathGraphics)
# years = names(smatList)
# set.seed(6886)

# # pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
# par(mfrow=c(2,2),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))

# for(ii in which( names(smatList) %in% seq(1970,2000,10) ) ){
# 	smatAdj=creatAdj(smatList[[ii]], mult=6)
# 	V(smatAdj)$name=""
# 	plot(smatAdj, layout=layout.fruchterman.reingold, main=years[ii],
#           # vertex.label=V(smatAdj)$name,
#           vertex.size=2,
#           # vertex.label.dist=0.5, vertex.label.cex=1, vertex.label.color="black", 
#           vertex.color="white", 
#           edge.arrow.size=0.5, edge.color=brewer.pal(8,'Blues')[4],
#           edge.width=E(smatAdj)$weight,
#           edge.curved=F)
# 	}
# # dev.off()
# ###################################################

#### Plots for preeze
###################################################
# Plot of sanction network for 1984
map84=cshp(date=as.Date('1984-12-12'))
coords=coordinates(map84)
dst=as.matrix(dist(coords, upper = TRUE, diag=TRUE))
xy=cmdscale(dst, k=3)
r=rank(xy[,1])/dim(xy)[1]
g=rank(xy[,2])/dim(xy)[1]
b=rank(xy[,3])/dim(xy)[1]
farben=rgb(g,r,0)
map84$mapcolors=farben
# create color-cntry frame
netColors=data.frame(cbind(as.character(map84$CNTRY_NAME), map84$mapcolors))
netColors$X1=as.character(netColors$X1);netColors$X2=as.character(netColors$X2)
netColors$X1[netColors$X1=='Congo, DRC'] = "Congo, Democratic Republic of"
netColors$ccode=panel$ccode[match(netColors$X1, panel$CNTRY_NAME)]

## Plot map with colors
setwd(pathGraphics)
pdf('MapLegend.pdf', width = 5, height = 3)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(map84, col=farben, lwd=1e-200)
dev.off()

# Plot network
par(mar=c(0,0,0,0), oma=c(0,0,0,0), mfrow=c(1,1))
years = names(smatList)
ii=which(years==1984)
smatAdj=creatAdj(smatList[[ii]],mult=0,shortName=F)
nodes=V(smatAdj)$name
ccodes=panel$ccode[match(nodes, panel$CNTRY_NAME)]
matColors=netColors[ which(netColors$ccode %in% ccodes), 2]

# Shorten and Remove some labels for aesthetics
V(smatAdj)$name[V(smatAdj)$name=='Germany Federal Republic']='W. Germany'
V(smatAdj)$name[V(smatAdj)$name=='United Kingdom']='UK'
V(smatAdj)$name[V(smatAdj)$name=='United States']='USA'
V(smatAdj)$name[V(smatAdj)$name=='Saudi Arabia']='S. Arabia'
V(smatAdj)$name[V(smatAdj)$name=='South Africa']='S. Africa'
keep=names(degree(smatAdj)[degree(smatAdj)>quantile(degree(smatAdj),0.8)])
V(smatAdj)$name[ which( ! V(smatAdj)$name %in% keep) ]=""

setwd(pathGraphics)
pdf(file='84net.pdf',height=10,width=15)
set.seed(12345)
plot(smatAdj, 
	# layout=layout.kamada.kawai, 
	layout=layout.fruchterman.reingold,
		  # main=paste(years[ii], 'Sanction Network'),
		  main='',
          vertex.label=V(smatAdj)$name, vertex.size=2,
          vertex.label.dist=0.5, vertex.label.cex=.7,
          vertex.color=matColors,
           vertex.label.color="black", 
          edge.arrow.size=0.5, edge.color=brewer.pal(8,'Greys')[3],
          edge.width=E(smatAdj)$weight,
          edge.curved=F)
dev.off()

## Write for gephi
# setwd(pathGraphics)
# write.graph(smatAdj, file='84_Sanction_Network.gml', format='gml')
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
plot(safMatAdj, 
		  layout=layout.fruchterman.reingold, 
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