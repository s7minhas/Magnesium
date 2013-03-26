# Need to pull out sanction variable for use in analysis
# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Magnesium/R";
	pathGraphics="~/Dropbox/Research/Magnesium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathData="~/Dropbox/Research/Magnesium/Data"}

# Load sanction network Data
setwd(pathData)
load('sanction.rda')

# Clean sanctions network data by dropping -99s
remove <- '-99'
sanctionDyadData <- lapply(sanctionDyadData, 
	function(x) FUN=x[!rownames(x) %in% remove, !colnames(x) %in% remove])

###################################################
# Loading libraries and functions
library(bipartite)
library(colorspace)
library(ergm)
library(ggplot2)
library(igraph)
library(NetIndices)
library(network)
library(sna)
library(tnet)
library(grid)
# layout for igraph plot
layout.svd3 <- function (graph, d = shortest.paths(graph), ...)
{
  if (!is.igraph(graph)) {
    stop("Not a graph object")
  }
  l <- svd(d, 3)$u
  l[, 1] <- l[, 1]/dist(range(l[, 1]))
  l[, 2] <- l[, 2]/dist(range(l[, 2]))
  l[, 3] <- l[, 3]/dist(range(l[, 3]))
  l
}
###################################################

###################################################
# Plotting networks
setwd(pathGraphics)
years <- seq(1971, 2000, 1)
pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
par(mfrow=c(6,5),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(sanctionDyadData)){
	sanctions<- sanctionDyadData[[ii]]
	# Not accounting for sender prim
	sanctions <- sanctions/sanctions
	sanctions[is.na(sanctions)] <- 0
	rows<-rowSums(sanctions)==0
	cols<-colSums(sanctions)==0
	both<-rows*cols
	bothsub<-both[both==0]
	sanctions <- sanctions[match(names(bothsub),rownames(sanctions)),match(names(bothsub),colnames(sanctions))]

	sanction.grW <- graph.adjacency(sanctions, mode='directed', weighted=T, diag=F)

	# layout.kamada.kawai
	# layout.fruchterman.reingold
	plot(sanction.grW, layout=layout.fruchterman.reingold, main=years[ii],vertex.size=4,
	          vertex.label=V(sanction.grW)$name, vertex.label.dist=0.5,
	          vertex.color="gray", vertex.label.color="black", 
	          edge.arrow.size=0.1, edge.color='deepskyblue3',
	          edge.width=E(sanction.grW)$weight/10,
	          vertex.label.cex=.74,edge.curved=T, vertex.label.dist=0.5)
}
dev.off()
###################################################