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
pdf(file='weightedNetworkPlot.pdf', height=12, width=16)
par(mfrow=c(6,5),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(sanctionDyadData)){
	sanctions<- sanctionDyadData[[ii]]
	# Drop -99s
	sanctions <- sanctions[1:(nrow(sanctions)-1), 1:(ncol(sanctions)-1)]
	rows<-rowSums(sanctions)==0
	cols<-colSums(sanctions)==0
	both<-rows*cols
	bothsub<-both[both==0]
	sanctions <- sanctions[match(names(bothsub),rownames(sanctions)),match(names(bothsub),colnames(sanctions))]

	sanction.grW <- graph.adjacency(sanctions, mode='directed', weighted=T, diag=F)

	plot(sanction.grW, layout=layout.kamada.kawai, main=years[ii],vertex.size=4,
	          vertex.label=V(sanction.grW)$name, vertex.label.dist=0.5,
	          vertex.color="gray", vertex.label.color="black", 
	          edge.arrow.size=0.5, edge.color='deepskyblue3',
	          edge.width=E(sanction.grW)$weight,
	          vertex.label.cex=.74,edge.curved=T, vertex.label.dist=0.5)
}
dev.off()
###################################################