source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

# Load sanction network Data
setwd(pathData)
load('sanctionNet.rda')
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
years <- seq(1960, 2005, 1)
ii=1
# pdf(file='SanctionNetworkPlot.pdf', height=12, width=16)
# par(mfrow=c(6,5),mar=c(2, 2, 2, 2)*0.5, mgp=c(0,0,0), oma=c(0,0,0,0))
for(ii in 1:length(smatList)){
	sanctions<- smatList[[ii]]

	# Not accounting for sender prim
	rows<-rowSums(sanctions)==0
	cols<-colSums(sanctions)==0
	both<-rows*cols
	bothsub<-both[both==0]
	sanctions <- sanctions[match(names(bothsub),rownames(sanctions)),
		match(names(bothsub),colnames(sanctions))]

	sanction.grW <- graph.adjacency(sanctions, mode='directed', weighted=T, diag=F)

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

setwd(pathData)
rownames(smatList[[30]])
library(countrycode)
countrycode(rownames(smatList[[30]]), 'cown', 'country.name')