##################################################
# Purpose: create a recip network plot
# CD & SM

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R');
source('~/Research/Magnesium/R/Data/SRM.R');
load('~/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R');
load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda');
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/SRM.R')
}
###################################################

###################################################
# Load SRM network data

# If compliance = TRUE then compliance recip network plots are generated
## if false then sanction recip network plots
compliance=FALSE

# To print plots set to true
printPlot=TRUE

setwd(pathData)
if(compliance){ 
	load('compSRM.rda') } else { 
	load('sancSRM.rda'); ueffect=Sueffect }
###################################################

###################################################
# Add abbreviation column to panel dataset
panel$abb = countrycode(panel$cname, 'country.name', 'iso3c')
panel$abb[panel$cname=='German Democratic Republic'] = 'GDR'
panel$abb[panel$cname=='Zanzibar'] = 'ZNZ'
panel$abb[panel$cname=='Kosovo'] = 'KOS'

yrs = as.character(seq(1962, 2012, 10))
ecols = brewer.pal(3, 'RdBu')[c(1,3)]
for(yr in yrs){

	# Choose network to plot
	adj = ueffect[[yr]]

	# Set node names
	rownames(adj) = colnames(adj) = panel$abb[match(rownames(adj), panel$ccode)]

	# Pick most active countries
	topS=names(sort(rowSums(abs(adj)), decreasing=TRUE)[1:5])
	topR=names(sort(colSums(abs(adj)), decreasing=TRUE)[1:5])
	top = unique(c(topS, topR))
	adjSub = adj[top, top]

	# Create igraph object
	g = graph.adjacency(adjSub, mode='directed', diag=FALSE, weighted=TRUE)

	# Define extra attributes
	E(g)$color = ifelse(E(g)$weight<0, ecols[1], ecols[2])
	V(g)$weight = colSums(abs(adjSub))
	E(g)$weight = log(abs(E(g)$weight)+1)

	# Plot
	set.seed(6886)
	curves = autocurve.edges2(g)
	setwd(pathGraphics)
	if(printPlot){
		if(compliance){ pdf(file=paste0('compNet_',yr,'.pdf'), width=6, height=6) }
		if(!compliance){ pdf(file=paste0('sancNet_',yr,'.pdf'), width=6, height=6) } }
	par(mar=c(1,1,1,1), mgp=c(1.5,.5,0))		
	plot.igraph(g, 
		layout=layout.circle,	
		vertex.color='white', 
		vertex.label.cex=(sqrt(V(g)$weight) + 18)/17,
		vertex.label.color='black',
		vertex.size=sqrt(V(g)$weight) + 18,
		edge.width=E(g)$weight,
		edge.arrow.size=.5,
		edge.curved = curves,
		asp=FALSE
		)
	if(printPlot){ dev.off() }
}
###################################################s