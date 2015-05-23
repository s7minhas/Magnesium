##################################################
# Purpose: create a recip network plot
# CD & SM

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
source('~/Research/Magnesium/R/Setup.R');
source('~/Research/Magnesium/R/Analysis/SRM.R');
load('~/Research/Magnesium/R/Data/BuildingPanelData/panel.rda')
}

if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R');
load('/Users/cassydorff/ProjectsGit/Magnesium/R/Data/BuildingPanelData/panel.rda');
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Analysis/SRM.R')
}
###################################################

###################################################
# Load SRM network data
setwd(pathData)
load('compSRM.rda')
###################################################

###################################################
# Add abbreviation column to panel dataset
panel$abb = countrycode(panel$cname, 'country.name', 'iso3c')
panel$abb[panel$cname=='German Democratic Republic'] = 'GDR'
panel$abb[panel$cname=='Zanzibar'] = 'ZNZ'
panel$abb[panel$cname=='Kosovo'] = 'KOS'

yrs = as.character(seq(1962, 2012, 10))
for(yr in yrs){

	# Choose comp network to plot
	comp = ueffect[[yr]]

	# Set node names
	rownames(comp) = colnames(comp) = panel$abb[match(rownames(comp), panel$ccode)]

	# Pick most active countries
	topS=names(sort(rowSums(abs(comp)), decreasing=TRUE)[1:5])
	topR=names(sort(colSums(abs(comp)), decreasing=TRUE)[1:5])
	top = unique(c(topS, topR))
	compSub = comp[top, top]

	# Create igraph object
	g = graph.adjacency(compSub, mode='directed', diag=FALSE, weighted=TRUE)

	# Define extra attributes
	E(g)$color = ifelse(E(g)$weight<0, "tomato", "lightblue")
	V(g)$weight = colSums(abs(compSub))

	# Plot
	set.seed(6886)
	curves = autocurve.edges2(g)
	setwd(pathGraphics)
	pdf(file=paste0('compNet_',yr,'.pdf'), width=6, height=6)
	par(mar=c(1,1,1,1), mgp=c(1.5,.5,0))		
	plot.igraph(g, 
		layout=layout.circle,	
		vertex.color='white', 
		vertex.label.cex=(sqrt(V(g)$weight) + 5)/19,
		vertex.size=sqrt(V(g)$weight) + 5,
		edge.arrow.size=0.5,
		edge.width=abs(E(g)$weight)/3,
		edge.curved = curves,
		asp=FALSE
		)
	dev.off()
}
###################################################s