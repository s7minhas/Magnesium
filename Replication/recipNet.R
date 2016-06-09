source('Setup.R')

###################################################
# Load SRM network data
load(paste0(pathData, 'compSRM_all.rda'))
###################################################

###################################################
# Add abbreviation column to panel dataset
panel$abb = countrycode(panel$cname, 'country.name', 'iso3c')
panel$abb[panel$cname=='German Democratic Republic'] = 'GDR'
panel$abb[panel$cname=='Zanzibar'] = 'ZNZ'
panel$abb[panel$cname=='Kosovo'] = 'KOS'

yrs = char(c(1972, 1992, 2012))
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
	fname = paste0(pathGraphics, 'compNet_',yr,'.pdf')
	pdf(file=fname, width=6, height=6)
	par(mar=c(1,1,1,1), mgp=c(1.5,.5,0))		
	plot.igraph(g, 
		layout=layout.circle,	
		vertex.color='white', 
		vertex.label.cex=(sqrt(V(g)$weight) + 18)/17,
		vertex.label.color='black',
		vertex.size=sqrt(V(g)$weight) + 18,
		edge.width=E(g)$weight,
		edge.arrow.size=.7,
		edge.curved = curves,
		asp=FALSE
		)
	dev.off()
	system(paste0('pdfcrop ', fname, ' ', fname))
}
###################################################s