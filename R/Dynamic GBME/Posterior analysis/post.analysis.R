#######################################################
# Replication of Zero Infl GBME
# From Gravity Rainbow paper
# SM
#######################################################

#######################################################
# Clearing workspace
rm(list=ls())
#######################################################

#######################################################
# Setting working directory
if(Sys.info()["user"]=="janus829"
){pathData="~/Desktop/ICEWS/caesium/RCode/Replication/GravityRainbowReplication/Estimation";
  pathFunction="~/Desktop/ICEWS/caesium/RCode/Replication/GravityRainbowReplication/Estimation";
  pathPosterior="~/Desktop/ICEWS/caesium/RCode/Replication/GravityRainbowReplication/Posterior analysis";
  pathResults="~/Dropbox/Research/caesium/GravityRainbowReplication/Results";  
  pathGraphics="~/Dropbox/Research/caesium/GravityRainbowReplication/Graphics"}
#######################################################

setwd(pathResults)
year=1990:2008
# Not sure where Y.hat is located
# load("Y.hat")
load("out")


## Appendix Figure of MCMC scans
setwd(pathResults)
by = read.table("output.y/out/out.1997", header=TRUE)
bm = read.table("output.m/out/out.1997", header=TRUE)

names.y = c(expression(beta[distance]), expression(beta[uv]), expression(beta[economy-sender]), expression(beta[economy-receiver]), expression(sigma[s]^2), expression(sigma[sr]), expression(rho), expression(sigma[gamma]^2))


names.m = c(expression(delta[distance]), expression(delta[ab]), expression(delta[economy-sender]), expression(delta[economy-receiver]), expression(sigma[m]^2), expression(sigma[md]), expression(lambda))

var.m=c("bd1", "bd2", "bs1", "br1", "s2a", "sab", "rho")
var.y=c("bd1", "bd2", "bs1", "br1", "s2a", "sab", "rho", "se2")

library(coda)

plot.mcmc=function(b, x, var, names){
par(mar=c(2,2,3,0), mgp=c(.4,.4,0))
b=b[,var[x]]
plot(1:length(b), b, type="l", col="grey40", xlab="", ylab="", main="", ylim=c(min(b), max(b+sd(b)/2)), lwd=1.5, cex.main=1.8, )
abline(h=mean(b), col="red", lwd=2)
title(names[x], cex.main=2)
text(500, max(b+sd(b)/8), round(heidel.diag(as.mcmc(b))[[3]],2), cex=1.5, col="dark blue")
}

setwd(pathGraphics)
pdf("scansy1997.pdf", height=12, width=4)
par(mfrow=c(8,1), mar=rep(0,4))
sapply(1:8, plot.mcmc, b=by, var=var.y, names=names.y)
dev.off()


pdf("scansm1997.pdf", height=12, width=4)
par(mfrow=c(8,1), mar=rep(0,4))
sapply(1:7, plot.mcmc, b=bm, var=var.m, names=names.m)
dev.off()



# R2 for each year with confidence bounds (Figure 4)
setwd(pathResults)

r2=function(t){
load(paste("output.m/yhat/yhat",year[t], sep="."))
mhat = yhat
load(paste("output.y/yhat/yhat",year[t], sep="."))
ypred = log(pnorm(mhat)*exp(yhat) + 1)
r2 = rep(NA, dim(ypred)[1])
for (i in 1:dim(ypred)[1]){
r2[i] = summary(lm(c(log(out[[t]]$Y + 1)) ~ c(ypred[i,,])))$r.squared
}
quantile(r2, c(.025, .5, .975))
}

# r2 = t(sapply(1:19, r2))
# save(r2, file="r2")
load("r2")

setwd(pathGraphics)
library(ggplot2)
data=data.frame(year=year, r2=r2[,2], rl=r2[,1], ru=r2[,3])


pdf(file="r2.pdf", width=6, height=6)
qplot(x=year, ymin = rl, ymax = ru, data=data, geom = "linerange", 
  ylab=expression(R^2), xlab="", size=I(1.1)) + geom_point(aes(x=year, y=r2), 
  col="black", size=I(3)) + scale_x_continuous(breaks=seq(1990,2008, by=2), 
  labels=seq(1990,2008, by=2)) + opts(axis.title.y = theme_text( size=15))
dev.off()



## Predicted value plots (Figure 5)
setwd(pathResults)
Y.hat = function(t){
load(paste("output.y/yhat/yhat", year[t], sep="."))  
yhat.y=apply(yhat, 2:3, mean)  
load(paste("output.m/yhat/yhat", year[t], sep="."))  
yhat.m=apply(yhat, 2:3, mean)  
ypred = log(pnorm(yhat.m)*exp(yhat.y) + 1)
c(ypred)
}
# Y.hat = sapply(1:19, Y.hat)
#save(as.list = Y.hat, file="Y.hat")
load("Y.hat")

u=13
p1 <- qplot(c(Y.hat[[1]]), c(log(out[[1]]$Y + 1)), ylab="log(y + 1)", xlab=expression(log(hat(y) + 1)), main=year[1], xlim=c(0,u), ylim=c(0,u)) + geom_abline(intercept=0, slope=1, col="blue", size=I(1.1))
p2 <- qplot(c(Y.hat[[19]]), c(log(out[[19]]$Y + 1)), ylab="log(y + 1)", xlab=expression(log(hat(y) + 1)), main=year[19], xlim=c(0,u), ylim=c(0,u)) + geom_abline(intercept=0, slope=1, col="blue", size=I(1.1))
p3 <- qplot(c(Y.hat[[18]]), c(log(out[[19]]$Y + 1)), ylab="log(y + 1)", xlab=expression(log(hat(y) + 1)), main="2008 from 2007", xlim=c(0,u), ylim=c(0,u)) + geom_abline(intercept=0, slope=1, col="blue", size=I(1.1))

setwd(pathPosterior)
source("multiplot.R")
setwd(pathGraphics)
pdf(file="prediction_plots.pdf", width=15, height=5)
multiplot(p1, p2, p3, cols=3)
dev.off()


### RMSE for out-of-sample (Table 2)

for (t in 10:15){
print(sqrt(mean((Y.hat[[t]] - log(out[[t+1]]$Y + 1))^2, na.rm=TRUE)))
}




# Figure 6: Coefficient plots
# Distance effects
setwd(pathResults)
extract <- function(year, type, var) t(apply(sapply(year, function(x) read.table(paste(paste("output", type,sep="."), paste("/out/out",x, sep="."), sep=""), header=TRUE)[,var]), 2, function(x) quantile(x, c(.025, .5, .975))))

b.m =  extract(1990:2008, "m", "bd1")
b.y =  extract(1990:2008, "y", "bd1")

setwd(pathGraphics)
pdf(file="b_distance.pdf", width=5, height=3)
par(mar=c(2,2,.1,.1))
plot(year, b.m[,2], pch=15, ylim=c(min(b.y, b.m), max(b.y, b.m)), xlab="", ylab="", xaxt='n')
axis(1, at=year)
segments(year, b.m[,1], year, b.m[,3], lwd=1.2)
points(year, b.y[,2], pch=16, col="grey40")
segments(year, b.y[,1], year, b.y[,3], lwd=1.2, col="grey40")
text(1993, -5, "Trade volume", col="grey40", cex=1.2)
text(2005, -3.8, "Trade incidence", cex=1.2)
dev.off()

# Latent position effects
setwd(pathResults)
b.m =  extract(1991:2008, "m", "bd2")
b.y =  extract(1991:2008, "y", "bd2")

setwd(pathGraphics)
pdf(file="uv_effect.pdf", width=5, height=3)
par(mar=c(2,2,.1,.1))
plot(1991:2008, b.m[,2], pch=15, ylim=c(min(b.y, b.m), max(b.y, b.m)), xlab="", ylab="", xaxt='n')
axis(1, at=1991:2008)
segments(1991:2008, b.m[,1], 1991:2008, b.m[,3], lwd=1.2)
points(1991:2008, b.y[,2], pch=16, col="grey40")
segments(1991:2008, b.y[,1], 1991:2008, b.y[,3], lwd=1.2, col="grey40")
text(1994, 1.6, "Trade volume", col="grey40", cex=1.2)
text(2005, 1, "Trade incidence", cex=1.2)
dev.off()


## Economy effect for volumes
setwd(pathResults)
b.m =  extract(year, "y", "bs1")
b.y =  extract(year, "y", "br1")

setwd(pathGraphics)
pdf(file="economy_volume.pdf", width=5, height=3)
par(mar=c(2,2,.1,.1))
plot(year, b.m[,2], pch=16, ylim=c(min(b.y, b.m)-.2, max(b.y, b.m)+.2), xlab="", ylab="", xaxt='n')
axis(1, at=year)
segments(year, b.m[,1], year, b.m[,3], lwd=1.5)
points(year+0.1, b.y[,2], pch=16)
segments(year+0.1, b.y[,1], year+0.1, b.y[,3], lwd=1.5)
text(1993, 2.5, "Sender", cex=1.2)
text(2005, 1.6, "Receiver", cex=1.2)
dev.off()


## Reciprocity
setwd(pathResults)
b.m =  extract(year, "m", "rho")
b.y =  extract(year, "y", "rho")

setwd(pathGraphics)
pdf(file="rho.pdf", width=5, height=3)
par(mar=c(2,2,.1,.1))
plot(year, b.y[,2], pch=16, ylim=c(min(b.y)-.05, max(b.y)+.05), xlab="", ylab="", xaxt='n')
axis(1, at=year)
segments(year, b.y[,1], year, b.y[,3], lwd=1.5)
dev.off()



# Figure 7: Exporter random effects
setwd(pathResults)
aa = read.table("output.m/a/a.1990")
plot.ts(aa[,1:10])

plot.ab = function(var, year, type){
setwd(pathResults)
var = apply(read.table(paste(paste("output", type, sep="."),"/", var,"/", paste(var, year, sep="."), sep="")), 2, mean)
x=seq(-4, 4, length=100)
n=length(var)
par(mar=c(3,3,1,0))
plot(sort(var),(1:n)/n, type="l", pch=19,xlab="",ylab="",las=1, bty="n", lwd=2.5, xlim=c(-(max(abs(var))+1), max(abs(var)) + 1), main=year)
abline(h=0,lty=2,col="gray")
abline(h=1,lty=2,col="gray")
r = sd(var,na.rm=T)
polygon(x=c(-1.96*r,-1.96*r,1.96*r,1.96*r),y=c(0,1,1,0),col="gray90",border=NA)
lines(x, pnorm(x,0, sd(var)), col="black", lwd=2)
lines(sort(var),(1:n)/n,col="blue",lwd=2.5)

 t = which(1990:2008 == year)
names(var) = colnames(out[[t]]$Y)
names(var) = countrycode(names(var),"country.name","iso3c")
names(var)[which(colnames(out[[t]]$Y)=="Belgium-Luxembourg")]="BEL-LUX"
low = names(var)[which(var < -1.96*r)]
high = names(var)[which(var > 1.96*r)]

for (i in 1:length(low)){
text(sort(var)[i], i/20, low[i])  
}

for (i in 1:length(high)){
  text(-sort(-var)[i], 1-i/20, high[i])  
}
setwd(pathGraphics)
}

library(countrycode)

setwd(pathGraphics)
pdf(file="randef_y.pdf", width=10, height=5)
par(mfrow=c(1, 2))
plot.ab("a", 1990, type="y")
plot.ab("a", 2008, type="y")
dev.off()

setwd(pathGraphics)
pdf(file="randef_m.pdf", width=10, height=5)
par(mfrow=c(1, 2))
plot.ab("a", 1990, type="m")
plot.ab("a", 2008, type="m")
dev.off()



# Figure 8: random exporter and importer effects for US and CHINA
setwd(pathGraphics)

year = 1990:2008
pdf(file="us_china.pdf", width=10, height=5)
par(mar=c(3,3,1,0.1), mfrow=c(1, 2))
plot(year, rep(0,19), ylim=c(-1,2.2), xaxt='n', lty=2, type="l", main="Exporter random effects")
for (t in 1:19){
china = which(colnames(out[[t]]$Y) == "China, P.R.: Mainland")
us = which(colnames(out[[t]]$Y) == "United States")
setwd(pathResults)
r = apply(read.table(paste("output.y/a/a", year[t], sep="."))[, c(china, us)]  , 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))
points(year[t], r[2, 1], pch=16, col="dark red")
segments(year[t], r[1, 1], year[t], r[3, 1], lwd=2, col="dark red")
points(year[t]+.2, r[2, 2], pch=16)
segments(year[t]+.2, r[1, 2], year[t]+.2, r[3, 2], lwd=2)
}
axis(1, at=year)
text(1997, -.5, "US", cex=1.3)
text(2002, 1.6, "China", cex=1.3, col="dark red")

plot(year, rep(0,19), ylim=c(-1,2.2), xaxt='n', lty=2, type="l", main="Importer random effects")
for (t in 1:19){
  china = which(colnames(out[[t]]$Y) == "China, P.R.: Mainland")
  us = which(colnames(out[[t]]$Y) == "United States")
  setwd(pathResults)
  r = apply(read.table(paste("output.y/b/b", year[t], sep="."))[, c(china, us)]  , 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))
  points(year[t], r[2, 1], pch=16, col="dark red")
  segments(year[t], r[1, 1], year[t], r[3, 1], lwd=2, col="dark red")
  points(year[t]+.2, r[2, 2], pch=16)
  segments(year[t]+.2, r[1, 2], year[t]+.2, r[3, 2], lwd=2)
}
axis(1, at=year)
text(2002, 1.8, "US", cex=1.3)
text(1996, -.7, "China", cex=1.3, col="dark red")
dev.off()

## Figure 9: higher order variance terms

uTv.y.sd = function(t){
setwd(pathResults)
load(paste("output.m/uTv/uTv",year[t], sep="."))
apply(uTv, )  
}

library(reshape)
D1 = sapply(c("s2e1", "s2e2", "s2f1", "s2f2"), function(var) extract(1990:2008, "m", var)[,2])
D1 = data.frame(type="Incidence", melt(D1)[,-1])
D2 = sapply(c("s2e1", "s2e2", "s2f1", "s2f2"), function(var) extract(1990:2008, "y", var)[,2])
D2 = data.frame(type="Volume", melt(D2)[,-1])
data=rbind(D1, D2)

labs = expression(sigma[u[1]]^2, sigma[u[2]]^2, sigma[v[1]]^2, sigma[v[2]]^2)

setwd(pathGraphics)
pdf(file="variances.pdf", width=10, height=8)
ggplot(data,aes(x = X2,y = value)) + facet_wrap(~type, scale="free") +
 geom_boxplot(col="blue") + xlab("") + ylab("") + scale_x_discrete(labels=labs) +
  opts(axis.text.x = theme_text(size = 12))
dev.off()

# Figure 10

library(shapefiles)
library(maptools)

# read an ESRI shapefile of the world (you will need a world shape file)
setwd(pathPosterior)
# Couldn't read in shape file using cshapes instead
# w2007<-readShapePoly("countries07.shp")

library(cshapes)
w2007 <- cshp((date=as.Date("2007-1-1")))

## do some coloring magic
coords<-coordinates(w2007)
dst<-as.matrix(dist(coords, upper = TRUE, diag=TRUE))

xy<-cmdscale(dst, k=3)
r<-rank(xy[,1])/dim(xy)[1]
g<-rank(xy[,2])/dim(xy)[1]
b<-rank(xy[,3])/dim(xy)[1]
farben<-rgb(g,r,0)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))

#add colors back to the map
w2007$mapcolors<-farben

cc.all<-read.csv("ccall.csv",header=T)
rownames(cc.all) <- cc.all[,2]

library(mnormt)

setwd(pathResults)
load("out")

all.names = unique(unlist(sapply(1:19, function(x) colnames(out[[x]]$Y))))

post.uv <- function(t, v, Z0=NULL){
  setwd(pathResults)
  x <- read.table(paste(paste("output.y", paste(v, v, sep="/"), sep="/"),year[t], sep="."), header=FALSE)
  n <- ncol(out[[t]]$Y)
  if (!is.null(Z0)) set <- intersect(colnames(out[[t]]$Y), rownames(Z0))  
  x.star <- array(NA, dim=c(n, ncol(x), nrow(x)/n))
  x <- array(t(x), dim=c(ncol(x), n, nrow(x)/n))
  dimnames(x)[[2]]=dimnames(x.star)[[1]]=colnames(out[[t]]$Y)
  for (i in 1:dim(x)[3]){
    if(!is.null(Z0)) {x.star[set,,i] <- procrustes(Z0[set,], t(x[,set,i]))$Yrot}
    if(is.null(Z0))  {x.star[,,i] <- t(x[,,i])}
  }  
  x <- apply(x.star[set,,], 1:2, mean)
  if(!is.null(Z0)) rownames(x) <- set
  if(is.null(Z0)) rownames(x) <- colnames(out[[t]]$Y)
  x
}


library(vegan)

setwd(pathGraphics)
pdf(file="U_levels_small.pdf", width=6, height=3)

set.seed(100)
Z0 = rmnorm(length(all.names),0, diag(2))
rownames(Z0) = all.names # REFERENCE MATRIX

year=1990:2008
par(mfrow=c(2, 3))
par(mar=c(0,0,0,0), oma=c(0,0,0,0), mgp=c(0,0,0))
plot(w2007,col=farben,lwd=1e-200)

for (t in 1:19){
  
  if(t==1) X = post.uv(t, "u", Z0)
  
  if(t > 1) {
    X.new = X.old = post.uv(t, "u", Z0)
    X.old = X.old*0
    set = intersect(row.names(X.old), row.names(X))
    X.old[set,] = X[set,]
    X = X.old + X.new
  }
  
  if (sum(t==c(1,5,10,15,19))==1) {
    par(mar=c(2,2,2,2))
    plot(X[,1], X[,2], axes=F, main=year[t], pch=19,
      col = as.character(cc.all[row.names(X),5]), type="p", bty="n", 
      xlab="", ylab="", 
      xlim=c(min(X[,1])+sd(X[,1])/10,max(X[,1])-sd(X[,1])/10), cex=1.5)
  abline(h=0, lty=3, lwd=2, col="grey70")
  abline(v=0, lty=3, lwd=2, col="grey70")
  }
}
dev.off()