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

#### DYNAMIC TRADE NETWORK MODEL
setwd(pathFunction)
source("gbme_mixture.R")
source("gbme_mix_ef.R")

out <- as.list(1:19)
yrs <- 1990:2008

main.time <- proc.time()

adj2005<-c(1.385035773, 1.337611188, 1.306639547, 1.278369379, 1.252004178, 1.226442068, 1.203533839, 1.182648457, 1.169443339, 1.152488373, 1.128071145, 1.103147907, 1.085573607, 1.062699132, 1.0333676, 1, 0.968452634, 0.941502813, 0.921808135)

setwd(pathData)
load("TradeAndCovData0910.RData")

setwd(pathResults)
for (t in 1:length(yrs)){

  start.time <- proc.time()
  
  Y = ExDat[[t]]*adj2005[t]
  Xs = matrix(cntr(log(CovDat[[t]]$GDPcurrent*adj2005[t])), ncol=1)
  Xd = array(distance[[t]]/max(c(distance[[t]])), dim=c(ncol(Y), ncol(Y), 1))
  #clean up
  rm("CovDat", "distnorm", "ExDat","prox","distance","proxnorm")
    
  cat("Year", yrs[t], '\n')
  out[[t]] <- gbme.mix(Y=Y, Xd=get.Xd(Xd, t), Xs=Xs, Xr=Xs, NS=5000, odens=5, burn=1000, k=2, write.all=TRUE, out.name=yrs[t])

  cat("Time elapsed:", round((proc.time() - start.time)[1]/60,3), "mins", '\n')
  cat("Memory used:", gc()[2,2], "Mb", '\n')
  cat("", '\n')
  
}

cat("Total time elapsed:", round((proc.time() - main.time)[1]/60,3), "mins", '\n')

save(out, file="out")
