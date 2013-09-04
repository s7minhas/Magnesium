# # Purpose: Some basic duration modeling attempts for magnesium 
# # Author: CD 

# rm(list=ls())
# library(survival)
# library(OIsurv)
# library(foreign)
# #library(eha)

# #Lazy laoding of our data and krustev rep
# load("~/Dropbox/My Research/Magnesium/Data/forCassyDurPractice.rda")
# krust<-read.dta("~/Dropbox/My Research/Magnesium/Data/Replication Krustev/duration.dta")

#source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')


setwd(pathData)
load('forCassyDurPractice.rda')
setwd(paste(pathData, '/Replication Krustev', sep=''))
krust <- read.dta('duration.dta')

# head(aData)
# head(krust)

# ##########################
# # create survival objects
# ##########################

# # For right-censored data, only two arguments are needed in the Surv() function: a vector of times and a vector indicating which times are observed and censored.
# dv1<- Surv(aData$year, aData$compliance, type="right")

# # For left truncated right- censored data, three arguments are needed
# dv2 <- Surv(aData$startyear, aData$endyear2, aData$compliance)

# ##########################
# # models
# ##########################

# # model, simple example
# survfit(dv1 ~1)
# fitOne <- survfit(dv1 ~1)
# summary(fitOne)
# plot(fitOne, main="Kaplan-Meier estimate with 95% confidence bounds",
# xlab="time", ylab="survival function")

# # model, weibull
# wFit<-survreg(dv1 ~ autoc, data=aData, dist='weibull') 
# summary(wFit)

# # more basic examples 
# fitTwo<-survfit(dv1~ aData$Ethnic.Tensions)
# summary(fitTwo)

# fitThree<-survfit(dv1~ autoc + Government.Stability + parcomp, data=aData)
# summary(fitThree)

# cb<-confBands(dv1,confLevel=.95, type="hall")
# plot(fitThree, xlim=c(100,600), xlab="time",  ylab="Estimated Survival Function")
# lines(cb$time, cb$lower, lty=3, type="s")
# lines(cb$time, cb$upper, lty=3, type="s")
# legend(100, 0.3, legend=c("K-M survival estimate","pointwise intervals","confidence bands"), lty=1:3)

# # Slightly diff exampe
# model1<-weibreg(dv1 ~ autoc + Government.Stability + change + parcomp, data=aData, shape=0)
# summary(model1) 
# model1$coefficients
# plot.weibreg(model1)

# #semi parametric cox proportional hazards model KRUSTEV replication
# cph1Rep<- coxph(Surv(krust$year, krust$marker, type="right") ~ ldistance + lev4cont + powdisparity + allies + jointdem + kdeplo, data=krust)
# summary(cph1Rep)
# cph2Rep<-coxph(Surv(krust$strtyr, krust$marker) ~ ldistance + lev4cont + powdisparity + allies + jointdem + kdeplo, data=krust)

############################################################
# SM replication of Krustev Table 1, Model 1
krust$begin <- as.Date(paste(krust[,'strtyr'], 
	krust[,'strtmnth'], krust[,'strtday'],sep='-'))
krust$end <- as.Date(paste(krust[,'endyear'], 
	krust[,'endmnth'], krust[,'endday'],sep='-'))
krust$temp <- krust$end - krust$begin
krust[1:10, c(6:8, 10:12, 18:19, 23, 48:49)]


krust2 <- krust[krust$marker==1,]
krust2$cens2 <- krust2$censored
krust2$cens2[krust2$cens2==1] <- 0
krust2$cens2[is.na(krust2$cens2)] <- 1

# Time fixed covariate model
cpModKr <- coxph(Surv(krust2$calcdur, krust2$cens2, type="right") 
	~ ldistance + lev4cont + powdisparity + allies + jointdem + kdeplo,
	 data=krust2)
summary(cpModKr)

############################################################
# Returning to our modeling
cpModF <- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict,
	data=aData)
summary(cpModF)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))

cpMod <- coxph(
	Surv(aData$startyear, aData$endyear2, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict,
	data=aData)
summary(cpMod)

plot(survfit(cpMod), conf.int=T, ylim=c(0.8, 1))
cox.zph(cpMod)