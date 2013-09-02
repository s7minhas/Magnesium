# Purpose: Some basic duration modeling attempts for magnesium 
# Author: CD 

rm(list=ls())
library(survival)
library(OIsurv)
library(foreign)
#library(eha)

#Lazy laoding of our data and krustev rep
load("~/Dropbox/My Research/Magnesium/Data/forCassyDurPractice.rda")
krust<-read.dta("~/Dropbox/My Research/Magnesium/Data/Replication Krustev/duration.dta")

head(aData)
head(krust)

##########################
# create survival objects
##########################

# For right-censored data, only two arguments are needed in the Surv() function: a vector of times and a vector indicating which times are observed and censored.
dv1<- Surv(aData$year, aData$compliance, type="right")

# For left truncated right- censored data, three arguments are needed
dv2<-Surv(aData$startyear, aData$endyear2, aData$compliance)

##########################
# models
##########################

# model, simple example
survfit(dv1 ~1)
fitOne <- survfit(dv1 ~1)
summary(fitOne)
plot(fitOne, main="Kaplan-Meier estimate with 95% confidence bounds",
xlab="time", ylab="survival function")

# model, weibull
wFit<-survreg(dv1 ~ autoc, data=aData, dist='weibull') 
summary(wFit)

# more basic examples 
fitTwo<-survfit(dv1~ aData$Ethnic.Tensions)
summary(fitTwo)

fitThree<-survfit(dv1~ autoc + Government.Stability + parcomp, data=aData)
summary(fitThree)

cb<-confBands(dv1,confLevel=.95, type="hall")
plot(fitThree, xlim=c(100,600), xlab="time",  ylab="Estimated Survival Function")
lines(cb$time, cb$lower, lty=3, type="s")
lines(cb$time, cb$upper, lty=3, type="s")
legend(100, 0.3, legend=c("K-M survival estimate","pointwise intervals","confidence bands"), lty=1:3)

# Slightly diff exampe
model1<-weibreg(dv1 ~ autoc + Government.Stability + change + parcomp, data=aData, shape=0)
summary(model1) 
model1$coefficients
plot.weibreg(model1)

#semi parametric cox proportional hazards model KRUSTEV replication
cph1Rep<- coxph(Surv(krust$year, krust$marker, type="right") ~ ldistance + lev4cont + powdisparity + allies + jointdem + kdeplo, data=krust)
summary(cph1Rep)
cph2Rep<-coxph(Surv(krust$strtyr, krust$marker) ~ ldistance + lev4cont + powdisparity + allies + jointdem + kdeplo, data=krust)



