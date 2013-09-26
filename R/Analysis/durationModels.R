# Purpose: Some basic duration modeling attempts for magnesium 

source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
# source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')

setwd(pathData)
load('durData.rda')

############################################################
# Time varying models
# Controlling for heterogeneity in duration data
require(CRISP)
aData$date=paste(aData$year,'1-1',sep='-')
aData$ccode=aData$caseid
duration=build.duration(data=aData, y='compliance',
	trainingend='1990-01-01', teststart='1991-01-01',dataend='2005-01-01')

dataend='2005-01-01'
teststart='1991-01-01'
trainingend='2000-01-01'
y='compliance'
data=aData
lastdate = trainingend

> build.duration
function (data, y, trainingend = NULL, teststart = NULL, dataend = NULL) 
{
    if (is.null(data)) 
        stop("No data supplied")
    if (is.null(teststart)) 
        stop("teststart not defined")
    if (is.null(trainingend)) 
        stop("testend not defined")
    if (is.null(dataend)) 
        stop("dataend (end of data available) is not defined")
    if (!y %in% names(data)) 
        stop("Var. name y not in provided data.")
    duration.convert <- function(data, y, lastdate = NULL) {
        lastdate <- as.Date(lastdate)
        data <- subset(data, date <= lastdate)
        data <- data[order(data$ccode, data$date), ]
        failure <- function(x) return(c(0, pmax(0, diff(x))))
        data$failure <- unlist(by(data[, y], data$ccode, failure))
        data <- subset(data, !(get(y) == 1 & failure == 0))
        data$end.spell <- ifelse(data$date == as.Date(lastdate), 
            1, 0)
        data$end.spell <- ifelse(data$failure == 1, 1, data$end.spell)
        data$spellID <- rev(cumsum(rev(data$end.spell)))
        failedspells <- data$spellID[data$failure == 1]
        helper <- cbind(failedspells, 1)
        colnames(helper) <- c("spellID", "c")
        data <- merge(data, helper, by = ("spellID"), all.x = TRUE)
        data$c[is.na(data$c)] <- 0
        helper <- rep(1, dim(data)[1])
        data <- data[order(data$spellID, data$date), ]
        data$duration <- unlist(by(helper, data$spellID, cumsum))
        data <- data[order(data$ccode, data$date), ]
        return(data)
    }
    training <- duration.convert(data, y, lastdate = trainingend)
    full <- duration.convert(data, y, lastdate = dataend)
    test <- subset(full, date >= as.Date(teststart))
    pred.data <- subset(test, date == as.Date(dataend))
    missing <- setdiff(unique(data$ccode), unique(pred.data$ccode))
    missing <- subset(data[data$date == dataend, ], ccode %in% 
        missing)
    missing$failure <- 0
    missing$end.spell <- NA
    missing$spellID <- NA
    missing$c <- 1
    missing$duration <- 1
    pred.data <- rbind(pred.data, missing)
    training$t.0 <- training$duration - 1
    test$t.0 <- test$duration - 1
    pred.data$t.0 <- pred.data$duration - 1
    output <- list(training = training, test = test, pred.data = pred.data)
    return(output)
}

############################################################

############################################################
# Time fixed models
############################################################
# Modeling with net data (edata = exports)
	# Finding thus far: in almost all models, ethnic tensions / internal conflict is significant

emodel1<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + edata,
	data=aData)
summary(model1)
plot(survfit(model1), conf.int=T, ylim=c(0.8, 1))

emodel2<-cpModF <- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + Government.Stability + edata,
	data=aData)
summary(model2)
plot(survfit(model2), conf.int=T, ylim=c(0.8, 1))

emodel3<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + edata, data=aData)
summary(model3)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))


############################################################
# Modeling with net data (tdata = trade)
############################################################
	# here too, ethnic tensions and internal conflict play a role
tmodel1<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + tdata,
	data=aData)
summary(tmodel1)
plot(survfit(tmodel1), conf.int=T, ylim=c(0.8, 1))

tmodel2<-cpModF <- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + Government.Stability + tdata,
	data=aData)
summary(tmodel2)
plot(survfit(tmodel2), conf.int=T, ylim=c(0.8, 1))

tmodel3<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + tdata +polity*tdata, data=aData)
summary(tmodel3)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))

#no of Senders influential 
tmodel4<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + 
	tdata +polity*tdata +noS, data=aData)
summary(tmodel4)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))

############################################################
# Modeling with net data (allydata)
############################################################

allymodel1<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + allydata,
	data=aData)
summary(allymodel1)
plot(survfit(allymodel1), conf.int=T, ylim=c(0.8, 1))

allymodel2<-cpModF <- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + Government.Stability + allydata,
	data=aData)
summary(allymodel2)
plot(survfit(allymodel2), conf.int=T, ylim=c(0.8, 1))

allymodel3<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions + allydata, data=aData)
summary(allymodel3)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))

############################################################
# Modeling with net data (multiple)
############################################################
	#ethnic tensions still play a role
	#internal conflict matter! 
    # so does Democratic.Accountability, so domestic stuff seems important
	#interestingly in the last model, allydata becomes sig at the .1 level.
	#also, note the changes from model 3 to 4, kinda interesting.
model1<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + tdata +
	 allydata, data=aData)
summary(model1)
plot(survfit(allymodel1), conf.int=T, ylim=c(0.8, 1))

#add ethnic tensions and gov stability (latter doesn't matter)
model2<-cpModF <- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity + Internal.Conflict + Ethnic.Tensions
	+ Government.Stability + tdata +  allydata,
	data=aData)
summary(model2)
plot(survfit(allymodel2), conf.int=T, ylim=c(0.8, 1))

#add demo accountability **THIS might be one of the most interesting
#of the models
model3<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity  + Democratic.Accountability + Internal.Conflict 
	+ Ethnic.Tensions + tdata + allydata, data=aData)
summary(model3)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))

#model partly inspired by gibson article
#ethnic tensions * ally, makes ethnic tensions drop

model4<- coxph(
	Surv(aData$slength, aData$compliance) ~
	noS + gdpCAP + polity  + Democratic.Accountability 
	+ Internal.Conflict + Ethnic.Tensions 
    + tdata + allydata + Internal.Conflict *allydata, data=aData)
summary(model4)
plot(survfit(cpModF), conf.int=T, ylim=c(0.8, 1))

model5<- coxph(
	Surv(aData$slength, aDat
		a$compliance) ~
	noS + gdpCAP + polity  + Democratic.Accountability
	+ Internal.Conflict + Ethnic.Tensions + tdata
	+ allydata + polity*tdata, data=aData)
summary(model5)

#things I tried and dropped: 
#xropen
#Bureaucracy.Quality











# ##########################
# # create survival objects
# ##########################

# # For right-censored data, only two arguments are needed in the Surv() function: a vector of times and a vector indicating which times are observed and censored.
# dv1<- Surv(aData$year, aData$compliance, type="right")

# # For left truncated right- censored data, three arguments are needed
# dv2 <- Surv(aData$startyear, aData$endyear2, aData$compliance)

# ##########################
# # model types & examples
# ##########################

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

