source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
# http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-cox-regression.pdf

Rossi <- read.table("http://cran.r-project.org/doc/contrib/Fox-Companion/Rossi.txt",
	header=T)
Rossi$id = 1:nrow(Rossi)

mod.allison <- coxph(Surv(week, arrest) ~ 
	fin + age + race + wexp + mar + paro + prio, 
	data=Rossi)
mod.allison

summary(mod.allison)

attach(Rossi)
Rossi.fin <- data.frame(fin=c(0,1), age=rep(mean(age),2), race=rep(mean(race),2),
	wexp=rep(mean(wexp),2), mar=rep(mean(mar),2), paro=rep(mean(paro),2),
	prio=rep(mean(prio),2))
detach(Rossi)

# legend(locator(1), legend=c('fin = 0', 'fin = 1'), lty=c(1,2))

Rossi.2 <- matrix(0, 19809, 15) # to hold new data set
colnames(Rossi.2) <- c('start', 'stop', 'arrest.time', names(Rossi)[c(1:10,ncol(Rossi))], 'employed')

row <- 0 # set record counter to 0
for (i in 1:nrow(Rossi)) { # loop over individuals
 for (j in 11:62) { # loop over 52 weeks
	 if (is.na(Rossi[i, j])) next # skip missing data
	 else {
		 row <- row + 1 # increment row counter
		 start <- j - 11 # start time (previous week)
		 stop <- start + 1 # stop time (current week)
		 arrest.time <- if (stop == Rossi[i, 1] && Rossi[i, 2] ==1) 1 else 0
		 # construct record:
		 Rossi.2[row,] <- c(start, stop, arrest.time, unlist(Rossi[i, c(1:10,ncol(Rossi), j)]))
	 }
	}
}

Rossi.2 <- as.data.frame(Rossi.2)
remove(i, j, row, start, stop, arrest.time) # clean up

mod.allison.2 <- coxph(Surv(start, stop, arrest.time) ~
	fin + age + race + wexp + mar + paro + prio + employed
	+ frailty(id),
	data=Rossi.2)
summary(mod.allison.2)

attach(Rossi.2)
Rossi.fin.2 <- data.frame(fin=c(0,1), age=rep(mean(age),2), race=rep(mean(race),2),
	wexp=rep(mean(wexp),2), mar=rep(mean(mar),2), paro=rep(mean(paro),2),
	prio=rep(mean(prio),2), employed=rep(mean(prio),2))
detach(Rossi.2)


# Comparing fit of time-fixed and varying
par(mfrow=c(1,2))
plot(survfit(mod.allison), ylim=c(.7, 1), 
	xlab='Weeks', ylab="Proportion Not Rearrested")
plot(survfit(mod.allison.2), ylim=c(.7, 1), 
	xlab='Weeks', ylab="Proportion Not Rearrested")

plot(survfit(mod.allison, newdata=Rossi.fin), conf.int=T,
	lty=c(1,2), ylim=c(.6, 1))
plot(survfit(mod.allison.2, newdata=Rossi.fin.2), conf.int=T,
	lty=c(1,2), ylim=c(.6, 1))
############################################################
# SM replication of Krustev Table 1, Model 1
############################################################

setwd(paste(pathData, '/Replication Krustev', sep=''))
krust <- read.dta('duration.dta')

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