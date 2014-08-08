if(Sys.info()["user"]=="janus829"){
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')}
if(Sys.info()["user"]=="cassydorff"){
source('/Users/cassydorff/ProjectsGit/Magnesium/R/Setup.R')}


###############################################################
setwd(pathData)
load('durDataEconImp.rda'); tableName='durModelResults.tex'; label='tab:regResults'; caption = 'Duration model with time varying covariates estimated using Cox Proportional Hazards. Standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'

head(aData)
range(aData$slength)

foo<-aData[order(aData$slength, -aData$year, -aData$lag1_uData),]
top20<-foo[1:20, ]
top20<-subset(top20, select=c("id", "year","targetstate", "slength",
		"compliance", "primarysender_ccode", "sender1_ccode",
		"sender2_ccode", "sender3_ccode", "startyear", "endyear",
		"noS", "lag1_uData", "lag1_SuData2") )
write.csv(top20, "top20.csv")

#min & max (kinda)
greatThan35<-foo[which(foo$slength>=35 & foo$slength<40),]
greatThan35<-greatThan35[order(greatThan35$slength, -greatThan35$year, -greatThan35$lag1_uData),]
greatThan35<-subset(greatThan35, select=c("id", "year","targetstate", "slength",
		"compliance", "primarysender_ccode", "sender1_ccode",
		"sender2_ccode", "sender3_ccode", "startyear", "endyear",
		"noS", "lag1_uData", "lag1_SuData2") )

write.csv(greatThan35, "longcases.csv")


lessThan5<-foo[which(foo$slength<5),]
lessThan5<-lessThan5[order(lessThan5$slength, -lessThan5$year, -lessThan5$lag1_uData),]
lessThan5<-subset(lessThan5, select=c("id", "year","targetstate", "slength",
		"compliance", "primarysender_ccode", "sender1_ccode",
		"sender2_ccode", "sender3_ccode", "startyear", "endyear",
		"noS", "lag1_uData", "lag1_SuData2") )

write.csv(lessThan5, "shortcases.csv")

