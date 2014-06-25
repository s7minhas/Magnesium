### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')

### Load data
setwd(pathData)
load('~/Desktop/Research/BuildingPanelData/panel.rda')

###############################################################
# Organizing WB data
### Fx for Melting/Cleaning WB Data for Merge
setwd(paste(pathData, '/Components',sep=''))
WBgdp <- read.csv('NY.GDP.MKTP.CD_Indicator_MetaData_en_EXCEL.csv')
WBgdpCap <- read.csv('NY.GDP.PCAP.CD_Indicator_MetaData_en_EXCEL.csv')
WBgdpgr <- read.csv('NY.GDP.MKTP.KD.ZG_Indicator_MetaData_en_EXCEL.csv')
WBfdi <- read.csv('BX.KLT.DINV.CD.WD_Indicator_MetaData_en_EXCEL.csv')
WBfdiGdp <- read.csv('BX.KLT.DINV.WD.GD.ZS_Indicator_MetaData_en_EXCEL.csv')
WBpop <- read.csv('SP.POP.TOTL_Indicator_MetaData_en_EXCEL.csv')

WBgdpClean <- cleanWbData(WBgdp, 'gdp')
WBgdpCapClean <- cleanWbData(WBgdpCap, 'gdpCAP')
WBgdpgrClean <- cleanWbData(WBgdpgr, 'gdpGR')
WBfdiClean <- cleanWbData(WBfdi, 'fdi')
WBfdiGdpClean <- cleanWbData(WBfdiGdp, 'fdiGDP')
WBpopClean <- cleanWbData(WBpop, 'population')

# Make sure order matches
sum(WBfdiClean$cyear!=WBfdiGdpClean$cyear)
sum(WBfdiClean$cyear!=WBgdpClean$cyear)
sum(WBfdiClean$cyear!=WBgdpCapClean$cyear)
sum(WBfdiClean$cyear!=WBgdpgrClean$cyear)
sum(WBfdiClean$cyear!=WBpopClean$cyear)

# combine data
setwd(pathData)
wbData <- data.frame(cbind(WBgdpClean,
	gdpCAP=WBgdpCapClean[,4],
	gdpGR=WBgdpgrClean[,4],
	fdi=WBfdiClean[,4],
	fdiGdp=WBfdiGdpClean[,4],
	population=WBpopClean[,4] ) )
###############################################################

###############################################################
# constraints
setwd(paste(pathData, '/Components',sep=''))
constraints <- read.dta('polcon2012.dta')

constraints2 <- constraints[constraints$year>=1960,1:10]
constraints2 <- constraints2[!is.na(constraints2$ccode),]

constraints2$cnts_country <- as.character(constraints2$cnts_country)
constraints2$cnts_country[constraints2$cnts_country=='CONGO (BRA)'] <- 'Congo, Republic of'
constraints2$cnts_country[constraints2$cnts_country=='CONGO (KIN)'] <- 'Congo, Democratic Republic of'
constraints2$cnts_country[constraints2$cnts_country=='CONGO DR'] <- 'Congo, Democratic Republic of'
constraints2$cnts_country[constraints2$cnts_country=='GERMAN DR'] <- "Germany Democratic Republic"

constraints2$cname <- countrycode(constraints2$cnts_country, 'country.name', 'country.name')
constraints2[is.na(constraints2$cname),'cname'] <- countrycode(
	constraints2[is.na(constraints2$cname),'polity_country'],
	'country.name', 'country.name')
constraints2$cname[constraints2$cnts_country=='VIETNAM REP'] <- 'S. VIETNAM'
constraints2$cname[constraints2$cnts_country=='YEMEN PDR'] <- 'S. YEMEN'
constraints2$cname[constraints2$cnts_country=="CZECHOS'KIA"] <- 'CZECH REPUBLIC'
constraints2$cname[constraints2$cnts_country=="YUGOSLAVIA"] <- 'SERBIA'
constraints2 <- constraints2[constraints2$cname!='HONG KONG',]

constraints2$cnameYear <- paste(constraints2$cname, constraints2$year, sep='')

names(table(constraints2$cnameYear)[table(constraints2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
constraints2$ccode <- panel$ccode[match(constraints2$cname,panel$cname)]
constraints2$cyear <- paste(constraints2$ccode, constraints2$year, sep='')
table(constraints2$cyear)[table(constraints2$cyear)>1] # Dupe check
###############################################################

###############################################################
# banks dataset
setwd(paste(pathData, '/Components/Banks Cross National Time Series', sep=''))
banks <- read.csv('CNTSDATA.csv')

banks2 <- banks[banks$year>=1960,c('code', 'Wbcode', 'country', 
	'year', paste('domestic', 1:9, sep=''))]
banks2 <- banks2[!is.na(banks2$code),]
banks2$country <- trim(banks2$country)

banks2$country <- as.character(banks2$country)
banks2$country[banks2$country=='Congo (BRA)'] <- 'Congo, Republic of'
banks2$country[banks2$country=='Congo (KIN)'] <- 'Congo, Democratic Republic of'
banks2$country[banks2$country=='German DR'] <- "Germany Democratic Republic" 
banks2$country[banks2$country=='German FR'] <- "Germany" 
banks2 <- banks2[banks2$country!='Cyprus: Turkish Sector',]
banks2 <- banks2[banks2$country!='Cyprus: Greek Sector',]
banks2 <- banks2[banks2$country!='Senegambia',]
banks2 <- banks2[banks2$country!='Somaliland',]
banks2 <- banks2[banks2$code!=1145,] # Removing extra cases for Trinidad
banks2 <- banks2[banks2$code!=1247,] # Removing extra cases for Venezuela

banks2$cname <- countrycode(banks2$country, 'country.name', 'country.name')
banks2$cname[banks2$country=='Vietnam REP'] <- 'S. VIETNAM'
banks2$cname[banks2$country=='Yemen PDR'] <- 'S. YEMEN'
banks2$cname[banks2$country=="Yemen PDR (So. Yemen)"] <- 'S. YEMEN'
banks2$cname[banks2$country=="Yugoslavia"] <- 'SERBIA'
banks2 <- banks2[banks2$cname!='HONG KONG',]
banks2$cname[banks2$country=="Czechoslovakia"] <- 'CZECH REPUBLIC'

drop <- unique(banks2[is.na(banks2$cname),c('country')])
banks2 <- banks2[which(!banks2$country %in% drop),]

banks2$cnameYear <- paste(banks2$cname, banks2$year, sep='')

names(table(banks2$cnameYear)[table(banks2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
banks2$ccode <- panel$ccode[match(banks2$cname,panel$cname)]
banks2$cyear <- paste(banks2$ccode, banks2$year, sep='')
drop <- unique(banks2[is.na(banks2$ccode),'cname'])
banks2 <- banks2[which(!banks2$cname %in% drop),]
table(banks2$cyear)[table(banks2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Polity
setwd(paste(pathData, '/Components',sep=''))
polity <- read.csv('p4v2011.csv')

polity2 <- polity[polity$year>=1960,3:ncol(polity)]

polity2$country <- as.character(polity2$country)
polity2$country[polity2$country=='UAE'] <- 'United Arab Emirates'
polity2$country[polity2$country=='Congo Brazzaville'] <- 'Congo, Republic of'
polity2$country[polity2$country=='Congo Kinshasa'] <- 'Congo, Democratic Republic of'
polity2$country[polity2$country=='Germany East'] <- "Germany Democratic Republic"
polity2$cname <- countrycode(polity2$country, 'country.name', 'country.name')
polity2$cname[polity2$country=='Yemen South'] <- "S. YEMEN"
polity2$cname[polity2$country=='Vietnam South'] <- "S. VIETNAM"
polity2[polity2$cname=='Yugoslavia', 'cname'] <- 'SERBIA'
polity2[polity2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

polity2$cnameYear <- paste(polity2$cname, polity2$year, sep='')

polity2$drop <- 0
polity2[polity2$scode=='ETH' & polity2$year==1993, 'drop'] <- 1
polity2[polity2$scode=='GMY' & polity2$year==1990, 'drop'] <- 1
polity2[polity2$scode=='YGS' & polity2$year==1991, 'drop'] <- 1
polity2[polity2$scode=='YGS' & polity2$year==2006, 'drop'] <- 1
polity2[polity2$scode=='SDN' & polity2$year==2011, 'drop'] <- 1
polity2[polity2$scode=='DRV' & polity2$year==1976, 'drop'] <- 1
polity2[polity2$scode=='YAR' & polity2$year==1990, 'drop'] <- 1
polity2 <- polity2[polity2$drop==0,]; polity2 <- polity2[,1:(ncol(polity2)-1)]

names(table(polity2$cnameYear)[table(polity2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
polity2$ccode <- panel$ccode[match(polity2$cname,panel$cname)]
polity2$cyear <- paste(polity2$ccode, polity2$year, sep='')
table(polity2$cyear)[table(polity2$cyear)>1] # Dupe check
###############################################################

# ###############################################################
# # ICRG data from PRS group
# setwd(paste(pathData, '/Components',sep=''))
# icrg <- read.csv('PRS_Melted_Format.csv')

# icrg2 <- icrg

# icrg2$Country <- as.character(icrg$Country)
# icrg2$Country[icrg2$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
# icrg2$Country[icrg2$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'
# drop <- c("Hong Kong", "New Caledonia")
# icrg2 <- icrg2[which(!icrg2$Country %in% drop),]
# icrg2$cname <- countrycode(icrg2$Country, 'country.name', 'country.name')
# icrg2[icrg2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

# icrg2$cnameYear <- paste(icrg2$cname, icrg2$Year, sep='')

# icrg2$drop <- 0
# icrg2[icrg2$Country=='Serbia and Montenegro' & icrg2$Year>=2006, 'drop'] <- 1
# icrg2[icrg2$Country=='Serbia' & icrg2$Year<2006, 'drop'] <- 1
# icrg2[icrg2$Country=='Czechoslovakia' & icrg2$Year>=1993, 'drop'] <- 1
# icrg2[icrg2$Country=='Czech Republic' & icrg2$Year<1993, 'drop'] <- 1
# icrg2 <- icrg2[icrg2$drop==0,]; icrg2 <- icrg2[,1:(ncol(icrg2)-1)]

# table(icrg2$cnameYear)[table(icrg2$cnameYear)>1]

# # Adding in codes from panel
# icrg2$ccode <- panel$ccode[match(icrg2$cname,panel$cname)]
# icrg2$cyear <- paste(icrg2$ccode, icrg2$Year, sep='')
# table(icrg2$cyear)[table(icrg2$cyear)>1] # Dupe check
# ###############################################################

###############################################################
# PRIO Civil War
setwd(paste(pathData, '/Components/PRIO_ArmedConflict', sep=''))
war <- read.csv('ucdp.prio.armed.conflict.v4.2013.csv')
civwar <- unique(war[war$Type==3 | war$Type==4,c('SideA', 'YEAR')])

# Cleaning country names
civwar$SideA <- as.character(civwar$SideA)
civwar <- civwar[civwar$SideA!='Hyderabad',]
civwar$SideA[civwar$SideA=='United Arab Emirate'] <- 'United Arab Emirates'
civwar$SideA[civwar$SideA=='Rumania'] <- 'Romania'
civwar$SideA[civwar$SideA=='Serbia (Yugoslavia)'] <- 'SERBIA'
civwar$SideA[civwar$SideA=='DR Congo (Zaire) ']='Congo, Democratic Republic of'

civwar$cname <- cname(civwar$SideA)
civwar$cname[civwar$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
civwar$cnameYear=paste(civwar$cname,civwar$YEAR,sep='')
names(table(civwar$cnameYear)[table(civwar$cnameYear)>1])

civwar$ccode <- panel$ccode[match(civwar$cname,panel$cname)]
civwar$cyear <- paste(civwar$ccode, civwar$YEAR, sep='')
names(table(civwar$cyear)[table(civwar$cyear)>1])

civwar$civwar <- 1
###############################################################

###############################################################
# Combining data
frame <- unique(panel[,c('ccode', 'cname')])
dframe <- NULL; frame$year <- NA; years <- seq(1960,2012,1)
for(ii in 1:length(years)){
	frame$year <- years[ii]; dframe <- rbind(dframe, frame) }
dframe$cyear <- paste(dframe$ccode, dframe$year, sep='')
dim(dframe)
monadData <- merge(dframe, wbData[,c(4,8:ncol(wbData))],by='cyear',all.x=T,all.y=F)
unique(monadData[is.na(monadData$ccode), 1:5]); dim(monadData)
monadData <- merge(monadData, polity2[,c(7:35,ncol(polity2))],by='cyear',all.x=T,all.y=F)
unique(monadData[is.na(monadData$ccode), 1:5]); dim(monadData)
# monadData <- merge(monadData, icrg2[,c(5:16,ncol(icrg2))],by='cyear',all.x=T,all.y=F)
# unique(monadData[is.na(monadData$ccode), 1:5]); dim(monadData)
monadData <- merge(monadData, banks2[,c(5:13,ncol(banks2))],by='cyear',all.x=T,all.y=F)
unique(monadData[is.na(monadData$ccode), 1:5]); dim(monadData)
monadData <- merge(monadData, constraints2[,c(1,8:10)],by='cyear',all.x=T,all.y=F)
unique(monadData[is.na(monadData$ccode), 1:5]); dim(monadData)
monadData <- merge(monadData, civwar[,6:7],by='cyear',all.x=T,all.y=F)
unique(monadData[is.na(monadData$ccode), 1:5]); dim(monadData)
monadData$civwar[is.na(monadData$civwar)]=0

# monadData <- monadData[monadData$year>=1960 & monadData$year<=2005,] # Compliance data ends at 2005
###############################################################

###############################################################
# Var mods
monadData$lgdp=log(monadData$gdp)
monadData$lgdpCAP=log(monadData$gdpCAP)
monadData$lpopulation=log(monadData$population)
monadData$lfdi=log(monadData$fdi + abs(min(monadData$fdi,na.rm=T))+1)
monadData$domSUM=monadData$domestic1+ monadData$domestic2+ monadData$domestic3+
	monadData$domestic4+monadData$domestic5+monadData$domestic6+monadData$domestic7+
	monadData$domestic8
###############################################################

###############################################################
# Set up imputation

# Drop unnecessary vars
drop=c('democ','autoc','polity','durable','xrreg','xrcomp','xropen',
	'xconst','parreg','parcomp','exrec','prior','emonth','eday','eyear',
	'eprec','interim','bmonth','bday','byear','bprec','post','change',
	'd4','sf','regtrans','exconst','polcomp',
	paste0('domestic',1:9),'polconv','polconvj'
	,'gdp','gdpCAP','population', 'fdi')
monadData = monadData[,which(!names(monadData) %in% drop ) ]

# Checks for lagdata function
mdl=monadData
mdl$cyear=numSM(mdl$cyear)
lagVars=names(mdl)[5:ncol(monadData)]
lagVars=lagVars[which(!lagVars %in% c('civwar','polity2'))] # not enough variance in polity and civwar
sum(apply(mdl[,lagVars],2,class)=='numeric')/ncol(mdl[,lagVars])

# Set up lags for sbgcop
mdl=lagDataSM(data=mdl,country_year='cyear',country='ccode',varsTOlag=lagVars,lag=1)
mdl=lagDataSM(data=mdl,country_year='cyear',country='ccode',varsTOlag=lagVars,lag=2)
mdl=lagDataSM(data=mdl,country_year='cyear',country='ccode',varsTOlag=lagVars,lag=3)
mdl=lagDataSM(data=mdl,country_year='cyear',country='ccode',varsTOlag=lagVars,lag=4)
mdl=lagDataSM(data=mdl,country_year='cyear',country='ccode',varsTOlag=lagVars,lag=5)
lagVarsAll=as.vector(apply(t(lagVars), 2, 
	function(x) FUN=paste(paste('lag',1:5,'_',sep=''), x, sep='')))

# Impute missing value
# This takes time, set it and go for a run
sbgcopTimeSR <- system.time(
  sbgData <- sbgcop.mcmc(mdl[,c('ccode','year',lagVars,lagVarsAll,'civwar','polity2')]
  , nsamp=6000, seed=123455, verb=TRUE) ) # default odens = nsamp/1000  

# Clean
impData=data.frame(
	cbind(
		mdl[,c('cyear')],
		sbgData$Y.pmean[,c(lagVars,'civwar','polity')]
	)
)
###############################################################

###############################################################
setwd(pathData)
save(monadData, impData, file='monadData.rda')
save(sbgcopTimeSR, sbgData, file='SBGCOPmonadData.rda')
###############################################################