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

cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] <- var
	mdata$year <-  as.numeric(as.character(substring(mdata$variable,2)))
	mdata <- mdata[,c(1,2,5,4)]

	# Remove non-country observations and small islands/territories
	drop <- c('Arab World', 'Caribbean small states', 
		'East Asia & Pacific (all income levels)', 
		'East Asia & Pacific (developing only)', 'Euro area', 
		'Europe & Central Asia (all income levels)', 
		'Europe & Central Asia (developing only)', 
		'European Union', 'Heavily indebted poor countries (HIPC)', 
		'High income', 'High income: nonOECD', 'High income: OECD', 
		'Latin America & Caribbean (all income levels)', 
		'Latin America & Caribbean (developing only)', 
		'Least developed countries: UN classification', 
		'Low & middle income', 'Low income', 'Lower middle income', 
		'Middle East & North Africa (all income levels)', 
		'Middle East & North Africa (developing only)', 'Middle income', 
		'North America', 'Not classified', 'OECD members', 
		'Other small states', 'Pacific island small states', 
		'Small states', 'South Asia', 
		'Sub-Saharan Africa (all income levels)', 
		'Sub-Saharan Africa (developing only)', 'Upper middle income', 
		'World',
		 "American Samoa",            "Aruba",                    
		 "Bermuda",                   "Cayman Islands", "Channel Islands",          
		 "Curacao",                   "Faeroe Islands",           
		 "French Polynesia",          "Greenland",                
		 "Guam",                      "Hong Kong SAR, China",     
		 "Isle of Man",               "Macao SAR, China",         
		 "New Caledonia",             "Northern Mariana Islands", 
		 "Puerto Rico",               "Sint Maarten (Dutch part)",
		 "St. Martin (French part)",  "Turks and Caicos Islands", 
		 "Virgin Islands (U.S.)",     "West Bank and Gaza")
	mdata <- mdata[which(!mdata$Country.Name %in% drop),]

	# Setting standardized countryname for WB data
	mdata$Country.Name <- as.character(mdata$Country.Name)
	mdata$Country.Name[mdata$Country.Name=='Korea, Dem. Rep.'] <- 'North Korea' 
	mdata$Country.Name[mdata$Country.Name=='Korea, Rep.'] <- 'South Korea' 
	mdata$cname <- countrycode(mdata$Country.Name, 'country.name', 'country.name')
	mdata$cnameYear <- paste(mdata$cname, mdata$year, sep='')
	
	# Adding in codes from panel
	mdata$ccode <- panel$ccode[match(mdata$cname,panel$cname)]
	mdata$cyear <- paste(mdata$ccode, mdata$year, sep='')
	mdata }

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
save(wbData, file='wbData.rda')
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

###############################################################
# ICRG data from PRS group
setwd(paste(pathData, '/Components',sep=''))
icrg <- read.csv('PRS_Melted_Format.csv')

icrg2 <- icrg

icrg2$Country <- as.character(icrg$Country)
icrg2$Country[icrg2$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
icrg2$Country[icrg2$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'
drop <- c("Hong Kong", "New Caledonia")
icrg2 <- icrg2[which(!icrg2$Country %in% drop),]
icrg2$cname <- countrycode(icrg2$Country, 'country.name', 'country.name')
icrg2[icrg2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

icrg2$cnameYear <- paste(icrg2$cname, icrg2$Year, sep='')

icrg2$drop <- 0
icrg2[icrg2$Country=='Serbia and Montenegro' & icrg2$Year>=2006, 'drop'] <- 1
icrg2[icrg2$Country=='Serbia' & icrg2$Year<2006, 'drop'] <- 1
icrg2[icrg2$Country=='Czechoslovakia' & icrg2$Year>=1993, 'drop'] <- 1
icrg2[icrg2$Country=='Czech Republic' & icrg2$Year<1993, 'drop'] <- 1
icrg2 <- icrg2[icrg2$drop==0,]; icrg2 <- icrg2[,1:(ncol(icrg2)-1)]

table(icrg2$cnameYear)[table(icrg2$cnameYear)>1]

# Adding in codes from panel
icrg2$ccode <- panel$ccode[match(icrg2$cname,panel$cname)]
icrg2$cyear <- paste(icrg2$ccode, icrg2$Year, sep='')
table(icrg2$cyear)[table(icrg2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Combining data
setwd(pathData)
save(icrg2, polity2, wbData,
	constraints2, banks2,
	file='cleanedData.rda')

### Load setup
source('/Users/janus829/Desktop/Research/Magnesium/R/Setup.R')
setwd(pathData)
load('cleanedData.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')

frame <- unique(panel[,c('ccode', 'cname')])
dframe <- NULL; frame$year <- NA; years <- seq(1960,2012,1)
for(ii in 1:length(years)){
	frame$year <- years[ii]; dframe <- rbind(dframe, frame) }
dframe$cyear <- paste(dframe$ccode, dframe$year, sep='')
dim(dframe)
combData <- merge(dframe, wbData[,c(4,8:ncol(wbData))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, polity2[,c(7:35,ncol(polity2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, icrg2[,c(5:16,ncol(icrg2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, banks2[,c(5:13,ncol(banks2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, constraints2[,c(8:10,ncol(constraints2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)

save(combData, file='combinedData.rda')
write.csv(combData, file='combinedData.csv')
###############################################################