#script to process storm data and do some analysis on it
library(ggplot2)
library(data.table)
library(dplyr)

#file and data processing
originalfile <- 'StormData.csv'
cleanfile <- 'stormdataclean.csv'
#make a nice CSV file and save it
if (!file.exists(cleanfile)){
	rawstormdata <- read.csv(originalfile)
	rawstormdata <- tbl_df(rawstormdata)
	#get rid of some unused columns to speed up operations
	rawstormdata <- select(rawstormdata, -REMARKS, -STATE__,
		-(BGN_RANGE:END_LOCATI), -(CROPDMG:ZONENAMES))
	write.csv(rawstormdata, cleanfile)
}

#load data into local data table, make a couple changes
if (!exists('stormdata')){
	stormdata <- tbl_dt(fread(cleanfile, stringsAsFactors=T))
}

#create convenient grouped data sets and get relevant sums
#human costs
stormdata_byevent <- group_by(stormdata, EVTYPE)
stormdata_byevent_humansums <- tbl_df(summarise(stormdata_byevent, 
	fatalities=sum(FATALITIES, na.rm=T), injuries = sum(INJURIES, na.rm=T),
	casualties=sum(FATALITIES, INJURIES, na.rm=T)))
stormdata_byevent_humansums <- filter(stormdata_byevent_humansums, casualties>0)
stormdata_byevent_humansums <- arrange(stormdata_byevent_humansums,
	desc(casualties))
stormdata_byevent_humansumstop5 <- head(stormdata_byevent_humansums, n=5)

#property costs	
stormdata_byeventpdmg <- group_by(stormdata, EVTYPE, PROPDMGEXP)
stormdata_byevent_propsums <- tbl_df(summarise(stormdata_byeventpdmg, 
	damages=sum(PROPDMG, na.rm=T)))
stormdata_byevent_propsums <- filter(stormdata_byevent_propsums, damages>0)
#one last processing step to get apply PROPDMGEXP column to sums
#create a new column of data that translates EXP tag into a multiplier
allexp <- stormdata_byevent_propsums[,'PROPDMGEXP']
multy <- numeric()
for (i in 1:dim(allexp)[1]){
	crnt_str <- as.character(allexp[i,])
	if (crnt_str=='M' | crnt_str=='m'){
		multy <- c(multy, 1000000.0)
	} else if (crnt_str=='B' | crnt_str=='b'){
		multy <- c(multy, 1000000000.0)
	} else {
		#assume if it is K, k or blank, the multiplier in 1,000
		multy <- c(multy, 1000.0)
	}
}
#use that multiplier to create a columns that is the actual numerical damage
stormdata_byevent_propsums <- cbind(stormdata_byevent_propsums, multy=multy)
stormdata_byevent_propsums <- mutate(stormdata_byevent_propsums, 
	propdmgtotal=damages*multy)
#group by one more time to consolidate the event types in to one row
stormdata_byeventpdmg <- group_by(stormdata_byevent_propsums, EVTYPE)
stormdata_byevent_propsums <- summarise(stormdata_byeventpdmg,
	damages= sum(propdmgtotal))
stormdata_byevent_propsums <- arrange(stormdata_byevent_propsums,
	desc(damages))
stormdata_byevent_propsumstop5 <- head(stormdata_byevent_propsums, n=5)
	
	
#plots
propbartop5 <- ggplot(head(stormdata_byevent_propsumstop5))
propbartop5 <- propbartop5 + geom_bar(aes(x=EVTYPE, y=damages),
	stat='identity')
propbartop5 <- propbartop5 + labs(title='5 Most Costly Events',
	x='Event', y='Property Damage (USD)')
	
humanbartop5 <- ggplot(head(stormdata_byevent_humansumstop5))
humanbartop5 <- humanbartop5 + geom_bar(aes(x=EVTYPE, y=casualties),
	stat='identity')
humanbartop5 <- humanbartop5 + labs(title='5 Most Dangerous Events',
	x='Event', y='Casualties')	