#script to preprocess and analyze activity data

#this is a sort of rough working draft while the final analysis will go into
# the R markdown file PA1_template.Rmd
library(dplyr)
library(lubridate)
library(ggplot2)

#load data to a data frame
dfraw <- read.csv("./activity.csv")

#create a tbl_df to manipulate with dplyr
tdfraw <- tbl_df(dfraw)
rm(dfraw)

#mutate data as needed to make it easier to work with
tdf1 <- transmute(tdfraw, date=ymd(date), interval, steps)

createweekendfactor <- function(wdayints){
	#create empty container for char vector
	charout <- character(length(wdayints))
	#get weekend and weekday values
	weekdayind <- which(wdayints>1 & wdayints<7)
	weekendind <- which(wdayints==1 | wdayints==7)
	#assign values
	charout[weekdayind] <- 'weekday'
	charout[weekendind] <- 'weekend'
	
	return(as.factor(charout))
}

tdf <- mutate(tdf1, 
	dayofweek = wday(date), 
	partofweek = createweekendfactor(dayofweek))
rm(tdf1)

#create some useful grouped data sets
tdf_bydate <- group_by(tdf, date)
tdf_byday <- group_by(tdf, dayofweek)
tdf_bydayint <- group_by(tdf, dayofweek, interval)
tdf_bypartint <- group_by(tdf, partofweek, interval)
tdf_byint <- group_by(tdf, interval)

#create mean and median data from grouped data
ms_bydate <- summarise(tdf_bydate,
	meanstepbyint=mean(steps, na.rm=T),
	sumstepbydate=sum(steps, na.rm=T))
	 
ms_byint <- summarise(tdf_byint,
	meanstepbyint=mean(steps, na.rm=T),
	sumstepbyint=sum(steps, na.rm=T))
	
m_bydayint <- summarise(tdf_bydayint,
	meanstepbyint = mean(steps, na.rm=T))
	
#max, mean, median calculations
meansumperday <- mean(ms_bydate$sumstepbydate, na.rm=T)
medsumperday <- median(ms_bydate$sumstepbydate, na.rm=T)
maxstepint <- max(ms_byint$meanstepbyint, na.rm=T)
	
#impute missing values to form new data set without any NA's
tdfnaind <- which(is.na(tdf$steps))
num_na <- length(tdfnaind)

#create a copy to add imputed values to
imptdf <- tdf

for (ind in tdfnaind) {
	#find the average for this day of week and this interval
	crnt_day <- tdf[[ind, 'dayofweek']]
	crnt_int <- tdf[[ind, 'interval']]
	crnt_dayintavg <- filter(m_bydayint, 
		dayofweek==crnt_day, interval==crnt_int)[[1, 'meanstepbyint']]
		
	#add value to new data frame
	imptdf[ind, 'steps'] <- crnt_dayintavg
}

#create some useful grouped data sets with imputed data
imptdf_bydate <- group_by(imptdf, date)
imptdf_bydayint <- group_by(imptdf, dayofweek, interval)
imptdf_bypartint <- group_by(imptdf, partofweek, interval)
imptdf_byint <- group_by(imptdf, interval)

#create mean and median data from grouped data
msimp_bydate <- summarise(imptdf_bydate,
	meanstepbyint=mean(steps, na.rm=T),
	sumstepbydate=sum(steps, na.rm=T))
	 
msimp_byint <- summarise(imptdf_byint,
	meanstepbyint=mean(steps, na.rm=T),
	sumstepbyint=sum(steps, na.rm=T))
	
mimp_bydayint <- summarise(imptdf_bydayint,
	meanstepbyint = mean(steps, na.rm=T))
	
mimp_bypartint <- summarise(imptdf_bypartint,
	meanstepbyint = mean(steps, na.rm=T))

#create some plots	 
histsumbydate <- ggplot(data=ms_bydate, aes(x=sumstepbydate))
histsumbydate <- histsumbydate + geom_histogram()

histimpsumbydate <- ggplot(data=msimp_bydate, aes(x=sumstepbydate))
histimpsumbydate <- histimpsumbydate + geom_histogram()

linebyint <- ggplot(data=ms_byint)
linebyint <- linebyint + geom_line(aes(x=interval, y=meanstepbyint))

scatbyintday <- ggplot(data=m_bydayint, aes(x=interval, y=meanstepbyint))
scatbyintday <- scatbyintday + geom_point() + facet_wrap(~dayofweek)

linebyintpart <- ggplot(data=mimp_bypartint, aes(x=interval, y=meanstepbyint))
linebyintpart <- linebyintpart + geom_line() + facet_wrap(~partofweek)