#script to preprocess and analyze activity data

#this is a sort of rough working draft while the final analysis will go into
# the R markdown file PA1_template.Rmd
library(plyr)
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
tdf_bypart <- group_by(tdf, partofweek)
tdf_byint <- group_by(tdf, interval)

#create mean and median data from grouped data
mms_bydate <- summarise(tdf_bydate,
	meanstepsbydate=mean(steps, na.rm=T),
	medstepbydate=median(steps, na.rm=T),
	sumstepbydate=sum(steps, na.rm=T))
	 
mms_byint <- summarise(tdf_byint,
	meanstepbyint=mean(steps, na.rm=T),
	medstepbyint=median(steps, na.rm=T),
	sumstepbyint=sum(steps, na.rm=T)) 
	 
histbydate <- ggplot(data=mm_bydate) + geom_histogram(aes(x=meanbydate))
linebyint <- ggplot(data=mms_byint) + geom_line(aes(x=interval, y=meanstepbyint))

