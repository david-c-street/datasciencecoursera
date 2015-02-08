#Script to create time vs. global active power line plot
library(data.table)

#read in datafile
filename <- "household_power_consumption.txt"
pathname <- "~/Google Drive/docs/mooc_courses/dstrack/expdata"
filepath <- paste(pathname, filename, sep='/')

#read in whole file as dataframe
housepower <- fread(filepath, na.strings = c("NA","?",""), data.table = F)

#parse data into the chunk I want
startdate <- as.Date('2007-02-01')
enddate <- as.Date('2007-02-02')
alldates <- as.Date(housepower[,1], format="%d/%m/%Y")
dateind <- which(alldates >= startdate & alldates <= enddate)

housepowersub <- housepower[dateind,]
timesubraw <- paste(housepower[dateind,1], housepower[dateind,2])
timesub <- strptime(timesubraw, format="%d/%m/%Y %H:%M:%S")

#build plot, save to working dir
plotname = "plot2.png"
png(plotname, width = 480, height = 480)
with(housepowersub, plot(timesub, as.numeric(housepowersub[,3]), type='l',
	 main='', xlab='', ylab=''))
title(ylab='Global Active Power (kilowatts)')
dev.off()