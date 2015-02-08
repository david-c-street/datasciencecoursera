#Script to plot global active power vs frequency histogram
library(data.table)

#read in datafile
filename <- "household_power_consumption.txt"
path1 <- "~/Google Drive/docs/mooc_courses/dstrack/expdata"
filepath <- paste(path1, filename, sep='/')

#read in whole file as dataframe
housepower <- fread(filepath, na.strings = c("NA","?",""), data.table = F)

#parse data into the chunk I want
startdate <- as.Date('2007-02-01')
enddate <- as.Date('2007-02-02')
alldates <- as.Date(housepower[,1], format="%d/%m/%Y")
dateind <- which(alldates >= startdate & alldates <= enddate)

housepowersub <- housepower[dateind,]

#build plot, save to working dir
plotname = "plot1.png"
png(plotname, width = 480, height = 480)
with(housepowersub, hist(as.numeric(housepowersub[,3]), col='red',main='', xlab=''))
title(main='Global Active Power', xlab='Global Active Power (kilowatts)')
dev.off()