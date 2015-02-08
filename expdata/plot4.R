#Script to create house power quad plot
#upper left: time vs global active power line plot
#upper right: time vs voltage line plot
#lower left: time vs sub metering line plot
#lower right: time vs global reactive power line plot

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
plotname = "plot4.png"
png(plotname, width = 480, height = 480)
with(housepowersub, {
	par(mfrow = c(2,2))
	
	#subplot 1, upper left
	plot(timesub, as.numeric(housepowersub[,3]), type='l', main='', xlab='', ylab='')
	title(ylab='Global Active Power')
	
	#subplot 2, upper right
	plot(timesub, as.numeric(housepowersub[,5]), type='l',main='', xlab='', ylab='')
	title(ylab='Voltage', xlab='datetime')
	
	#subplot 3, lower left
	plot(timesub, as.numeric(housepowersub[,7]), type='l',main='', xlab='', ylab='')
	lines(timesub, as.numeric(housepowersub[,8]), col='red')
	lines(timesub, as.numeric(housepowersub[,9]), col='blue')
	title(ylab='Energy sub metering')
	legend("topright", legend=names(housepowersub)[7:9], lty=1, 
		col=c('black','red','blue'))
	
	#subplot 4, lower right
	plot(timesub, as.numeric(housepowersub[,4]), type='l',main='', xlab='', ylab='')
	title(ylab='Global reactive power', xlab='datetime')
	
})
dev.off()