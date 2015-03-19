#load needed libraries, set some simulation parameters
library(ggplot2)
randseed <- 1
nexpsims <- 1000
nmeansims <- 40

#create simulated data
#create a single set of exponential data
expdata <- rexp(nexpsims, 0.2)

#create a set of data that is the means of 40 sets of exp data
meanexpdata <- numeric()
for (i in 1:nmeansims) {
	meanexpdata <- c(meanexpdata, mean(rexp(nexpsims, 0.2)))
}

#Compare sim results to theoretical
#hist with line overlay of theoretical, vertical line for mean

#calculate summary statistics of sim data and theoretical


#Compare sim means dist to theoretical