#load needed libraries, set some simulation parameters
library(ggplot2)
library(dplyr)
randseed <- 1
set.seed(1)
nexpsims <- 40
nmeansims <- 1000
#properties of source exponential dist
lambda <- 0.2
thmean <- 1/lambda
thsd <- 1/lambda
thvar <- thsd^2
#properties of theoretical CLT distribution
thmeanmean <- thmean
thmeansd <- thsd/sqrt(nexpsims)
thmeanvar <- thmeansd^2

#create simulated data
#create a set of data that is the means of 40 sets of exp data
msimdata <- matrix(ncol=nmeansims, nrow=nexpsims)
for (i in 1:nmeansims) {
	msimdata[,i] <- rexp(nexpsims, lambda)
}

#create nice tidy data frames to hold data
#sim data
dfdatasim <- data.frame(msimdata)
names(dfdatasim) <- paste('sim', as.character(1:nmeansims), sep='')
dfdatasim <- tbl_df(dfdatasim)
#means of sim data
dfmeansim <- tbl_df(data.frame(meansimexp=colMeans(msimdata)))

#create sim data for theoretical distribution of means
dfmeanth <- tbl_df(data.frame(meanth=rnorm(nmeansims,
	 mean=thmeanmean, sd=thmeansd)))

#calculate summary statistics of sim data and theoretical


#Compare sim results to theoretical
#hist of just simulated mean data
simmeanhist <- ggplot(data=dfmeansim, aes(x=meansimexp)) 
simmeanhist <- simmeanhist + geom_histogram(binwidth=0.1)

#hist of theoretical distribution
thmeanhist <- ggplot(data=dfmeanth, aes(x=meanth)) 
thmeanhist <- thmeanhist + geom_histogram(binwidth=0.1)

#hist of a individual sim cases
exphist1 <- ggplot(data=dfdatasim, aes(x=sim1)) + geom_histogram(binwidth=3)
exphist2 <- ggplot(data=dfdatasim, aes(x=sim2)) + geom_histogram(binwidth=3)


