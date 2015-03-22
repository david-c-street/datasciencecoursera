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

#calculate summary statistics of sim data
meanmeans <- mean(dfmeansim$meansimexp)
medmeans <- median(dfmeansim$meansimexp)
varmeans <- var(dfmeansim$meansimexp)

#create sim data for theoretical distribution of means, what it should look like
nnormsims <- 10*nmeansims
dfmeanth <- tbl_df(data.frame(meanth=rnorm(nnormsims,
	 mean=thmeanmean, sd=thmeansd)))

#Compare sim results to theoretical
#hist of just simulated mean data
simmeanhist <- ggplot(data=dfmeansim, aes(x=meansimexp)) 
simmeanhist <- simmeanhist + geom_histogram(binwidth=0.2)
simmeanhist <- simmeanhist + geom_vline(xintercept=thmeanmean, color='red')
simmeanhist <- simmeanhist + geom_vline(xintercept=meanmeans, color='blue')
simmeanhist <- simmeanhist + coord_cartesian(xlim=c(2,8),ylim=c(0,nmeansims/9))
simmeanhist <- simmeanhist + labs(title='Sample Mean Distribution',
	x='sample mean') 

#hist of theoretical distribution
thmeanhist <- ggplot(data=dfmeanth, aes(x=meanth))
thmeanhist <- thmeanhist + geom_histogram(binwidth=0.2)
thmeanhist <- thmeanhist + geom_vline(xintercept=thmeanmean, color='red')
thmeanhist <- thmeanhist + coord_cartesian(xlim=c(2,8),ylim=c(0,nnormsims/9))
thmeanhist <- thmeanhist + labs(title='Theoretical Sample Mean Distribution',
	x='sample mean (simulated)') 

#hist of an individual exponential
dfsimexp <- tbl_df(data.frame(exp=rexp(nmeansims, lambda)))
exphist1 <- ggplot(data=dfsimexp, aes(x=exp)) + geom_histogram(binwidth=2)
exphist1 <- exphist1 + labs(title='Exponential Distribution Simulation',
	x='exponential data values (simulated)') 


