#Script to clean up UCI HAR dataset of phone accelerometer readings
library(dplyr)

#input file locations relative to working dir
test_data_rep <- './UCI HAR Dataset/test'
train_data_rep <- './UCI HAR Dataset/train'

#load all data into main memory - not that big, ~50 mb total
test_xdata_file <- paste(test_data_rep, 'X_test.txt', sep = "/")
test_ydata_file <- paste(test_data_rep, 'y_test.txt', sep = "/")
test_subject_file <- paste(test_data_rep, 'subject_test.txt', sep = "/")
train_xdata_file <- paste(train_data_rep, 'X_train.txt', sep = "/")
train_ydata_file <- paste(train_data_rep, 'y_train.txt', sep = "/")
train_subject_file <- paste(train_data_rep, 'subject_train.txt', sep = "/")

#test_xdata = read.table(test_xdata_file)
#test_ydata = read.table(test_ydata_file)
#test_subdata = read.table(test_subject_file)
#train_xdata = read.table(train_xdata_file)
#train_ydata = read.table(train_ydata_file)
#train_subdata = read.table(train_subject_file)

#combine train and test data
xdata <- rbind(train_xdata, test_xdata)
ydata <- rbind(train_ydata, test_ydata)
subdata <- rbind(train_subdata, test_subdata)

#parse feature text file to get means and stdev variable names and col indices
features_file <- './UCI HAR Dataset/features.txt'
featurelist <- read.table(features_file, colClasses=c('integer', 'character'))
all_var_names <- featurelist[,2]

#sort out the desired indices of mean and st. dev. variables
meanvar_bool <- grepl('mean', all_var_names)
stdvar_bool <- grepl('std', all_var_names)
meanFreqvar_bool <- grepl('meanFreq', all_var_names)
desired_var_ind <- which((meanvar_bool | stdvar_bool) & !meanFreqvar_bool)

#filter out unneeded columns and assign nice names to x data
meanstd_xdata <- xdata[,desired_var_ind]
names(meanstd_xdata) <- all_var_names[desired_var_ind]

#make y data (activity) and subjects into nicely formatted factor variables
ydata <- rename(ydata, activity = V1)
subdata <- rename(subdata, subject = V1)

activity_labels <- c('walking', 'walking up stairs', 'walking down stairs',
					 'sitting', 'standing', 'lying')
ydata$activity <- factor(ydata$activity, 1:6, activity_labels)
subdata$subject <- factor(subdata$subject, 1:30)

#combine all data into one big happy data frame
meanstd_data <- cbind(subdata, ydata, meanstd_xdata)

#write first tidy data set to file
write.table(meanstd_data, file = './accel_meanstd_data.txt', row.name=F)


#make new data set of averages per subject and activity
avg_meanstd_data <- data.frame()
parameter <- character()
parameter_value <- character()

allacts <- levels(meanstd_data$activity)
allsubs <- levels(meanstd_data$subject)

#get averages for each activity
for (i in seq(along = allacts)){
	parameter <- c(parameter, 'activity')
	parameter_value <- c(parameter_value, allacts[i])
	crnt_means <- colMeans(meanstd_xdata[meanstd_data$activity == allacts[i],])
	avg_meanstd_data <- rbind(avg_meanstd_data, crnt_means)
}

#get averages for each subject
for (i in seq(along = allsubs)){
	parameter <- c(parameter, 'subject')
	parameter_value <- c(parameter_value, allsubs[i])
	crnt_means <- colMeans(meanstd_xdata[meanstd_data$subject == allsubs[i],])
	avg_meanstd_data <- rbind(avg_meanstd_data, crnt_means)
}

#add var names and descriptive columns to data frame
names(avg_meanstd_data) <- paste('AVG', names(meanstd_xdata), sep = '-')
avg_meanstd_data <- cbind(parameter, parameter_value, avg_meanstd_data)

#write second tidy data set to file
write.table(avg_meanstd_data, file = './accel_avg_meanstd_data.txt', row.name=F)