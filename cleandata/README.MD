###Script to clean and format phone accelerometer data

This repository contains a a single R script "run\_analysis.R".

The script does the following:

1. Loads specified raw data into R data frames (data used can be found here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
2. Combine the test and train data sets into one larger data set
3. Parse out unneeded variables, only the mean and standard deviation of each variable is retained here
4. Cleans and formats data such that factor variables are properly labeled and all columns of data set have descriptive names
5. Creates a smaller data set that contains the averages for each subject and activity for the desired variables.
6. Saves both the larger and smaller (just averages) data sets to text files that can easily be loaded into R.

Also included in this repository is "CodeBook.md" which contains more details about the data outputted by the script.