## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#setwd("/Users/hsinhua/Desktop/Coursera/Getting and Cleaning Data/Course-Project")

if (!require("data.table")) {
        install.packages("data.table")
}

if (!require("reshape2")) {
        install.packages("reshape2")
}

library("data.table")
library("reshape2")

## Let's first read the data in the test foler

## Import the subject IDs for tests
subject_testID <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Import the test results
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

## Import the activity test for each test subject labeled from 1 - 6 
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")[,1]

## Now in order to make tidy data, we need the "column names"
## The column names for test results are given in features.txt
## Import features as the column names for the test results data
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

## Now let's assign the "column names" to the test results
names(X_test) <- features

## We prefer the activity names instead of the number label
## We reexpress y_test in terms of actual activity names
## Import the activity label
Act_Label <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

## Nowe we have the activity labels as a list
## We rewrite y_test
y_test <- as.data.frame(Act_Label[y_test])

## Now we assign column names for subject_testID and y_test
names(subject_testID) <- "ID"
names(y_test) <- "Activity"

## Let's column combine the ID column, Activity column, and all feature columns

test_data <- cbind(subject_testID, y_test, X_test)

## Now we simply repeat the same procedue to construct the train_data

## Import the subject IDs for trains
subject_trainID <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## Import the training results
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

## Import the activity test for each train subject labeled from 1 - 6 
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")[,1]

## Now in order to make tidy data, we need the "column names"
## The column names for train results are given in features.txt

## Now let's assign the "column names" to the train results
names(X_train) <- features

## We prefer the activity names instead of the number label
## We rewrite y_train
y_train <- as.data.frame(Act_Label[y_train])

## Now we assign column names for subject_trainID and y_train
names(subject_trainID) <- "ID"
names(y_train) <- "Activity"

## Note that for train and test data have the same column names

## Let's column combine the ID column, Activity column, and all feature columns

train_data <- cbind(subject_trainID, y_train, X_train)

## Now let's row combine test_data and train data

data <- rbind(test_data, train_data)

## Now we select the columns we need for the course project
## and rename data
data <- data[, grep("mean|std|ID|Activity", names(data))]


## The tricky part is how to creates an independent tidy data set 
## with the "average of each variable for each activity and each subject
## The simplest way is to melt the data to form a long data frame
## with column ID, Activity, + other column names as the variable
## The we use dcast to obtain the data frame along with calculating the 
## average we want

datamelt <- melt(data, id = c("ID", "Activity"),
                 measure.vars = names(data)[-(1:2)])
tidy_data <- dcast(datamelt, ID + Activity ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt")
