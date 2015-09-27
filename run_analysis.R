## run_analysis.R - Course Project
## September, 2015 - Getting and Cleaning Data Course = Coursera

library(plyr)

# Read in the text files

subject_train <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/train/subject_train.txt")
subject_test <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/test/subject_test.txt")
y_train <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/train/y_train.txt")
x_train <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/train/x_train.txt")
y_test <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/test/y_test.txt")
x_test <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/test/x_test.txt")

## Read in the column names
features <- read.table("D:/Data Files/R Data/Coursera Data Science/Getting and Cleaning Data/dataset/features.txt")
features <- subset(features, select = -V1)

## Need to add Subject and Activity to the list of column names (features) before adding them
added_cols <- data.frame(c("Subject", "Activity"))
names(added_cols) <- names(features)
features_rev <- rbind(added_cols, features)

## Transpose features to row to add to dataset later
features_rev2 <- t(features_rev)

## Consolidate training files and add column names
train_full1 <- cbind(subject_train, y_train)
train_full2 <- cbind(train_full1, x_train)
colnames(train_full2) <- features_rev2

## Consolidate test files and add column names
test_full1 <- cbind(subject_test, y_test)
test_full2 <- cbind(test_full1, x_test)
colnames(test_full2) <- features_rev2

## Merge the datasets together into single dataset
complete_set <- rbind(train_full2, test_full2)

## Extract the measurements with "mean" and standard deviation ("std") in column names
dataset_reduced <- complete_set[ ,grep("mean|std", names(complete_set))]

## Generate numerical description of activities for calculation of means in tidy dataset
desc_num <- rbind(y_train,y_test)

## Add the Subject and Activity columns to the reduced dataset
subject_full <- rbind(subject_train,subject_test)
subj_act <- cbind(subject_full, desc_num)
names(subj_act) <- c("Subject", "Activity")
complete_desc <- cbind(subj_act, dataset_reduced)

## Sort reduced dataset before calculating means
desc_sort <- complete_desc[with (complete_desc, order(Subject, Activity)), ]

## Use ddply to sort data by subject and activity and calculate column means
tidy_dataset <- ddply(desc_sort, c("Subject", "Activity"), colMeans)

## Remove columns from tidy dataset to add back columns with descriptions of actvitiies
subject_tidy <- tidy_dataset[,1]
desc_tidy <- tidy_dataset[,2]
tidy_dataset <- tidy_dataset[ , -(1:2)]

## Substitute descriptive names to the activity
desc_tidy <- replace(desc_tidy, desc_tidy == 1, "walking")
desc_tidy <- replace(desc_tidy, desc_tidy == 2, "walking_up")
desc_tidy <- replace(desc_tidy, desc_tidy == 3, "walking_down")
desc_tidy <- replace(desc_tidy, desc_tidy == 4, "sitting")
desc_tidy <- replace(desc_tidy, desc_tidy == 5, "standing")
desc_tidy <- replace(desc_tidy, desc_tidy == 6, "laying")

## Add descriptive columns back to complete tidy dataset
tidy_desc <- cbind(subject_tidy, desc_tidy)
names(tidy_desc) <- c("Subject", "Activity")
tidy_dataset <- cbind(tidy_desc, tidy_dataset)

## Write tidy dataset to a text file for submission
write.table(tidy_dataset, file = "tidy_dataset.txt", row.names = FALSE)

## Provide illustrative output from the script
str(tidy_dataset)
