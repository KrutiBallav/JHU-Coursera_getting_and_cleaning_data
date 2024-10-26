# Load the required library for data manipulation
library(dplyr)

# Download the dataset if it doesn't exist
if (!file.exists("./DATA")) {
  dir.create("./DATA")  # Create a folder to store the data
}

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./DATA/projectdataset.zip")   # Download the dataset

# Unzip the dataset to the DATA folder
unzip(zipfile = "./DATA/projectdataset.zip", exdir = "./DATA")

# 1. Merging the training and test datasets

# Read the training data
x_train <- read.table("./DATA/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./DATA/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./DATA/UCI HAR Dataset/train/subject_train.txt")

# Read the test data
x_test <- read.table("./DATA/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./DATA/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./DATA/UCI HAR Dataset/test/subject_test.txt")

# Read the features (column names for the data)
features <- read.table("./DATA/UCI HAR Dataset/features.txt")

# Read the activity labels (activity names)
activityLabels <- read.table("./DATA/UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("activityID", "activityType")

# Assign proper column names to the training and test data
colnames(x_train) <- features[, 2]  # Using feature names for x_train
colnames(y_train) <- "activityID"  # Naming y_train column as activityID
colnames(subject_train) <- "subjectID"  # Naming subject_train as subjectID

colnames(x_test) <- features[, 2]  # Using feature names for x_test
colnames(y_test) <- "activityID"  # Naming y_test column as activityID
colnames(subject_test) <- "subjectID"  # Naming subject_test as subjectID

# 1. Combine the training data and test data
alltrain <- cbind(y_train, subject_train, x_train)  # Merging training labels, subjects, and features
alltest <- cbind(y_test, subject_test, x_test)  # Merging test labels, subjects, and features
finaldataset <- rbind(alltrain, alltest)  # Merging training and test datasets together

# 2. Extract only the measurements on the mean and standard deviation (std) for each variable
mean_and_std <- grepl("activityID|subjectID|mean\\(\\)|std\\(\\)", colnames(finaldataset))  # Identify columns with mean and std
setforMeanandStd <- finaldataset[, mean_and_std]  # Create a subset of data with mean and std only

# 3. Add descriptive activity names (e.g., "Walking", "Sitting") to the dataset
setWithActivityNames <- merge(setforMeanandStd, activityLabels, by = "activityID", all.x = TRUE)

# 4. Label the dataset with clearer, more descriptive variable names
colnames(setWithActivityNames) <- gsub("^t", "time", colnames(setWithActivityNames))  # Replace "t" with "time"
colnames(setWithActivityNames) <- gsub("^f", "frequency", colnames(setWithActivityNames))  # Replace "f" with "frequency"
colnames(setWithActivityNames) <- gsub("Acc", "Accelerometer", colnames(setWithActivityNames))  # Replace "Acc" with "Accelerometer"
colnames(setWithActivityNames) <- gsub("Gyro", "Gyroscope", colnames(setWithActivityNames))  # Replace "Gyro" with "Gyroscope"
colnames(setWithActivityNames) <- gsub("Mag", "Magnitude", colnames(setWithActivityNames))  # Replace "Mag" with "Magnitude"
colnames(setWithActivityNames) <- gsub("BodyBody", "Body", colnames(setWithActivityNames))  # Replace duplicate "Body" with one

# 5. Create a second, independent tidy dataset with the average of each variable for each activity and subject
tidySet <- setWithActivityNames %>%
  group_by(subjectID, activityID, activityType) %>%  # Group by subject and activity
  summarise_all(mean)  # Calculate the mean for each group

# Save the tidy dataset to a CSV file
write.csv(tidySet, "tidySet.csv", row.names = FALSE)

# Save the tidy dataset to a text file
write.table(tidySet, "tidySet.txt", row.names = FALSE)

# contact email :- krutiballavstark@gmail.com
# github profile page :- https://github.com/KrutiBallav