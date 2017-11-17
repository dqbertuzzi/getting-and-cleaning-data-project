library(dplyr)
library(stringr)
library(reshape2)

###------------
### 1 Merges the training and the test sets to create one data set.
###------------

# Importing the X_train and X_test set
dataset_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(dataset_train) # 7352 observations, 561 variable

dataset_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
dim(dataset_test) # 2947 observations, 561 variable

# Merging the train_set and test_set set into one dataset
dataset <- bind_rows(dataset_train, dataset_test)
dim(dataset) # 10299 observations (7352 + 2947 from the X_train and X_test), 563 variables (subjects +
# activities + features)

# Importing subjects train and test set
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Merging the subjects set into one dataset
subjects <- bind_rows(subject_train, subject_test)

# Importing activities train and test set
activities_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
activities_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Merging the activities set into one dataset
activities <- bind_rows(activities_train, activities_test)

###------------
### 2 Extracts only the measurements on the mean and standard deviation for each measurement.
###------------

# Importing activity and features labels
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2]) # Transforming into character type
featuresLabels <- read.table("./UCI HAR Dataset/features.txt")
featuresLabels[,2] <- as.character(featuresLabels[,2]) # Transforming into character type

# Extracting only the columns with the expression -mean() or -std()
extractedFeatures <- str_detect(featuresLabels[,2], "-(mean|std)\\(\\)")

# Creating a new dataset with the extracted features subsetting the original dataset
extractedDataset <- dataset[,extractedFeatures]


###------------
### 3 Uses descriptive activity names to name the activities in the data set.
###------------

# Labeling activities and turning it to lower case and into a data frame
activities <- activityLabels[activities[, ],2] %>% str_to_lower() %>% as.data.frame()

###------------
### 4 Appropriately labels the data set with descriptive variable names.
###------------

colnames(activities) <- "activity"
colnames(subjects) <- "subject"
colnames(extractedDataset) <- featuresLabels[,2][extractedFeatures]

# Merging all data frames (subjects, activities and extractedDataset) into one Data
data <- bind_cols(subjects, activities, extractedDataset)

###------------
### 5 From the data set in step 4, creates a second, independent tidy data set with the average
### of each variable for each activity and each subject.
###------------

meltedData <- melt(data, id = c("subject", "activity"))
avgData <- dcast(meltedData, subject + activity ~ variable, mean)

write.table(avgData, file = "./tidydata.txt", row.names = F)

