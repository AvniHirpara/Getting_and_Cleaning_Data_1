# This R script does the following:

# 1. Merges the training and the test sets to create one data set.
# Read the Activity files
dataActivityTest  <- read.table("test/Y_test.txt", header = FALSE)
dataActivityTrain <- read.table("train/Y_train.txt", header = FALSE)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)

# Read the Subject files
dataSubjectTrain <- read.table("train/subject_train.txt", header = FALSE)
dataSubjectTest  <- read.table("test/subject_test.txt", header = FALSE)
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)

# Read Fearures files
dataFeaturesTest  <- read.table("test/X_test.txt", header = FALSE)
dataFeaturesTrain <- read.table("train/X_train.txt", header = FALSE)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
dataFeatures <- dataFeatures[, indices_of_good_features]
names(dataFeatures) <- features[indices_of_good_features, 2]
names(dataFeatures) <- gsub("\\(|\\)", "", names(dataFeatures))
names(dataFeatures) <- tolower(names(dataFeatures))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
dataActivity[, 1] = activities[dataActivity[, 1], 2]
names(dataActivity) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(dataSubject) <- "subject"
cleanedData <- cbind(dataSubject, dataActivity, dataFeatures)
write.table(cleanedData, "merged_clean_data.txt", row.names = FALSE)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(dataSubject)[, 1]
numSubjects = length(uniqueSubjects)
numActivities = length(activities[, 1])
numCols = dim(cleanedData)[2]
result = cleanedData[1:(numSubjects * numActivities), ]

row = 1
# for every subject
for (s in 1:numSubjects) {
   # for every activity
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        # select currnet subject and activity data
        tmp <- cleanedData[cleanedData$subject==s & cleanedData$activity==activities[a, 2], ]
        # calculate average of data column
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row + 1
    }
}
write.table(result, "data_set_with_the_averages.txt", row.names = FALSE)
