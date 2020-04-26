# This code:  
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)

# Reading the label data 
activity_labels <- read.table("./activity_labels.txt")
colnames(activity_labels) <- c("ActivityN", "Activity")

features <- read.table("./features.txt")

# Read and rename subject data varbiable 
subject_test <- read.table("./test/subject_test.txt")
subject_train <- read.table("./train/subject_train.txt")
subject <- rbind(subject_test, subject_train)
colnames(subject) <- c("Subject")

# features_info <- read.table("./features_info.txt")

testlabels <- read.table("./test/y_test.txt")
trainlabels <- read.table("./train/y_train.txt")
labels <- rbind(testlabels, trainlabels)
colnames(labels) <- c("ActivityN")

# Reading the datasets
testset <- read.table("./test/X_test.txt")
trainset <- read.table("./train/X_train.txt")

### Merging of the training and the test sets 

# Save features (new variable names) to vector
f1 <- features[,2]

dataset <- rbind(testset, trainset)
colnames(dataset) <- f1

### Extract the variables mean and standard deviation 

means <- grepl("mean", f1)
stds <- grepl("std", f1)

# This variable has TRUE values for the columns we want to keep in the final dataset
trues <- xor(means, stds)
subdata <- dataset[trues]

### Add descriptive activity names

new_1 <- data.frame(subdata, subject)
new_2 <- data.frame(new_1, labels)

finaldata <- merge(new_2, activity_labels, by.x = "ActivityN", by.y = "ActivityN", all.x=TRUE) 
#fin_sub <- finaldata[which(finaldata$ID < 11),]

### Create second dataset with average of each variable (n=78) for each activity (n=6) and each subject (n=30)
averages <- finaldata %>% group_by(Subject, Activity) %>% summarise_if(is.numeric, mean)

# Sort data by subject and activity
averages[with(averages, order(-Subject,ActivityN)), ]

write.table(averages, file= "tidy_Dataset.txt", row.names= FALSE)
