# Download and Unzip the file and it should be in the working directory. Then :
library(dplyr)
## step 1
## Merge the training and the test sets to create one data set.
# read all the data
test.labels <- read.table("test/y_test.txt", col.names="label")
test.subjects <- read.table("test/subject_test.txt", col.names="subject")
test.data <- read.table("test/X_test.txt")
train.labels <- read.table("train/y_train.txt", col.names="label")
train.subjects <- read.table("train/subject_train.txt", col.names="subject")
train.data <- read.table("train/X_train.txt")


data <- rbind(cbind(test.subjects, test.labels, test.data),
             cbind(train.subjects, train.labels, train.data))

## step 2
# Extract only the measurements on the mean and standard deviation for each measurement.
rtable <- read.table("features.txt", strip.white=TRUE, stringsAsFactors=FALSE)
rtable.mean.std <- rtable[grep("mean\\(\\)|std\\(\\)", rtable$V2), ]
data.mean.std <- data[, c(1, 2, rtable.mean.std$V1+2)]

## step 3
# Use descriptive activity names to name the activities in the data set
labels <- read.table("activity_labels.txt", stringsAsFactors=FALSE)
data.mean.std$label <- labels[data.mean.std$label, 2]

## step 4
# Appropriately label the data set with descriptive activity names.
final.colnames <- c("subject", "label", rtable.mean.std$V2)
final.colnames <- tolower(gsub("[^[:alpha:]]", "", final.colnames))
colnames(data.mean.std) <- final.colnames

## step 5
#Create a second, independent tidy data set with the average of each variable for each activity and each subject.

final.data <- aggregate(data.mean.std[, 3:ncol(data.mean.std)],by=list(subject = data.mean.std$subject, label = data.mean.std$label),mean)

write.table(format(final.data, scientific=T), "tidyData1.txt", row.names=F, col.names=F, quote=2)
