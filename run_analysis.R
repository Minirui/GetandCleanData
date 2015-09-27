##Loading needed packages:
library(doBy)


##Reading in  data files
test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = F)
testactivites <- read.table("./UCI HAR Dataset/test/y_test.txt", header = F)
testsubject <-read.table("./UCI HAR Dataset/test/subject_test.txt", header = F)
train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = F)
trainactivites <- read.table("./UCI HAR Dataset/train/y_train.txt", header = F)
trainsubject <-read.table("./UCI HAR Dataset/train/subject_train.txt", header = F)

features <- read.table("./UCI HAR Dataset/features.txt", header = F)
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = F)

##setting column names
colnames(test) <- features$V2
colnames(train) <- features$V2
colnames(testactivites) <- "activity"
colnames(testsubject) <- "volunter"
colnames(trainactivites) <- "activity"
colnames(trainsubject) <- "volunter"


#Adding activites and volunter number to the two data sets
Test <- cbind(test, testactivites, testsubject)
Train <- cbind(train, trainactivites, trainsubject) 

#Reducing the measurements to those that are mean and standard deviation of a measurement, along 
#with activity and volunter number 
tidy <- fullset[,c(grep("mean()", colnames(fullset)),grep("std()", colnames(fullset)),length(colnames(fullset))-1,length(colnames(fullset)))]
# Renaming the activities using factors and the activity labels. 
tidy$activity <- as.factor(tidy$activity)
levels(tidy$activity) <- activitylabels[,2]

#Modifying the column names so that these can be recognized as variable names by the summaryBy function  
colnames(tidy) <- gsub("-", "_",colnames(tidy), fixed = T)
colnames(tidy) <- gsub("()", "",colnames(tidy), fixed = T)

#Creating the tidy data containing avarages for all varaiables grouped by activity and volunter
tidy2 <- summaryBy(. ~ activity + volunter, data = tidy, FUN = mean )

write.table(tidy2, file = "tidy.txt", row.name=FALSE)

colnames(tidy2)
