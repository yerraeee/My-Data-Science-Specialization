width <-c()
width[1:561]<- 16


X_train <- read.fwf("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "",skip=0,widths=width,n=100)

subject_train <- read.fwf("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "",skip=0,widths=2,n=100)

y_train <- read.fwf("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "",skip=0,widths=1,n=100)

X_test <- read.fwf("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "",skip=0,widths=width,n=100)

subject_test <- read.fwf("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "",skip=0,widths=2,n=100)

y_test <- read.fwf("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "",skip=0,widths=1,n=100)

data <- rbind(cbind(y_train,subject_train,X_train),cbind(y_test,subject_test,X_test))
head(data$activity)

features <- read.delim("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/features.txt",header = FALSE,sep=" ")

activity_lbl <- read.delim("D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/UCI HAR Dataset/activity_labels.txt",header = FALSE,sep=" ")
head(activity_lbl)

names(data)<- c("activity","subject",as.vector(features$V2))

data$activity <- as.integer(data$activity)
activity_lbl$V1 <- as.integer(activity_lbl$V1)
for (i in 1:nrow(data)) {
  a <- activity_lbl[which(activity_lbl$V1 == data$activity[i]),]$V2
  data$activity[i] <- as.character(a)}

head(data$activity)
data$activity <- as.factor(data$activity)
dataMeanSD <- data[,c(1:2,grep("-mean",names(data)),grep("-std",names(data)))]

head(dataMeanSD)

library(psych)
Final_summary <- describeBy(dataMeanSD,group=list(dataMeanSD$activity,dataMeanSD$subject),mat=TRUE)

View(Final_summary)

write.table(Final_summary[,1:7],file="D:/hLearn/Coursera_DataScience Specialization_JhonHopkins University/My DSS/Getting and Cleaning Data Assignments/Final_summary.txt",sep=",")
