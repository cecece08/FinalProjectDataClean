########## *********** Please put it into the directory called "UCI UAR Dataset" to run
## 1. Merges the training and the test sets to create one data set.
#read data from file including data set(***data) and activity label (***label)
testdata <- read.table("./test/X_test.txt")
traindata <- read.table("./train/X_train.txt")
testlabel <- read.table("./test/y_test.txt")
trainlabel <- read.table("./train/y_train.txt")
#merge data set (train and test) together
alllabel <- rbind(testlabel,trainlabel)
alldata <- rbind(testdata,traindata)
## 2.Extracts only the measurements on the mean and standard deviation for each measurement.
#read name of variables and get required variables (mean and std)
features <- read.table("./features.txt")
mean_index <- grep("mean",features$V2, ignore.case = TRUE)
deviation_index <- grep("std",features$V2, ignore.case = TRUE)
index <- append(mean_index, deviation_index)
#get new data set with only required variables (std and mean)
subindex <- !is.na(match(1:length(alldata),index))
newset <- subset.data.frame(alldata,select=subindex)
newfeatures <- features[subindex,]
## 3.Uses descriptive activity names to name the activities in the data set
#read name of activities and rename the activities
activity <- read.table("./activity_labels.txt")
named_act<-merge(alllabel,activity,by.x="V1",by.y="V1")
activities <- named_act$V2
## 4.Appropriately labels the data set with descriptive variable names.
colnames(newset) <- newfeatures$V2
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#read data
testsub <- read.table("./test/subject_test.txt")
trainsub <- read.table("./train/subject_train.txt")
#merge all together including subject and activity
subject <- rbind(testsub,trainsub)$V1
#reshape into desired table
n=0
for(i in 1:30){
  for(j in activity$V1) {
    ind=which(subject==i & named_act$V1==j)
    if(length(ind)==0) {
      next
    }
    n=n+1
    subjects=i
    activities=j
    variable<- colMeans(newset[ind,])
    mean_sub <- cbind(subjects,activities)
    for(k in 1:length(variable)) {#convert row to column
      var <- variable[k]
      mean_sub <-cbind(mean_sub,var)
      colnames(mean_sub)[2+k] <- names(var)
      rownames(mean_sub)=NULL
    }
    if(n==1){
      mean_all=mean_sub
    } else {
    mean_all=rbind(mean_all,mean_sub)
    }
  }
}
mean_all=data.frame(mean_all)
named_act<-merge(mean_all$activities,activity,by.y="V1")
mean_all$activities <- named_act$V2
#save into txt file
write.table(mean_all,file="./meansdata_tidy.txt",row.name=FALSE)
