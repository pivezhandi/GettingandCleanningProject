####################### here is loading stage for data base ###########
setwd(file.path("D:", "sbu", "RLearning", "Getting and Cleaning Data",
                "programming"))

#here is different directories for all datasets
################################ part1 ################################
#all files in directory that should be considered 
#1- features.txt
features<-read.table(file.path("D:", "sbu", "RLearning","Getting and Cleaning Data",
                               "programming", "UCI HAR Dataset", "features.txt"),col.names=c("ID2","feature"))
#2- subject_train.txt
subject_train<-read.table(file.path("D:", "sbu", "RLearning",
"Getting and Cleaning Data","programming", "UCI HAR Dataset", "train", "subject_train.txt"))
#3- X_train.txt
X_train<-read.table(file.path("D:", "sbu", "RLearning",
"Getting and Cleaning Data","programming", "UCI HAR Dataset", "train", "X_train.txt"))
#4- y_train.txt
y_train<-read.table(file.path("D:", "sbu", "RLearning",
"Getting and Cleaning Data","programming", "UCI HAR Dataset", "train", "y_train.txt"))
#5- subject_test.txt
subject_test<-read.table(file.path("D:", "sbu", "RLearning",
"Getting and Cleaning Data","programming", "UCI HAR Dataset", "test","subject_test.txt"))
#6- X_test.txt
X_test<-read.table(file.path("D:", "sbu", "RLearning",
"Getting and Cleaning Data","programming", "UCI HAR Dataset", "test","X_test.txt"))
#7- y_test.txt
y_test<-read.table(file.path("D:", "sbu", "RLearning",
"Getting and Cleaning Data","programming", "UCI HAR Dataset", "test","y_test.txt"))
#8- activity_labels.txt
activity_labels<-read.table(file.path("D:", "sbu", "RLearning","Getting and Cleaning Data",
"programming","UCI HAR Dataset","activity_labels.txt"), col.names=c("ID1","activity"))

#in this part of assignment trainning and test sets should
#  be merged together
TrainValues<-cbind(subject_train,X_train,y_train)
TestValues<-cbind(subject_test,X_test,y_test)
subject<-rbind(TrainValues,TestValues)

############################## part2 #################################
##grep function return all mean and std features in feature set
MeanMeasurment<-features$feature[grep("mean",tolower(as.character(features$feature)))]
StdMeasurment<-features$feature[grep("std",tolower(as.character(features$feature)))]
allfeatures<-grep("mean|std",tolower(as.character(features$feature)))

############################## part3 #################################
# all column in train and test sets renamed by col.names parameter
# in read.table function
#2- subject_train.txt
subject_train<-read.table(file.path("D:", "sbu", "RLearning", "Getting and Cleaning Data",
"programming", "UCI HAR Dataset", "train", "subject_train.txt"), col.names="subject")
#3- X_train.txt
X_train<-read.table(file.path("D:", "sbu", "RLearning","Getting and Cleaning Data",
"programming", "UCI HAR Dataset", "train", "X_train.txt"), col.names=features$feature)
#4- y_train.txt
y_train<-read.table(file.path("D:", "sbu", "RLearning","Getting and Cleaning Data",
"programming", "UCI HAR Dataset", "train", "y_train.txt"), col.names="labels")
#5- subject_test.txt
subject_test<-read.table(file.path("D:", "sbu", "RLearning", "Getting and Cleaning Data",
"programming", "UCI HAR Dataset", "test","subject_test.txt"), col.names="subject")
#6- X_test.txt
X_test<-read.table(file.path("D:", "sbu", "RLearning", "Getting and Cleaning Data",
"programming", "UCI HAR Dataset", "test","X_test.txt"), col.names=features$feature)
#7- y_test.txt
y_test<-read.table(file.path("D:", "sbu", "RLearning", "Getting and Cleaning Data",
"programming", "UCI HAR Dataset", "test","y_test.txt"), col.names="labels")
# renamed files modified as bellow

RenamedTrain<-cbind(subject_train,y_train,X_train[,allfeatures])
RenamedTest<-cbind(subject_test,y_test,X_test[,allfeatures])
Renamedsubject<-rbind(RenamedTrain,RenamedTest)

############################### part4 #################################
# all feature names modified as bellow
RepairedNames1<-gsub("\\(\\)","",as.character(names(Renamedsubject)))
RepairedNames2<-gsub("\\.","",RepairedNames1)
RepairedNames3<-gsub("[0-9]","",RepairedNames2)
names(Renamedsubject)<-RepairedNames3

################################ part5 ################################
# all features average dependent on subject cases 
UniqueSubject<-unique(Renamedsubject$subject)
UniqueActivity<-length(unique(Renamedsubject$label))
SubjectFeatureMeans<-matrix(rep(0,
                length(UniqueSubject)*UniqueActivity*(ncol(Renamedsubject))),
                (length(UniqueSubject)*UniqueActivity),(ncol(Renamedsubject)))
for (i in 1:length(UniqueSubject)){
        for (j in 1:UniqueActivity){
        SubjectFeatureMeans[(((i-1)*UniqueActivity)+j),]<-
                c(i,tolower(as.character(activity_labels$activity[j])),
                sapply(Renamedsubject[(Renamedsubject$subject== 
                UniqueSubject[i])&(Renamedsubject$label==j),(3:ncol(Renamedsubject))],mean))
}
}

####################### writting modified tidy data ###################
SubjectFeatureMeans<-data.frame(SubjectFeatureMeans)
names(SubjectFeatureMeans)<-RepairedNames3

## the tidy data will round to 7 points
SubjectFeatureMeans2<-SubjectFeatureMeans
for (i in 3:ncol(SubjectFeatureMeans)){
        SubjectFeatureMeans2[,i] <- round(data.frame(as.numeric(as.matrix(SubjectFeatureMeans[,i]))),7)
}
write.table(SubjectFeatureMeans2,file="SubjectFeatureMeans.txt", row.name=FALSE)
test<-read.table(file.path("D:", "sbu", "RLearning","Getting and Cleaning Data",
                           "programming", "UCI HAR Dataset", "new2.txt"))

head(test)
head(SubjectFeatureMeans)
