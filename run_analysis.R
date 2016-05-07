library(dplyr)
# part 1
features<-as.character(read.table('UCI HAR Dataset/features.txt')[,2])
xtest<-read.table('UCI HAR Dataset/test/X_test.txt',col.names=features)
subjtest<-read.table('UCI HAR Dataset/test/subject_test.txt',col.names='subject')
ytest<-read.table('UCI HAR Dataset/test/y_test.txt',col.names='activity')
xtrain<-read.table('UCI HAR Dataset/train/X_train.txt',col.names=features)
subjtrain<-read.table('UCI HAR Dataset/train/subject_train.txt',col.names='subject')
ytrain<-read.table('UCI HAR Dataset/train/y_train.txt',col.names='activity')
actlab<-read.table('UCI HAR Dataset/activity_labels.txt',col.names=c('activity_code','activity_desc'))
data_train<-cbind(xtrain,subjtrain,ytrain)
data_test<-cbind(xtest,subjtest,ytest)
combdata<-rbind(data_train,data_test)

# part 2
mean_std<-grepl('.mean',names(combdata))&!grepl('.meanFreq',names(combdata))|grepl('.std',names(combdata))
sel_col<-c(1:length(names(combdata)))[mean_std]
data<-select(combdata,sel_col,subject,activity)

# part 3
data<-mutate(data,activity=actlab[match(activity,actlab$activity_code),2])

#part 4
write.table(names(data),'names.txt',row.name=FALSE)
# at this point changes were made separately in the .txt file which was renamed "names1.txt" upon saving
names<-read.table('names1.txt')
names<-names[2:69,1]
colnames(data) <- make.names(names)

#part 5
group<-group_by(data,subject,activity)
data2<-summarise_each(group,funs(mean),1:68)
write.table(data2,'dataset.txt',row.name=FALSE)  
