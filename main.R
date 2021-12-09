getwd()

# считываем данные и названия колонок
dataset<- read.csv("Objects.csv",header= FALSE, sep=";")
dat<- read.csv("names.csv",header= FALSE)
names <- dat[, 1]
colnames(dataset) <- names

# Выведем первые 6 и последнии с dataseta

head(dataset, 6)
tail(dataset, 6)

# Выведем dataset
# dataset

#посчитаем количество 0 и 1 в target

target=readLines("target.csv")
count <- 0

for(x in target){
  if(x==1)
  {
    count=count+1
  }

}
print(count, "0") #считаем кол-во 1
print(length(target)-count,"1") #считаем кол-во 0

#task 2

print(names(dataset)[9])#check if dataset[9] is WorkSectorID
#output dataset[9]
#dataset[, 9]
nancount=0
for (i in dataset[, 9])
{
  if(is.nan(i))
  {
    nancount=nancount+1
  }

}
print(nancount) #считаем nan


dataset2 <- cbind(dataset)   #copy dataset in dataset2
#dataset2 

dataset[is.na(dataset)] = -666
dataset[, 9]

# print(names(dataset)[38])  HowLongLivesHere
# print(names(dataset)[39])  HowLongWorksHere

# dataset[, 38]
# dataset[, 39]


for (i in dataset[, 38])
{
  if(i<0)
  {
    print(i)
    
  }
  
}


dataset[, 38][dataset[, 38] == -26] <- 0  #replace -26 with 0 in column 38

dataset[, 48]<-as.numeric(gsub(",", ".", gsub("\\.", "", dataset[, 48])))   #replace coma with dot

dataset[, 48] <- round(dataset[, 48], digits = 2)   #set digits to 2

dataset[, 49]<-as.numeric(gsub(",", ".", gsub("\\.", "", dataset[, 49])))   #replace coma with dot

dataset[, 49] <- round(dataset[, 49], digits = 2)   #set digits to 2

#task 3

dataID<- data.frame(dataset[,20],dataset[,14])
colnames(dataID) <- c( "BranchRegionID","FamilyRevenueID")


plotdata<-dataID[order(dataID$BranchRegionID),]


library(plyr)
#form new count dataset that contains BranchRegionID, FamilyRevenueID, Freq= number of clients with revenue in bank
counts <- ddply(plotdata, .(plotdata$BranchRegionID, plotdata$FamilyRevenueID), nrow)
names(counts) <- c("BranchRegionID,", "FamilyRevenueID", "Freq")

counts[counts=="-666"]<--1
counts
#create unique names vector 
names<- unique(counts[,1])
names 

library(ggplot2)

df=data.frame(BranchRegionID=c(counts[,1]),FamilyRevenueID=c(counts[,2]), Freq=c(counts[,3]) )

p<-ggplot(df,aes(x=BranchRegionID,y=Freq,fill=FamilyRevenueID)) + geom_bar(position="stack", stat="identity")+scale_x_continuous(breaks=seq(names[1], names[length(names)], 1))
p

#task 4
#поменяем запятые на точки в неоходимой колонке
dataset[, 15]<-as.numeric(gsub(",", ".", gsub("\\.", "", dataset[, 15]))) 
dataset[,15]


clsdata<-data.frame(as.numeric(target),dataset[,2], dataset[,14],dataset[,15],dataset[,33])

colnames(clsdata) <- c("target","Works", "FamilyRevenueID","Revenue","LastCreditSum")
clsdata

clsdata<-clsdata[1:1000,-15]
set.seed(1234)
#разбиваем 70% для обучения , 30 %
id<-sample(2,nrow(clsdata),prob=c(0.7,0.3),replace=T)
clstrain<-clsdata[id==1,]
clstest<-clsdata[id==2,]

library(e1071)
library(caret)
str(clstrain)
#using naiveBayes to train model 
cls_nb<-naiveBayes(target~ Revenue+Works+FamilyRevenueID+LastCreditSum,data=clstrain)
cls_nb

predictcls<-predict(cls_nb,clstest)
predictcls
#check the results of learning 
confusionMatrix(table(predictcls,clstest$target))



