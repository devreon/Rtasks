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

#Задание 2

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



dataID<- data.frame(dataset[,20],dataset[,14])
colnames(dataID) <- c( "BranchRegionID","FamilyRevenueID")


plotdata<-dataID[order(dataID$BranchRegionID),]


library(plyr)
counts <- ddply(plotdata, .(plotdata$BranchRegionID, plotdata$FamilyRevenueID), nrow)
names(counts) <- c("BranchRegionID,", "FamilyRevenueID", "Freq")

counts[counts=="-666"]<--1
counts

names<- unique(counts[,1])
names 
# barplot(
#   t(counts)[2:3,],
#   names.arg= t(counts)[1,],
#   space=c(0,diff(t(counts)[1,])),
#   axis.lty=1
# )

 
# # считаем сумму по ключу
# sumbykey<-with(dataID,tapply(dataID[,2],dataID[,1],sum))
# sumbykey
# 
library(ggplot2)

df=data.frame(BranchRegionID=c(counts[,1]),FamilyRevenueID=c(counts[,2]), Freq=c(counts[,3]) )

p<-ggplot(df,aes(x=BranchRegionID,y=Freq,fill=FamilyRevenueID)) + geom_bar(position="stack", stat="identity") 
p
