train.data = read.csv("train.csv",na.strings = c("NA",""))
train.data$Survived = factor(train.data$Survived)
train.data$Pclass = factor(train.data$Pclass)

# To Check the percentage of missing values
#install.packages("Amelia")
require(Amelia)
missmap(train.data, main = "Missing Map")

#Imputing missing values
table(train.data$Embarked, useNA = "always")
train.data$Embarked[which(is.na(train.data$Embarked))] = "S"
table(train.data$Embarked, useNA = "always")

train.data$Name = as.character(train.data$Name)
table_words = table(unlist(strsplit(train.data$Name, "\\s+")))
sort(table_words[grep('\\.',names(table_words))],decreasing = TRUE)

#install.packages("stringr")
library(stringr)
tb = cbind(train.data$Age, str_match(train.data$Name, " [a-zA-Z]+\\."))
table(tb[is.na(tb[,1]),2])

mean.mr = mean(train.data$Age[grepl(" Mr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.mrs = mean(train.data$Age[grepl(" Mrs\\.", train.data$Name) & !is.na(train.data$Age)])
mean.dr = mean(train.data$Age[grepl(" Dr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.miss = mean(train.data$Age[grepl(" Miss\\.", train.data$Name) & !is.na(train.data$Age)])
mean.master =  mean(train.data$Age[grepl(" Master\\.", train.data$Name) & !is.na(train.data$Age)])

train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] = mean.mr
train.data$Age[grepl(" Mrs\\.", train.data$Name) & is.na(train.data$Age)] = mean.mrs
train.data$Age[grepl(" Dr\\.", train.data$Name) & is.na(train.data$Age)] = mean.dr
train.data$Age[grepl(" Miss\\.", train.data$Name) & is.na(train.data$Age)] = mean.miss
train.data$Age[grepl(" Master\\.", train.data$Name) & is.na(train.data$Age)] = mean.master

#Exploraing and visualizing data
barplot(table(train.data$Survived), main="Passenger Survival",  names= c("Perished", "Survived"))
barplot(table(train.data$Pclass), main="Passenger Class",  names= c("first", "second", "third"))
barplot(table(train.data$Sex), main="Passenger Gender")
hist(train.data$Age, main="Passenger Age", xlab = "Age")
barplot(table(train.data$SibSp), main="Passenger Siblings")
barplot(table(train.data$Parch), main="Passenger Parch")
hist(train.data$Fare, main="Passenger Fare", xlab = "Fare")
barplot(table(train.data$Embarked), main="Port of Embarkation")

counts <- table( train.data$Survived, train.data$Sex)
barplot(counts,  col=c("darkblue","red"), legend = c("Perished", "Survived"), main = "Passenger Survival by Sex")
counts <- table( train.data$Survived, train.data$Pclass)
barplot(counts,  col=c("darkblue","red"), legend =c("Perished", "Survived"), main= "Titanic Class Bar Plot" )
counts <- table( train.data$Sex, train.data$Pclass)
barplot(counts,  col=c("darkblue","red"), legend = rownames(counts), main= "Passenger Gender by Class")
hist(train.data$Age[which(train.data$Survived == "0")], main= "Passenger Age Histogram", xlab="Age", ylab="Count", col ="blue", breaks=seq(0,80,by=2))
hist(train.data$Age[which(train.data$Survived == "1")], col ="red", add = T, breaks=seq(0,80,by=2))
boxplot(train.data$Age ~ train.data$Survived, 
        main="Passenger Survival by Age",
        xlab="Survived", ylab="Age")
rain.child = train.data$Survived[train.data$Age < 13]
length(train.child[which(train.child == 1)] ) / length(train.child)
train.youth = train.data$Survived[train.data$Age >= 15 & train.data$Age < 25]
length(train.youth[which(train.youth == 1)] ) / length(train.youth)
train.adult  = train.data$Survived[train.data$Age >= 20 & train.data$Age < 65]
length(train.adult[which(train.adult == 1)] ) / length(train.adult)
train.senior  = train.data$Survived[train.data$Age >= 65]
length(train.senior[which(train.senior == 1)] ) / length(train.senior)

mosaicplot(train.data$Pclass ~ train.data$Survived,  main="Passenger Survival Class", color=TRUE,  
           xlab="Pclass", ylab="Survived")
