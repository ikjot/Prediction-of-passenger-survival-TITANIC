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


