train.data = read.csv("train.csv",na.strings = c("NA",""))
train.data$Survived = factor(train.data$Survived)
train.data$Pclass = factor(train.data$Pclass)
