setwd("D:/LEARN/KAGGLE")

# install.packages("mice")

library(mice)
#simple = train[c("Q98059","Q98078","Q98197","Q96024")]
train = read.csv("train2016.csv",na.strings = "")
test = read.csv("test2016.csv",na.strings = "")
summary(train)
simple = train[c("Q98059","Q98078","Q98197","Q96024")]
summary(simple)
imputed = complete(mice(simple))

summary(imputed)
train$EducationLevel.o <- ordered(train$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
train$EducationLevel.f = factor(train$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
mdl = glm(Party ~ EducationLevel.o,data=train, family=binomial)
summary(mdl)
train$Party.r = ifelse(train$Party,"Republican")
str(train$EducationLevel.o)

install.packages(plyr)
library(plyr)

train$Gender2 = factor(train$Gender)
train$Gender2[train$Gender=="Male"] <- 0
train$Gender2[train$Gender=="Female"] <- 1
summary(train)


