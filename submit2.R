
library(mice)
#simple = train[c("Q98059","Q98078","Q98197","Q96024")]
train = read.csv("train2018.csv",na.strings = "")
#test = read.csv("test2016.csv",na.strings = "")
summary(train)
# vars.for.imputation = setdiff(names(train), c("Party","USER_ID","HouseholdStatus","EducationLevel","Income"))
# imputed = complete(mice(train[vars.for.imputation]))
# train[vars.for.imputation] = imputed

################################################################################################
table(train$HouseholdStatus)
train$EducationLevel.o <- ordered(train$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
#train$EducationLevel.f = factor(train$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
train$Income.o <- ordered(train$Income, levels = c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000","$100,001 - $150,000","over $150,000"))

vars.for.imputation = setdiff(names(train), c("Party","USER_ID","HouseholdStatus","EducationLevel","Income","EducationLevel.o","Income.o"))
imputed2 = complete(mice(train[vars.for.imputation])) 

spl = sample.split(train$Party, SplitRatio = .7)
ptrain = subset(train,spl == TRUE)
ptest = subset(train, spl == FALSE)


###########################################################################################

simple = train[c("HouseholdStatus","EducationLevel.o","Income.o")]
imputed = complete(mice(simple))

traindup = train
traindup$HouseholdStatus = imputed$HouseholdStatus
traindup$Income = imputed$Income.o
traindup$EducationLevel = imputed$EducationLevel.o
traindup[vars.for.imputation] = imputed2

summary(traindup)
write.csv(traindup)
?write.csv

simple = train[c("Q98059","Q98078","Q98197","Q96024")]
summary(simple)
imputed = complete(mice(simple))

#############################
merge
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


?setdiff
