library(caTools)
library(randomForest)
library(ROCR)

# ---- use the ones with NA's
showTrain = read.csv("train2016.csv",na.strings=c("", "NA", "NULL"))  
showTest = read.csv("test2016.csv",na.strings=c("", "NA", "NULL"))

summary(showTrain)

showTrain$YOB = as.integer(as.character(showTrain$YOB))
showTest$YOB = as.integer(as.character(showTest$YOB))

showTrain$EducationLevel.o <- ordered(showTrain$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
showTrain$Income.o <- ordered(showTrain$Income, levels = c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000","$100,001 - $150,000","over $150,000"))
showTrain$EducationLevel = showTrain$EducationLevel.o 
showTrain$Income = showTrain$Income.o 
showTrain$EducationLevel.o =  NULL
showTrain$Income.o = NULL

showTest$EducationLevel.o <- ordered(showTest$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
showTest$Income.o <- ordered(showTest$Income, levels = c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000","$100,001 - $150,000","over $150,000"))
showTest$EducationLevel = showTest$EducationLevel.o 
showTest$Income = showTest$Income.o 
showTest$EducationLevel.o =  NULL
showTest$Income.o = NULL




summary(showTrain)
showTrain$YOB[showTrain$YOB < 1910] = NA
showTrain$YOB[showTrain$YOB > 2010] = NA
showTest$YOB[showTest$YOB < 1910] = NA
showTest$YOB[showTest$YOB > 2010] = NA

set.seed(666)
showTrain$IsRep = ifelse(showTrain$Party=="Republican",1,0) 
showTrain$Party = NULL
####### IMPUTATION########################################################################
vars.for.imputation = c("YOB","Gender","Income","HouseholdStatus","EducationLevel")
imputedtr = complete(mice(showTrain[vars.for.imputation],MaxNWts = 2000)) 
imputed = complete(mice(showTest[vars.for.imputation],MaxNWts = 2000)) 

# Assign imputed values to actual DF
showTrain$YOB = imputedtr$YOB
showTrain$Gender = imputedtr$Gender
showTrain$HouseholdStatus = imputedtr$HouseholdStatus
showTrain$Income = imputedtr$Income
showTrain$EducationLevel = imputedtr$EducationLevel

showTest$YOB = imputed$YOB
showTest$Gender = imputed$Gender
showTest$HouseholdStatus = imputed$HouseholdStatus
showTest$Income = imputed$Income
showTest$EducationLevel = imputed$EducationLevel


spl = sample.split(showTrain$IsRep, 0.75)
train = subset(showTrain, spl == TRUE)
test = subset(showTrain, spl == FALSE)

modelLog = glm(IsRep ~. - USER_ID, data=train, family="binomial", na.action=na.roughfix)
predictLog = predict(modelLog, newdata=test, na.action=na.roughfix)
table(test$IsRep, predictLog > 0.5)

#predictLog = predict(modelLog, newdata=test, type="response", na.action=na.roughfix)
predictLog = predict(modelLog, newdata=showTest,na.action=na.roughfix)
submission1 = data.frame(USER_ID = showTest$USER_ID, Predictions = as.factor(ifelse(predictLog >.5 ,"Republican","Democrat")))
write.csv(submission1, "submission_log1.csv", row.names=FALSE) 
str(submission)
table(predictLog)


## ------------------- RANDOM FORESTS  -------------------------
mrf = randomForest(IsRep ~. - USER_ID, data = train, ntree=200, nodesize=25 )
PredictForest = predict(mrf, newdata = test)
table(test$IsRep, PredictForest)
summary(test)

## ------ CART  --------------------------------------------
library(rpart)
library(rpart.plot)
dTree = rpart(IsRep ~. - USER_ID, data = train,  method="class", na.action=na.roughfix)

prp(dTree)
PredictTest = predict(dTree, newdata = test, type = "class")
table(test$IsRep, PredictTest)

(551+283)/(551+283+187+371)

+