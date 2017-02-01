library(mice)
test = read.csv("test2016.csv",na.strings = c("","NA"))
test$EducationLevel.o <- ordered(test$EducationLevel, levels = c("Current K-12", "High School Diploma", "Current Undergraduate", "Bachelor's Degree","Master's Degree","Doctoral Degree"))
test$Income.o <- ordered(test$Income, levels = c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000","$100,001 - $150,000","over $150,000"))

# ---------------------FOR TRAIN DATA ONLY-------------------------------------
###test$IsRep = ifelse(test$Party=="Republican",1,0) 
###test$Isrepublican = as.factor(test$IsRep==1)
test$HouseholdStatus = as.factor(test$HouseholdStatus)
summary(test)
# --------------------------------------------------------------------------------
testvars.for.imputation = setdiff(names(test), c("Party","USER_ID","EducationLevel","Income"))    ## ,"HouseholdStatus"
imputedt2 = complete(mice(test[testvars.for.imputation],MaxNWts = 2000)) 
imputedt3 = imputedt2


# --------------------------------------------------------------------------------
testdup = test
testdup$HouseholdStatus = imputedt3$HouseholdStatus
testdup$Income = imputedt3$Income.o
testdup$EducationLevel = imputedt3$EducationLevel.o

testdup$EducationLevel.o = NULL
testdup$Income.o = NULL
## test = subset(traindup3, YOB != "NA")  -- this is to be used for traindata only
testdup$YOB = as.numeric(testdup$YOB)

PredictTest = predict(dTree, newdata = testdup, type = "class")

######################################## SUBMISSION ############################################

submission = data.frame(USER_ID = testdup$USER_ID, Predictions = as.factor(ifelse(PredictTest == TRUE,"Republican","Democrat")))
write.csv(submission, "submission.csv", row.names=FALSE) 


