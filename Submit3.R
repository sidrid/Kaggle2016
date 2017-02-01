
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
train$Income.o <- ordered(train$YOB, levels = c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000","$100,001 - $150,000","over $150,000"))
train$YOB = as.numeric(train$YOB)
vars.for.imputation = setdiff(names(train), c("Party","USER_ID","HouseholdStatus","EducationLevel","Income","EducationLevel.o","Income.o"))
imputed2 = complete(mice(train[vars.for.imputation])) 



###########################################################################################

simple = train[c("YOB", "HouseholdStatus","EducationLevel.o","Income.o")]
imputed = complete(mice(simple))
summary(imputed)
traindup = train
traindup$HouseholdStatus = imputed$HouseholdStatus
traindup$Income = imputed$Income.o
traindup$EducationLevel = imputed$EducationLevel.o
traindup[vars.for.imputation] = imputed2
traindup$YOB = as.numeric(traindup$YOB)
summary(traindup)
write.table(traindup,file = "ptrain.csv",sep = ",")
?write.csv

######################################################################################################################

traindup3$EducationLevel.o = NULL
traindup3$Income.o = NULL
summary(traindup3)

traindup4 = subset(traindup3, YOB != "NA")
traindup4$YOB = as.numeric(traindup4$YOB)
traindup4$IsRep = ifelse(traindup4$Party=="Republican",1,0) 

traindup5 = traindup4
traindup5$Party=NULL
nrow(traindup3)
nrow(traindup4)

############################################ LOGIT MODELS  ##############################
library(caTools)

spl = sample.split(traindup4$Party, SplitRatio = .7)
mtrain = subset(traindup4,spl == TRUE)
mtest = subset(traindup4, spl == FALSE)
nrow(mtrain)
nrow(mtest)
mlog = glm(Party ~ . - USER_ID, data = mtrain, family = binomial)
summary(mlog)

## with traindup5
spl2 = sample.split(traindup5$IsRep, SplitRatio = .7)
mtrain2 = subset(traindup5,spl2 == TRUE)
mtest2 = subset(traindup5, spl2 == FALSE)
mlog2 = glm(IsRep ~. - USER_ID, data = mtrain2, family = binomial)
predictTest2 = predict(mlog2, type="response", newdata=mtest2)
table(mtest2$IsRep,predictTest2>.5)

############################### PREDICT  ################################################

predictTest = predict(mlog, type="response", newdata=mtest)
table(mtest$Party,predictTest)

#################################### RANDOM FORESTS  ######################################

install.packages("randomForest")
library(randomForest)
mrf = randomForest(IsRep ~. - USER_ID, data = mtrain2, ntree=200, nodesize=25 )
PredictForest = predict(mrf, newdata = mtest2)
table(mtest2$IsRep, PredictForest)

traindup6 = traindup5
traindup6$Isrepublican = as.factor(traindup6$IsRep==1)

summary(traindup6)
traindup6$IsRep = NULL
spl3 = sample.split(traindup6$Isrepublican ,SplitRatio = .7)
mtrain3 = subset(traindup6,spl3 == TRUE)
mtest3 = subset(traindup6, spl3 == FALSE)


mrf2 = randomForest(Isrepublican ~. - USER_ID, data = mtrain3, ntree=200, nodesize=25 )
PredictForest2 = predict(mrf2, newdata = mtest3)
table(mtest3$Isrepublican, PredictForest2)

#################################### CART MODEL  ##################################################
library(rpart)
library(rpart.plot)
dTree = rpart(Isrepublican ~. - USER_ID, data = mtrain3,  method="class")

prp(dTree)
PredictTest = predict(dTree, newdata = mtest3, type = "class")
table(mtest3$Isrepublican, PredictTest)
summary()


###############################################

letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")