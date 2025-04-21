# Unit 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)

prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)



# VIDEO 5 - Random Forests

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)


(40+74)/(40+37+19+74)
(43+78)/(43+34+15+78) #seed =100
(43+70)/(43+70+34+23)


# VIDEO 6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)
prp(StevensTreeCV)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

###ASSIGNMENT #$

gerber=read.csv("gerber.csv")
str(gerber)
table(gerber$voting)
108696/(108696+235388)

table((subset(gerber, voting=="1"))$hawthorne)
12316/(12316+96380)
#[1] 0.1133068

table((subset(gerber, voting=="1"))$civicduty)
12021/(96675+12021)
#[1] 0.1105928

table((subset(gerber, voting=="1"))$self)
13191/(13191+95505)
#0.1213568

table((subset(gerber, voting=="1"))$neighbors)
14438/(14438+94258)
#0.1328292

table((subset(gerber, voting=="1"))$neighbors)


votingModel = glm(voting ~ civicduty+hawthorne+self+neighbors,data=gerber, family=binomial)
summary(votingModel)
predictTrainGerber = predict(votingModel, type="response") # give probabilities
summary(predictTrainGerber)
table(gerber$voting, predictTrainGerber > 0.5)

accuracy t=0.3:
(134513+51966)/(134513+51966+56730+100875)
accuracy t=0.3:
(235388/(235388+108696))

#percentage did not vote
table(gerber$voting)
235388/(108696+235388)

#caTools package
ROCRpredTestgerber = prediction(predictTrainGerber, gerber$voting)
auc = as.numeric(performance(ROCRpredTestgerber, "auc")@y.values)
auc 

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

prp(CARTmodel)
ARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=gerber, cp=0.0)
prp(ARTmodel2)
.3/(.3+.31)
table(gerber$control=="1", gerber$sex)


Model31a = rpart(voting ~ control, data=gerber, cp=0.0)
prp(Model31a, digits = 6)
> abs(0.296638-.34)
Model31b = rpart(voting ~ control +sex, data=gerber, cp=0.0)
prp(Model31b, digits = 6)
abs(0.34176-0.345818)

logit33 = glm(voting~ control+sex, data=gerber, family=binomial)
summary(logit33)

LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
Possibilities
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")

##HW#2
letters_ABPR = read.csv("letters_ABPR.csv")
letters_ABPR$isB = as.factor(letters_ABPR$letter == "B")


# Split the data
library(caTools)
letters_ABPR$letter = as.factor(letters_ABPR$letter) #convert class variable to factor
set.seed(1000)
spl = sample.split(letters_ABPR$letter, SplitRatio = 0.5)
Trainletters = subset(letters_ABPR, spl==TRUE)
Testletters = subset(letters_ABPR, spl==FALSE)
table(letters_ABPR$letter)

CARTb = rpart(letter ~ . - isB, data=Trainletters, method="class")
prp(CARTmodel)
PredictCART = predict(CARTb, newdata = Testletters, type = "class")    
table(Testletters$letter, PredictCART)
PredictCART

install.packages("randomForest")
library(randomForest)


StevensForest = randomForest(letter ~ . - isB, data=Trainletters)
PredictForest = predict(StevensForest, newdata = Testletters)
table(Testletters$letter, PredictForest)
letters$letter = as.factor( letters$letter )

##HW#4

census=read.csv("census.csv")
# Split the data
library(caTools)
census$over50k = as.factor(census$over50k)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
Train = subset(census, spl==TRUE)
Test = subset(census, spl==FALSE)

LogModel2 = glm(over50k~ ., data=Train, family="binomial")
summary(LogModel2)

predictTest=predict(LogModel2, newdata=Test, type="response")
table(Test$over50k, predictTrain > 0.5)
table(Test$over50k)

ROCRpredTest = prediction(predictTest, Test$over50k)

(#caTools package)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)  
auc
  


CARTb = rpart(over50k ~ ., data=Train, method="class")
prp(CARTb)
CARTb

PredictCART = predict(CARTb, newdata = Test)   
table(Test$over50k,PredictCART)

ROCRpredTest = prediction(PredictCART, Test$over50k)
ROCRperf = performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperf)
(#caTools package)
  auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)  
  auc
