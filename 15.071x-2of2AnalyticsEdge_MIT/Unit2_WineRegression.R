# VIDEO 4

# Read in data
wine = read.csv("wine.csv")
str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE

# Linear Regression (all variables)
model3 = lm(Price ~., data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE


# VIDEO 5

# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)


# VIDEO 6

# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
WinterRain 


modelQQ = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQQ)

# VIDEO 7

# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4, newdata=wineTest)
predictTest

# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST


# ???????????|||||||||||||||homework#2 \\\\\\\\\\\\\\//////////////


model = lm(Temp ~ MEI + CO2 + CH4 +N2O+CFC.11+ CFC.12+TSI+Aerosols, data=ccTrain)

model8=step(model)
summary(model8)

predictTest = predict(model8, newdata=ccTest)
summary(predictTest)

SSE = sum((ccTrain$Temp –predictTest)^2)
          
# pisaTrain and pisaTest

pisaTrain=read.csv("pisa2009train.csv")

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#relevel factors
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

LinReg = lm(readingScore ~ ., data = pisaTrain)
summary(LinReg)

SSE = sum((pisaTrain$Temp –predictTest)^2)

SSE = sum(LinReg$residuals^2)
N=nrow(pisaTest)

RMSE=(SSE/N)^0.5

predTest = predict(LinReg, newdata=pisaTest)
summary(predTest)

SSE = sum((predTest-pisaTest$readingScore)^2)
SSE
N=nrow(pisaTest)
N
RMSE=(SSE/N)^0.5
RMSE

#baseline
summary(pisaTrain$readingScore)
summary(pisaTest$readingScore)
SST = sum((pisaTest$readingScore-mean(pisaTrain$readingScore))^2)
SST

#testset R^2

SSE = sum((pisaTest$readingScore-predTest)^2) # 	*******test not train

R2=1-SSE/SST
R2
        
#HWpage #3 Flu searches vs. visits

FluTrain=read.csv("FluTrain.csv")
> which.max(FluTrain$ILI)
[1] 303
> FluTrain$Week[303]
[1] 2009-10-18 - 2009-10-24
logILI=log(ILI)

plot(log(FluTrain$ILI), FluTrain$Queries, xlab = "ILI", ylab = "Queries", main = "Queries vs log(ILI)", col ="red")

#regression model
FluTrend1 = lm(log(FluTrain$ILI) ~ FluTrain$Queries, data = FluTrain)
summary(FluTrend1)

FluTest=read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FluTest$Week 
PredTest1[11]

2.187383
FluTest$ILI[11]
2.293422

SSE = sum((PredTest1-FluTest$ILI)^2)
SSE
str(PredTest1)
N=417
RMSE=(SSE/N)^0.5
RMSE

#time series
install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)

plot(log(ILILag2), log(FluTrain$ILI), xlab = "logILILag2", ylab = "logILI", main = "logILI vs logILILag2", col ="red")

FluTrend2 = lm(log(FluTrain$ILI) ~ FluTrain$Queries+log(ILILag2), data = FluTrain)
summary(FluTrend2)

#add ILILg2 to test data

> FluTest$ILILag2=lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

#fillin missing values
nrow(FluTrain)
nrow(FluTest)
head(FluTrain)
tail(FluTest)
head(FluTest)
tail(FluTraing)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

FluTrend2 = lm(log(FluTrain$ILI) ~ FluTrain$Queries+log(ILILag2), data = FluTrain)
summary(FluTrend2)

PredTest1 = exp(predict(FluTrend2, newdata=FluTest))

#more ?arima

## page3 (optional)

statedata=read.csv("statedata.csv")



plot(statedata$x, statedata$y, xlab = "X", ylab = "Y", main = "Y vs. X", col ="red")

tapply(statedata$HS.Grad, statedata$state.region, mean,na.rm=TRUE)


