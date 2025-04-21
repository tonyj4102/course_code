## Ridge Regression, Lasso Regression and Elastic Net
## Data used is "Hitters" dataset
## Data contains details of a players performance in the current season.
## This data is used to predict the player's salary for the next season.
## Example is taken from the book "Introduction to Statistical Learning"

install.packages("ISLR") # contains the  Hitters dataset

library("ISLR")
data(Hitters) # loads data into the workspace
summary(Hitters) # to get a sense of what the data looks like

Hitters
length(Hitters)
dim(Hitters)

# Data has 59 NA values in the Salary column.
# For convenience , let's remove them.
Hitters1=na.omit(Hitters)

Hitters
length(Hitters)
dim(Hitters)

# OLS regression (just as a reference point for co-efficients)

ols_m = lm(Salary~.,data=Hitters1) # ols model
summary(ols_m) # view the model, R-sq , p-values and co-efficients

#coef(ols_m) # view ONLY the co-efficients (for both significant and non-significant predictors)

# data preparation for Ridge and Lasso Regression

x=model.matrix(Salary~.,data=Hitters1)[,-1] 
x
# drop 'Salary' and create
# a matrix of predictors
# model.matrix also creates Dummy variables as and when necessary

y=Hitters1$Salary

#######################################################################
#######################################################################

# Ridge Regression
# the 'glmnet' package is used for Ridge and Lasso

install.packages("glmnet")
library(glmnet)

# To identify the best value of lambda ( the shrinkage co-efficient), we use
# the cv.glmnet function.
# This function carries out 10-fold cross-validation on the data, and computes
# the mean error. The result of this function also contains the best value of lambda.

set.seed(1) 

# we set seed to start with the same random numbers.
# cross-validation involves random sampling, so we use set.seed to
# ensure reproducible results.

ridge_model=cv.glmnet(x,y,alpha=0) # alpha= 0 implies ridge regression

# The best lambda is obtained by: It is 238.0769
(bestlam=ridge_model$lambda.min)
(cf_ridge=coef(glmnet(x,y,lambda=bestlam,alpha=0)))

# The above statement will give the co-efficients associated with the best lambda.
# You will notice that there are no zero co-efficient values.

####################################################################################
####################################################################################

## Lasso Regression

#Again, the glmnet package is used here.

set.seed(1)
lasso_model=cv.glmnet(x,y,alpha=1)# alpha = 1 for lasso regression
bestlam_lasso=lasso_model$lambda.min
bestlam_lasso # gives the best lamba value
# It is 2.935124

cf_lasso=coef(glmnet(x,y,lambda=bestlam_lasso))
cf_lasso # will give the coefficients



# You will notice that the following variables have no coefficients associated
# with them:
# HmRun,Runs,RBI,CAtBat,CHits,NewLeagueN

##########################################################
##########################################################

## ElasticNet

#  ElasticNet requires specification of an "alpha" value such that 0>alpha<1

#  This is chosen based on context and the discreton of the modeler.

#  It is not a calculation.

#  For this example, let's assume 0.5 (i.e. equal weightage to Ridge and Lasso penalites)


set.seed(1)
enet_model=cv.glmnet(x,y,alpha=0.5)
bestlam_enet=enet_model$lambda.min

bestlam_enet

# is 3.359171

cf_enet=coef(glmnet(x,y,lambda=bestlam_enet,alpha=0.5))
cf_enet

# The variable Runs,CHits have zero co-efficient values

##############################################################
##############################################################

### side-by-side view of the co-efficients obtained by the 3 methods

comp=cbind(cf_ridge,cf_lasso,cf_enet)
colnames(comp)=c("Ridge","Lasso","ElasticNet")
comp








