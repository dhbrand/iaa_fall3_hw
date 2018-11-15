
#' Baseball data from 1986-1987. The Hitters dataset contains 
#' information about 322 baseball players and 20 attributes as follows:
#' 
# AtBat: Number of times at bat in 1986
# Hits: Number of hits in 1986
# HmRun: Number of home runs in 1986
# Runs: Number of runs in 1986
# RBI: Number of runs batted in in 1986
# Walks: Number of walks in 1986
# Years: Number of years in the major leagues
# CAtBat: Number of times at bat during his career
# CHits: Number of hits during his career
# CHmRun: Number of home runs during his career
# CRuns: Number of runs during his career
# CRBI: Number of runs batted in during his career
# CWalks: Number of walks during his career
# League: A factor with levels A and N indicating player's league at the end of 1986
# Division: A factor with levels E and W indicating player's division at the end of 1986
# PutOuts: Number of put outs in 1986
# Assists: Number of assists in 1986
# Errors: Number of errors in 1986
# Salary: 1987 annual salary on opening day in thousands of dollars
# NewLeague: A factor with levels A and N indicating player's league at the beginning of 1987

#' This dataset is available in the ISLR library. The target variable of interest here
#' is the players' salaries. The target variable is missing for 59 of the players, thus
#'we must omit those players from our analysis.
 

setwd('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Data Mining/Data Mining 2017/Data Sets for Demo/Hitters')
load("Hitters.Rdata")
Hitters = na.omit(Hitters) 

#' Create training and validation index vectors. Do forward and backward selection on the training
#' data. The regsubsets function will find "best" model for each number of variables. You can 
#' then retrieve the coefficients, however you have to multiply them into the data yourself (using
#' the design matrix), you can't use the ever-fabulous predict() function.
install.packages("leaps")
library(leaps)
# Create logical vectors that indicate whether an observation goes in training set or test set:
set.seed(7515)
train=sample(c(T,F), nrow(Hitters), rep=TRUE, p=c(0.6,0.4))
test=!train
############
# Run forward and Backward variable selection
regfit.fwd = regsubsets(Salary~., data=Hitters[train, ], method="forward", nvmax=19)
regfit.bwd = regsubsets(Salary~., data=Hitters[train, ], method="backward",nvmax=19)

 # Check out the order of the variables entering the models from each selection technique:

regfit.fwd$vorder
regfit.bwd$vorder

# Create the design matrix for the test data so we can score it with the coefficients
# from each of the candidate models.

X = model.matrix(Salary~., data=Hitters[test, ])

#' Loop through and compute the validation MSE for each proposed model. There are 19 models
#' from forward selection and 19 from backward. For each selection technique, we want to find
#' the best number of parameters on out-of-sample data.

fwd.val.MSE=vector()
bwd.val.MSE=vector()
for (i in 1:19) {
  beta.fwd = coef(regfit.fwd, i)
  beta.bwd = coef(regfit.bwd, i)
  pred.fwd = X[,names(beta.fwd)] %*% beta.fwd
  pred.bwd = X[,names(beta.bwd)] %*% beta.bwd
  fwd.val.MSE[i] = mean((Hitters$Salary[test] - pred.fwd)^2)
  bwd.val.MSE[i] = mean((Hitters$Salary[test] - pred.bwd)^2)
}
min(fwd.val.MSE)
min(bwd.val.MSE)


plot(1:19,fwd.val.MSE, xlab="Number of Inputs", ylab = "Validation MSE")
plot(1:19,bwd.val.MSE, xlab="Number of Inputs", ylab = "Validation MSE")


# Examine the resulting models (trained on training data)

coef(regfit.fwd,10)
coef(regfit.bwd,7)

#' Finalize the resulting models on ALL of the data. Note that we run the variable selection method
#' again! This is the classic CV idea, but you may choose not to. You could always train that specific
#' model (with given variables) on all the data. You're likely to get a better model by
#' re-running the variable selection technique. 

regfit.fwd.best = regsubsets(Salary~., data=Hitters, method="forward", nvmax=10)
regfit.bwd.best = regsubsets(Salary~., data=Hitters, method="backward", nvmax=7)
coef(regfit.fwd.best,10)
coef(regfit.bwd.best,7)

#' Indeed the best model for forward selection changed slightly when we re-ran the selection
#' procedure. Runs leaves the model and Assists enters the model. These types of changes are 
#' not bad, but show evidence of multicollinearity in our variables.
#' 
#' Let's check that out.
#' By default the design matrix contains a column of all 1s for the intercept, so we remove 
#' that to compute the correlation matrix.

X=model.matrix(Salary~. ,data=Hitters)[,-1]
y = Hitters$Salary

cor(X)

#' Yikes! There is some pretty severe multicollinearity at play here. This is where
#' Ridge regression really shines. 


######################################################################
######################################################################
######################################################################

# Let's try Ridge Regression then...

#' 
#' The glmnet package does not use a model formula but instead requires a design matrix
#' with no intercept column, and a vector containing the target variable. 
#' 
#' This package kindly does all the work of cross-validation for us. We can choose the ridge
#' parameter lambda by trying many different values and seeing which one works best on 
#' cross-validation.

install.packages("glmnet")
library(glmnet)
set.seed(1)
grid = 10^seq(10,-2,length=100) #' values for lambda you want to try. You can
                                #' also let the cv.glmnet() function decide on
                                #' an appropriate interval to test simply by
                                #' omitting the lambda= option.
cv.out = cv.glmnet(X[train,], y[train], alpha=0, lambda=grid) #' Only using training data
                                                              #' for CV!
plot(cv.out)

# Find the value of lambda from cross-validation and see how the resulting model works on the
# test data.

bestlambda=cv.out$lambda.min
bestlambda
(ridge.mod.betas = coef(cv.out, s=bestlambda))
pred.ridge = predict(cv.out, s=bestlambda, newx=X[test,])
val.MSE.ridge = mean((pred.ridge - y[test])^2)
val.MSE.ridge

# Final step would be to re-calculate the model on ALL of the data.

out=glmnet(X,y,alpha=0, lambda=bestlambda)
ridge.coef = predict(out, type="coefficients")
ridge.coef 

######################################################################
######################################################################
######################################################################


#' Next, let's try LASSO. Again, just optimize the parameter lambda through 
#' Cross-Validation, seeing what provides the lowest MSE.
#' To Run Lasso instead of ridge, simply change alpha to 1.


grid = 10^seq(10,-2,length=100)
set.seed(1)
cv.out=cv.glmnet(X[train,],y[train],alpha=1,lambda=grid)
plot(cv.out)

#' From here, we can grab the best lambda and check the model on our test data.


bestlambda=cv.out$lambda.min
(lasso.mod.betas = coef(cv.out, s=bestlambda))
pred.lasso = predict(cv.out, s=bestlambda, newx=X[test,])
(val.MSE.lasso = mean((pred.lasso-y[test])^2))

#' Results comparable to the other methods. If we want to go with this method,
#' we'd just update the coefficients (by running the entire optimization again)
#' on the entire dataset.

out=glmnet(X,y,alpha=1,lambda=bestlambda)
lasso.coef=predict(out, type="coefficients")
lasso.coef

#' Notice the sparsity in the parameter estimates - this is the benefit of LASSO over 
#' Ridge Regression.

# Last thing to try if time permits is the ELASTIC NET

grid = 10^seq(10,-2,length=100)
set.seed(1)
cv.out=cv.glmnet(X[train,],y[train],alpha=0.5,lambda=grid)
plot(cv.out)

bestlambda=cv.out$lambda.min
(elastic.mod.betas = coef(cv.out, s=bestlambda))
pred.elastic = predict(cv.out, s=bestlambda, newx=X[test,])
(val.MSE.elastic = mean((pred.elastic-y[test])^2))

################################################################################
################################################################################
################################################################################
# A WORD ABOUT P-VALUES AND ATTEMPTING TO ESTIMATE STATISTICAL SIGNIFICANCE
################################################################################
################################################################################
################################################################################
#' "It is a very natural question to ask for standard errors of regression coefficients or other estimated quantities. 
#' In principle such standard errors can easily be calculated, e.g. using the bootstrap. Still, this package deliberately 
#' does not provide them. The reason for this is that standard errors are not very meaningful for strongly biased 
#' estimates such as arise from penalized estimation methods. Penalized estimation is a procedure that reduces the 
#' variance of estimators by introducing substantial bias. The bias of each estimator is therefore a major component 
#' of its mean squared error, whereas its variance may contribute only a small part. Unfortunately, in most applications
#'  of penalized regression it is impossible to obtain a sufficiently precise estimate of the bias. Any bootstrap-based
#'   calculations can only give an assessment of the variance of the estimates. Reliable estimates of the bias are only 
#'   available if reliable unbiased estimates are available, which is typically not the case in situations in which 
#'   penalized estimates are used. Reporting a standard error of a penalized estimate therefore tells only part of the story. 
#'   It can give a mistaken impression of great precision, completely ignoring the inaccuracy caused by the bias. It is 
#'   certainly a mistake to make confidence statements that are only based on an assessment of the variance of the estimates, 
#'   such as bootstrap-based confidence intervals do."
#'   
# ----Jelle Goeman, Ph.D. Leiden University, Author of the Penalized package in R.
