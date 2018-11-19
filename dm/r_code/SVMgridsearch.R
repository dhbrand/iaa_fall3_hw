setwd('')
load('churn.Rdata')
###########################################################################
###########################################################################
############       STANDARDIZE THE CONTINUOUS VARIABLES        ############
###########################################################################
###########################################################################
churnTrainScale = scale(churnTrain[,c(2,6:18)], center=T, scale=T)
churnTrainScale = cbind(churnTrain[,c(1,3:5,19,20)],churnTrainScale)
churnTestScale = scale(churnTest[,c(2,6:18)], center=T, scale=T)
churnTestScale = cbind(churnTest[,c(1,3:5,19,20)],churnTestScale)

install.packages("e1071")
library(e1071)
###########################################################################
###########################################################################
############     TUNE THE SVM HYPERPARAMETERS GAMMA AND C      ############
###########################################################################
###########################################################################
TuneSVM = tune.svm(churn~., data=churnTrainScale,kernel='radial', gamma=2^(-5:-3), cost=2^(-1:2))
 summary(TuneSVM)
plot(TuneSVM)
############################################################################
############################################################################
## USE THE BEST HYPERPARAMETERS TO BUILD FINAL MODEL AND TEST PERFORMANCE ##
############################################################################
############################################################################

bestgamma=0.0625
bestc=4
svm1=svm(churn~., data=churnTrainScale,type='C', kernel='radial', gamma=bestgamma, cost=bestc)
pred=predict(svm1,churnTestScale)
sum(pred!=churnTestScale$churn)/1667