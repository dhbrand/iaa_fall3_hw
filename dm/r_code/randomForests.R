setwd("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Data Mining/Data Mining 2017/RSweave/RandomForests")
load("PenDigits.Rdata")
library('randomForest')
rf = randomForest(digit ~ ., data=train, ntree=50, type='class')

#' We can then examine the confusion matrix to see how our model predicts each class:

rf$confusion

#' The classwise error rates are extremely small for the 
#' entire model! \blue{rf\$err.rate} will show the progression 
#' of misclassification rates as each individual tree is added 
#' to the forest for each class and overall (on the out of bag 
#' (OOB) data that was remaining after the data was sampled to 
#' train the tree).

#rf$err.rate
head(rf$err.rate)
rf$err.rate[50,]


#' Let's try to get a sense of how many trees are necessary to get a good 
#' performance on the test data. (Tuning the hyperparameter ntrees.)

accuracy=vector()
ntrees=seq(1,400,20)
i=1
for(n in ntrees){
rf = randomForest(digit ~ ., data=train, ntree=n, type='class')
test_pred =  predict(rf,test,type='class')
accuracy[i] =  sum(test_pred!=test$digit)/nrow(test)
i=i+1
}
plot(ntrees, accuracy)

#' Clear that we don't need that many trees here. Effect seems to level off
#' after 20-100 trees. Let's look over that range.

accuracy=vector()
ntrees=seq(1,100,2)
i=1
for(n in ntrees){
  rf = randomForest(digit ~ ., data=train, ntree=n, type='class')
  test_pred =  predict(rf,test,type='class')
  accuracy[i] =  sum(test_pred!=test$digit)/nrow(test)
  i=i+1
}

plot(ntrees, accuracy)

#' We could also play around with the other parameters, like mtry:

accuracy=vector()
mtry=seq(1,16)
i=1
for(m in mtry){
  rf = randomForest(digit ~ ., data=train,mtry=m, ntree=50, type='class')
  test_pred =  predict(rf,test,type='class')
  accuracy[i] =  sum(test_pred!=test$digit)/nrow(test)
  i=i+1
}

plot(mtry, accuracy)

#' Can get some good information about variable importance using the importance=T option
rf = randomForest(digit ~ ., data=train,importance = T,ntree=50, type='class')
#' Tells us which variables are important in prediction of each digit
importance(rf)
