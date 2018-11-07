# Load libraries
library(tidyverse)
library(magrittr)
library(recipes)
library(rsample)
library(glmnet)
        
load("dm/data/MLProjectData.RData")

# making all variables factors instead of logical
MLProjectData %<>% 
  modify_if(is.logical, as.factor) %>% 
  glimpse()
# building recipes ####
# bake a recipe with dummy variables
rec_obj <- recipe(target ~ ., data = MLProjectData)


trained_rec <- rec_obj %>%
  step_knnimpute(all_predictors()) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>% 
  prep(training = MLProjectData)

train_data <- bake(trained_rec, newdata = MLProjectData)
test_data <- bake(trained_rec, newdata = test.data)


set.seed(123)
valid_split <- initial_split(train_data, .8)

# training data
train_v2 <- analysis(valid_split)
x_train <- train_v2[setdiff(names(train_v2), "target")]
y_train <- train_v2$target

# validation data
valid <- assessment(valid_split)
x_test <- valid[setdiff(names(valid), "target")] %>% 
  as.matrix
y_test <- valid$target

# basic model in glmnet
x <- x_train  %>% 
  as.matrix
y <- y_train

# Basic Model ####
fit = glmnet(x, y)

plot(fit)

print(fit)

coef(fit,s=0.1)

predict(fit, newx=as.matrix(x_test), s=c(0.1,0.05))


cvfit = cv.glmnet(x, y)

plot(cvfit)

cvfit$lambda.min

coef(cvfit, s = "lambda.min")

preds <- predict(cvfit, newx = as.matrix(x_test), s = "lambda.min")

MLmetrics::MSE(preds, y_test)

fit = glmnet(x, y, alpha = 0.2, nlambda = 20)
print(fit)


# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x, y, family="gaussian", alpha=1)
fit.ridge <- glmnet(x, y, family="gaussian", alpha=0)
fit.elnet <- glmnet(x, y, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x_test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x_test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x_test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x_test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x_test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x_test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x_test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x_test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x_test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x_test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x_test)

mse0 <- mean((y_test - yhat0)^2)
mse1 <- mean((y_test - yhat1)^2)
mse2 <- mean((y_test - yhat2)^2)
mse3 <- mean((y_test - yhat3)^2)
mse4 <- mean((y_test - yhat4)^2)
mse5 <- mean((y_test - yhat5)^2)
mse6 <- mean((y_test - yhat6)^2)
mse7 <- mean((y_test - yhat7)^2)
mse8 <- mean((y_test - yhat8)^2)
mse9 <- mean((y_test - yhat9)^2)
mse10 <- mean((y_test - yhat10)^2)


# Fit models:
fit.lasso <- glmnet(x, y, family="gaussian", alpha=1)
fit.ridge <- glmnet(x, y, family="gaussian", alpha=0)
fit.elnet <- glmnet(x, y, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
fit.lasso.cv <- cv.glmnet(x, y, type.measure="mse", alpha=1, 
                          family="gaussian")
fit.ridge.cv <- cv.glmnet(x, y, type.measure="mse", alpha=0,
                          family="gaussian")
fit.elnet.cv <- cv.glmnet(x, y, type.measure="mse", alpha=.5,
                          family="gaussian")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x_test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x_test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x_test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x_test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x_test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x_test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x_test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x_test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x_test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x_test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x_test)

mse0 <- mean((y_test - yhat0)^2)
mse1 <- mean((y_test - yhat1)^2)
mse2 <- mean((y_test - yhat2)^2)
mse3 <- mean((y_test - yhat3)^2)
mse4 <- mean((y_test - yhat4)^2)
mse5 <- mean((y_test - yhat5)^2)
mse6 <- mean((y_test - yhat6)^2)
mse7 <- mean((y_test - yhat7)^2)
mse8 <- mean((y_test - yhat8)^2)
mse9 <- mean((y_test - yhat9)^2)
(mse10 <- mean((y_test - yhat10)^2))

# all the MSE's are  2.087041
