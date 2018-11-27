# Decided I need to get a baseline for what a good MSE might be.  I want to first build a simple linear model with no interactions or higher order terms with al the variables.  Then I will explore more complex models.

# libraries
library(tidyverse)
library(magrittr)
library(recipes)
library(rsample)
library(MLmetrics)
# the training and test data
load("dm/data/MLProjectData.RData")

# making all logicals into factors factors 
MLProjectData %<>% 
  modify_if(is.logical, as.factor)


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
x_valid <- valid[setdiff(names(valid), "target")]
y_valid <- valid$target



train <- read_csv('dm/data/train_data.csv')
test <- read_csv('dm/data/valid_data.csv')

X_train <- select(train, -target)
y_train <- train$target

X_test <- select(test, -target)
y_test <- test$target

# Basic model ####
simple_mod <- lm(target ~ . , data = train)
summary(simple_mod)

y_pred <- predict(simple_mod, test)

# exploring rank deficiency error which could indicate collinearity
length(simple_mod$coefficients) > simple_mod$rank

MSE(y_pred, y_test) # 2.147556


train <- read_csv('dm/data/fa_train.csv')
test <- read_csv('dm/data/fa_valid.csv')

X_train <- select(train, -target)
y_train <- train$target

X_test <- select(test, -target)
y_test <- test$target

# Basic model ####
ft_mod <- lm(target ~ . , data = train)
summary(ft_mod)

y_pred <- predict(ft_mod, test)

# exploring rank deficiency error which could indicate collinearity
length(simple_mod$coefficients) > simple_mod$rank

MSE(y_pred, y_test) # 2.023572


train <- read_csv('dm/data/pca_train.csv')
test <- read_csv('dm/data/pca_valid.csv')

X_train <- select(train, -target)
y_train <- train$target

X_test <- select(test, -target)
y_test <- test$target

# Basic model ####
pca_mod <- lm(target ~ . , data = train)
summary(pca_mod)

y_pred <- predict(pca_mod, test)

# exploring rank deficiency error which could indicate collinearity
length(simple_mod$coefficients) > simple_mod$rank

MSE(y_pred, y_test) # 2.036558
