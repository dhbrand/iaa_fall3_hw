library(tidyverse)
library(magrittr)
library(recipes)
library(rsample)

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
