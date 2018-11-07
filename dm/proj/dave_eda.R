# Load libraries
library(tidyverse)
library(magrittr)
library(rsample)
library(recipes)


load("dm/data/MLProjectData.RData")
test <- read_csv("dm/data/testData.csv")

glimpse(MLProjectData)
glimpse(test)

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
x_valid <- valid[setdiff(names(valid), "target")]
y_valid <- valid$target
