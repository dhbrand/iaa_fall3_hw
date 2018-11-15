# Load libraries
library(tidyverse)
library(magrittr)
library(randomForest)
library(rsample)
library(ranger)
library(recipes)
library(h2o)
library(MLmetrics)
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
  step_pca(all_predictors(), threshold = .85) %>% 
  prep(training = MLProjectData)

train_data <- bake(trained_rec, newdata = MLProjectData)
test_data <- bake(trained_rec, newdata = test.data)


set.seed(123)
valid_split <- initial_split(train_data, .8)

# training data
train <- analysis(valid_split)
x_train <- train[setdiff(names(train), "target")]
y_train <- train$target

# validation data
valid <- assessment(valid_split)
x_valid <- valid[setdiff(names(valid), "target")]
y_valid <- valid$target


# default RF model ####
m1 <- randomForest(
  formula = target ~ .,
  data    = train
)
m1

plot(m1)

# number of trees with lowest MSE
which.min(m1$mse) # 343

# what is the lowest mse
min(m1$mse) # 2.050811

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])

# checking the mae on valid data set
preds <- predict(m1, x_valid)
MAE(preds, y_valid)

# Out of Bag vs Validation Error ####
# using default parameters
rf_oob_comp <- randomForest(
  formula = target ~ .,
  data    = train,
  xtest   = x_valid,
  ytest   = y_valid
)

# extract OOB & validation errors
oob <- rf_oob_comp$mse
validation <- rf_oob_comp$test$mse

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, MSE, -ntrees) %>%
  ggplot(aes(ntrees, MSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")




# going to use the built in tuning function from randomForest, we are starting at 5 random variables per split and 
# names of features
features <- setdiff(names(MLProjectData), "target")

set.seed(123)

m2 <- tuneRF(
  x          = MLProjectData[features],
  y          = MLProjectData$target,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

# randomForest speed
system.time(
  speed_randomForest <- randomForest(
    formula = target ~ ., 
    data    = MLProjectData, 
    ntree   = 500,
    mtry    = floor(length(features) / 3)
  )
)
##    user  system elapsed 
## 232.759   0.350 233.188 

# ranger speed
system.time(
  speed_ranger <- ranger(
    formula   = target ~ ., 
    data      = MLProjectData, 
    num.trees = 500,
    mtry      = floor(length(features) / 3)
  )
)

##    user  system elapsed 
## 144.760   0.663  19.366

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(2, 20, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

hyper_grid <- expand.grid(
  mtry       = seq(2, 20, by = 2),
  num.trees  = seq(10, 1000, by = 250),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)
# total number of combinations
nrow(hyper_grid)
## [1] 96

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = target ~ ., 
    data            = train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


# buld model with recommended parameters and test number of trees
num.trees <- data.frame(num.trees = seq(1, 2000, 250))
for(i in 1:nrow(num.trees)) {
  
  # train model
  model <- ranger(
    formula         = target ~ ., 
    data            = train, 
    num.trees       = num.trees$num.trees[i],
    mtry            = 2,
    min.node.size   = 9,
    sample.fraction = 0.700,
    seed            = 123, 
    importance = 'impurity'
  )
  
  # add OOB error to grid
  num.trees$OOB_RMSE[i] <- sqrt(model$prediction.error)
}


num.trees %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# find the MAE on the predictions
mod_rang <- ranger(
  formula         = target ~ ., 
  data            = train, 
  num.trees       = 500,
  mtry            = 2,
  min.node.size   = 9,
  sample.fraction = 0.700,
  seed            = 123, 
  importance = 'impurity'
)

preds <- predict(mod_rang, x_valid)
MLmetrics::MAE(preds$predictions, y_valid)
# make ranger compatible names
names(train_data) <- make.names(names(train_data), allow_ = FALSE)



# names of features
features <- setdiff(names(train_data), "target")

set.seed(123)

m2 <- tuneRF(
  x          = train_data[features],
  y          = train_data$target,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = TRUE      # to show real-time progress 
)


# hyperparameter grid search --> same as above but with increased mtry values
hyper_grid_2 <- expand.grid(
  mtry       = seq(5, 32, by = 3),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE  = 0
)

# perform grid search
for(i in 1:nrow(hyper_grid_2)) {
  
  # train model
  model <- ranger(
    formula         = target ~ ., 
    data            = train_data, 
    num.trees       = 500,
    mtry            = hyper_grid_2$mtry[i],
    min.node.size   = hyper_grid_2$node_size[i],
    sample.fraction = hyper_grid_2$sampe_size[i],
    seed            = 123, 
    verbose = TRUE, 
    num.threads = 6
  )
  
  # add OOB error to grid
  hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}
hyper_grid_2 %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# buld model with recommended parameters
model <- ranger(
  formula         = target ~ ., 
  data            = train_data, 
  num.trees       = 500,
  mtry            = 5,
  min.node.size   = 5,
  sample.fraction = 0.632,
  seed            = 123, 
  importance = 'impurity'
)
# find variable importance from models

model$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")


# running grid search using h2o
h2o.init()

# create feature names
y <- "target"
x <- setdiff(names(train_data), y)

# turn training set into h2o object
train.h2o <- as.h2o(train_data)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 1000, by = 100),
  mtries      = seq(2, 20, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a validaiton set
test.h2o <- as.h2o(valid)
best_model_perf <- h2o.performance(model = best_model, newdata = test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt() 1.284694

# predict new values
pred_h2o <- predict(best_model, test.h2o)
head(pred_h2o)

pred_data <- cbind(as.data.frame(pred_h2o), test_data) %>% 
  select(target = predict)

pred.h2o <- as.h2o(pred_data)
best_model_perf <- h2o.performance(model = best_model, newdata = pred.h2o)
# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()
