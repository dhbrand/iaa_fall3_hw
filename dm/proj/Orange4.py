
# coding: utf-8

# # Exploratory Data Analysis
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


# Summary Stats of all  Variables ####
skimr::skim(MLProjectData)
# Skim summary statistics
# n obs: 6350 
# n variables: 86 
# 
# ── Variable type:factor ────────────────────────────────────────────────────────────────────────────────────────────
# variable missing complete    n n_unique                         top_counts ordered
# cat1       0     6350 6350        5 B: 1303, C: 1303, D: 1279, E: 1271   FALSE
# cat2       0     6350 6350       12     F: 557, H: 554, E: 550, J: 550   FALSE
# 
# ── Variable type:integer ───────────────────────────────────────────────────────────────────────────────────────────
# variable missing complete    n   mean      sd p0     p25    p50     p75 p100     hist
# num59       0     6350 6350 3227.5 1833.23 53 1640.25 3227.5 4814.75 6402 ▇▇▇▇▇▇▇▇
# 
# ── Variable type:logical ───────────────────────────────────────────────────────────────────────────────────────────
# variable missing complete    n   mean                      count
# cat10       0     6350 6350 0.0069  FAL: 6306, TRU: 44, NA: 0
# cat11       0     6350 6350 0.034  FAL: 6131, TRU: 219, NA: 0
# cat12       0     6350 6350 0.044  FAL: 6070, TRU: 280, NA: 0
# cat13       0     6350 6350 0.04   FAL: 6099, TRU: 251, NA: 0
# cat14       0     6350 6350 0.031  FAL: 6156, TRU: 194, NA: 0
# cat15       0     6350 6350 0.0069  FAL: 6306, TRU: 44, NA: 0
# cat16       0     6350 6350 0.0044  FAL: 6322, TRU: 28, NA: 0
# cat17       0     6350 6350 0.025  FAL: 6192, TRU: 158, NA: 0
# cat18       0     6350 6350 0.011   FAL: 6281, TRU: 69, NA: 0
# cat19       0     6350 6350 0.034  FAL: 6131, TRU: 219, NA: 0
# cat20       0     6350 6350 0.044  FAL: 6070, TRU: 280, NA: 0
# cat21       0     6350 6350 0.039  FAL: 6100, TRU: 250, NA: 0
# cat22       0     6350 6350 0.031  FAL: 6156, TRU: 194, NA: 0
# cat23       0     6350 6350 0.0068  FAL: 6307, TRU: 43, NA: 0
# cat24       0     6350 6350 0.0044  FAL: 6322, TRU: 28, NA: 0
# cat25       0     6350 6350 0.025  FAL: 6192, TRU: 158, NA: 0
# cat26       0     6350 6350 0.011   FAL: 6281, TRU: 69, NA: 0
# cat3       0     6350 6350 0.034  FAL: 6131, TRU: 219, NA: 0
# cat4       0     6350 6350 0.044  FAL: 6070, TRU: 280, NA: 0
# cat5       0     6350 6350 0.04   FAL: 6099, TRU: 251, NA: 0
# cat6       0     6350 6350 0.031  FAL: 6156, TRU: 194, NA: 0
# cat7       0     6350 6350 0.025  FAL: 6192, TRU: 158, NA: 0
# cat8       0     6350 6350 0.011   FAL: 6281, TRU: 69, NA: 0
# cat9       0     6350 6350 0.0044  FAL: 6322, TRU: 28, NA: 0
# 
# ── Variable type:numeric ───────────────────────────────────────────────────────────────────────────────────────────
# variable missing complete    n      mean       sd         p0      p25        p50       p75    p100     hist
# num1       0     6350 6350 849.86    1000.22     0.052    68.64     558.72   1250.55   8710.26 ▇▂▁▁▁▁▁▁
# num10       0     6350 6350  19.42       8.14     9.14     13.42      17.54     23.01     80.86 ▇▅▂▁▁▁▁▁
# num11       0     6350 6350  19.42       8.14     9.14     13.42      17.54     23.01     80.86 ▇▅▂▁▁▁▁▁
# num12       0     6350 6350   0.0022     0.068   -0.3      -0.036     -0.0032    0.033     1.16 ▁▇▁▁▁▁▁▁
# num13       0     6350 6350   0.0041     0.094   -0.35     -0.051     -0.0036    0.048     1.77 ▁▇▁▁▁▁▁▁
# num14       0     6350 6350   0.0058     0.11    -0.38     -0.059     -0.0051    0.057     1.76 ▁▇▁▁▁▁▁▁
# num15       0     6350 6350   0.00069    1.57   -17.36     -0.64      -0.05      0.56     20.01 ▁▁▁▇▁▁▁▁
# num16       0     6350 6350   0.00061    1.57   -17.36     -0.64      -0.05      0.56     20.01 ▁▁▁▇▁▁▁▁
# num17       0     6350 6350   0.00066    1.57   -17.36     -0.64      -0.05      0.56     20.01 ▁▁▁▇▁▁▁▁
# num18       0     6350 6350   0.036      1.42   -11.52     -0.55       0.062     0.68     12.85 ▁▁▁▇▃▁▁▁
# num19       0     6350 6350   0.036      1.42   -11.52     -0.55       0.062     0.68     12.85 ▁▁▁▇▃▁▁▁
# num2       0     6350 6350  19.42       8.14     9.14     13.42      17.54     23.01     80.86 ▇▅▂▁▁▁▁▁
# num20       0     6350 6350   0.036      1.42   -11.52     -0.55       0.062     0.68     12.85 ▁▁▁▇▃▁▁▁
# num21       0     6350 6350   0.036      1.42   -11.52     -0.55       0.062     0.68     12.85 ▁▁▁▇▃▁▁▁
# num22       0     6350 6350   0.037      1.42   -11.52     -0.54       0.062     0.68     12.85 ▁▁▁▇▃▁▁▁
# num23       0     6350 6350   0.5        4.55   -33.42     -1.57       0.8       3.15     16.92 ▁▁▁▁▂▇▂▁
# num24       0     6350 6350  50.66       8.3     16.9      45.5       51.24     56.36     75.97 ▁▁▂▅▇▆▂▁
# num25       0     6350 6350  35.52      15.87     5.02     24.14      32.52     43.98    135.46 ▃▇▅▂▁▁▁▁
# num26       0     6350 6350  26.46      17.25     0.00073  12.18      24.49     38.48     84.05 ▇▇▇▆▅▂▁▁
# num27       0     6350 6350  26.46      11.06     7.36     18.06      23.63     33.64     69.66 ▃▇▅▃▂▁▁▁
# num28       0     6350 6350  54.13      11.09    16.7      46.24      54.58     62.24     87.03 ▁▁▃▆▇▆▂▁
# num29       0     6350 6350  52.42      13.32    13.32     42.69      52.04     61.58    100    ▁▂▆▇▆▃▁▁
# num3       0     6350 6350   0.036      1.38   -15.12     -0.53       0.08      0.64      9.53 ▁▁▁▁▇▅▁▁
# num30       0     6350 6350   1.69      38.93  -555.89    -16.29      10.06     29.12     69.58 ▁▁▁▁▁▁▃▇
# num31       0     6350 6350   0.97       1.08     0         0.23       0.68      1.35     14.75 ▇▁▁▁▁▁▁▁
# num32       0     6350 6350   0.97       0.48     0.13      0.67       0.9       1.21      3.43 ▃▇▅▂▁▁▁▁
# num33       0     6350 6350 129.02      53       43.34     96.2      122.98    146.97    285.93 ▃▃▇▅▁▂▁▁
# num34       0     6350 6350 128.05      52.79    43.28     95.06     122       146       284.5  ▃▃▇▃▁▂▁▁
# num35       0     6350 6350   0.19       1.09    -7.52     -0.27       0.36      0.86      2.98 ▁▁▁▁▂▇▇▁
# num36       0     6350 6350   0.19       1.02    -6.86     -0.24       0.34      0.84      2.65 ▁▁▁▁▂▆▇▁
# num37       0     6350 6350   6.27      36.58  -251.93    -10.18      11.18     28.84     99.34 ▁▁▁▁▂▇▇▁
# num38       0     6350 6350   6.27      35.74  -241.7      -9.78      11.02     28.38     95.42 ▁▁▁▁▂▇▇▁
# num39       0     6350 6350  26.46      11.06     7.36     18.06      23.63     33.64     69.66 ▃▇▅▃▂▁▁▁
# num4       0     6350 6350   0.00035    0.012   -0.098    -0.0046 6e-04         0.0057    0.15 ▁▁▂▇▁▁▁▁
# num40       0     6350 6350  52.42      13.32    13.32     42.69      52.05     61.58    100    ▁▂▆▇▆▃▁▁
# num41       0     6350 6350   0.19       1.09    -7.52     -0.27       0.36      0.86      2.98 ▁▁▁▁▂▇▇▁
# num42       0     6350 6350   6.27      36.58  -251.93    -10.18      11.18     28.84     99.34 ▁▁▁▁▂▇▇▁
# num43       0     6350 6350   0.036      1.14    -9.25     -0.44       0.06      0.55     10.63 ▁▁▁▇▂▁▁▁
# num44       0     6350 6350  49.21       8.41    24.37     43.06      48.24     54.52     91.04 ▁▃▇▆▂▁▁▁
# num45       0     6350 6350  -0.17       4.47   -11.19     -3.16      -0.76      2.18     28.97 ▁▇▇▂▁▁▁▁
# num46       0     6350 6350  -0.18       4.04    -9.9      -2.86      -0.6       1.97     22.06 ▁▆▇▃▁▁▁▁
# num47       0     6350 6350  -0.035    152.01  -419.55    -97.7      -15.44     77.25    814.41 ▁▃▇▅▁▁▁▁
# num48       0     6350 6350  -0.11     147.24  -404.17    -95.13     -14.29     74.29    748.02 ▁▃▇▅▁▁▁▁
# num49       0     6350 6350  -0.18       4.47   -11.19     -3.16      -0.77      2.18     28.97 ▁▇▇▂▁▁▁▁
# num5       0     6350 6350   0.00069    0.016   -0.13     -0.0067     0.0013    0.0087    0.13 ▁▁▁▅▇▁▁▁
# num50       0     6350 6350  -0.067    152.01  -419.55    -97.7      -15.44     77.17    814.41 ▁▃▇▅▁▁▁▁
# num51       0     6350 6350  49.21       8.41    24.37     43.06      48.24     54.52     91.04 ▁▃▇▆▂▁▁▁
# num52       0     6350 6350  -0.17       4.47   -11.19     -3.16      -0.76      2.18     28.97 ▁▇▇▂▁▁▁▁
# num53       0     6350 6350  -0.18       4.04    -9.9      -2.86      -0.6       1.97     22.06 ▁▆▇▃▁▁▁▁
# num54       0     6350 6350  -0.035    152.01  -419.55    -97.7      -15.44     77.25    814.41 ▁▃▇▅▁▁▁▁
# num55       0     6350 6350  -0.11     147.24  -404.17    -95.13     -14.29     74.29    748.02 ▁▃▇▅▁▁▁▁
# num56       0     6350 6350  -0.18       4.47   -11.19     -3.16      -0.77      2.18     28.97 ▁▇▇▂▁▁▁▁
# num57       0     6350 6350  -0.067    152.01  -419.55    -97.7      -15.44     77.17    814.41 ▁▃▇▅▁▁▁▁
# num58       0     6350 6350   0.036      1.41   -13.51     -0.58       0.08      0.73     10.83 ▁▁▁▁▇▁▁▁
# num6       0     6350 6350   0.0024     0.027   -0.24     -0.011      0.0041    0.017     0.17 ▁▁▁▁▇▂▁▁
# num7       0     6350 6350   0.0047     0.036   -0.27     -0.013      0.0078    0.025     0.21 ▁▁▁▂▇▂▁▁
# num8       0     6350 6350  19.42       8.14     9.14     13.42      17.54     23.01     80.86 ▇▅▂▁▁▁▁▁
# num9       0     6350 6350  19.42       8.14     9.14     13.42      17.54     23.01     80.86 ▇▅▂▁▁▁▁▁
# target       0     6350 6350  20.04       1.42     8.48     19.45      20.06     20.68     32.85 ▁▁▁▇▃▁▁▁

# In[2]:


import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.cluster import FeatureAgglomeration as fa
from sklearn.decomposition import PCA as pca


# In[11]:


sns.set()


# In[3]:


df = pd.read_csv("../data/MLProjectData.csv")


# #### building training and validation datasets

# In[4]:


train, valid = train_test_split(df, test_size=0.2, random_state=303)

train.to_csv('../data/train_data.csv', index=False)
valid.to_csv('../data/valid_data.csv', index=False)
# ## Exploring numerical statistics and distributions

# In[15]:


df.describe().sort_values(by='50%', axis=1, ascending=False).to_html('df.html')
num_cols_sorted = df.describe().sort_values(by='mean', axis=1, ascending=False).columns


# In[16]:


print(num_cols_sorted)
len(num_cols_sorted)


# In[17]:


hist = df.hist(column=num_cols_sorted, figsize=(30,40), layout=(10,6), bins=30)


# ## Building feature clusters based on 15 similar histograms discovered

# In[133]:


df_num = train.filter(regex="num*")


# In[134]:


agglo = fa(n_clusters=15)


# In[135]:


dfa = pd.DataFrame(agglo.fit_transform(df_num))


# In[136]:


dfa.shape


# In[137]:


dfa.describe()


# In[48]:


hist_fa = dfa.hist(figsize=(20,20), layout=(5,4), bins=30)


# #### Adding 15 new features back to original data

# In[138]:


train_cat_y = train.filter(regex="cat*|target")
train_cat_y.index


# In[139]:


train_cat_y.index = pd.RangeIndex(len(train_cat_y.index))
train_cat_y.index


# In[140]:


fa_final = pd.concat([dfa, train_cat_y], axis=1)


# In[141]:


fa_final.shape


# In[142]:


fa_final.to_csv('../data/fa_train.csv')


# #### building feature clustering valid dataset

# In[99]:


df_num_valid = valid.filter(regex="num*")


# In[100]:


agglo = fa(n_clusters=15)


# In[101]:


dfa = pd.DataFrame(agglo.fit_transform(df_num_valid))


# In[102]:


dfa.shape


# In[120]:


dfa.index


# In[119]:


valid_cat_y = valid.filter(regex="cat*|target")
valid_cat_y.index


# In[121]:


valid_cat_y.index = pd.RangeIndex(len(valid_cat_y.index))
valid_cat_y.index


# In[128]:


fa_final = pd.concat([dfa, valid_cat_y], axis=1)


# In[129]:


fa_final.shape


# In[130]:


fa_final.to_csv('../data/fa_valid.csv')


# ## Comparing to PCA

# In[73]:


pca_obj = pca(random_state=303)


# In[74]:


pca_obj.fit(df_num)


# #### Cumulative Variance of Components

# In[75]:


np.cumsum(pca_obj.explained_variance_ratio_)


# In[76]:


df_pca = pd.DataFrame(pca_obj.fit_transform(df_num))


# In[77]:


hist_pca = df_pca.hist(figsize=(30,40), layout=(10,6), bins=30)


# #### Fitting to data with 3 components

# In[143]:


pca_obj3 = pca(n_components=3, random_state=303)


# In[144]:


pca_obj3.fit(df_num)


# In[145]:


np.cumsum(pca_obj3.explained_variance_ratio_)


# In[146]:


df_pca3 = pd.DataFrame(pca_obj3.fit_transform(df_num))


# In[83]:


hist_pca = df_pca3.hist(figsize=(30,40), layout=(10,6), bins=30)


# #### Adding 3 pca components back to original data

# In[147]:


pca_final = pd.concat([df_pca3, train_cat_y], axis=1)


# In[148]:


pca_final.shape


# In[86]:


pca_final.describe(include = 'all')


# In[149]:


pca_final.to_csv('../data/pca_train.csv')


# #### building pca valid set

# In[150]:


pca_obj3 = pca(n_components=3, random_state=303)


# In[151]:


pca_obj3.fit(df_num_valid)


# In[152]:


np.cumsum(pca_obj3.explained_variance_ratio_)


# In[153]:


df_pca3 = pd.DataFrame(pca_obj3.fit_transform(df_num_valid))


# In[95]:


hist_pca = df_pca3.hist(figsize=(30,40), layout=(10,6), bins=30)


# In[154]:


pca_final = pd.concat([df_pca3, valid_cat_y], axis=1)


# In[155]:


pca_final.shape


# In[116]:


pca_final.head()


# In[156]:


pca_final.to_csv('../data/pca_valid.csv')


# # PCA
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

# In[1]:


import h2o
from h2o.estimators.pca import H2OPrincipalComponentAnalysisEstimator as h2pca


# In[2]:


h2o.init()


# In[3]:


train = h2o.import_file('../data/train_data.csv')
test = h2o.import_file('../data/valid_data.csv')


# ### Identify predictors and response

# In[22]:


x = train.columns
y = 'target'
x.remove(y)


# In[23]:


ml_pca = h2pca(k=15, transform='standardize', seed=303, training_frame = train)


# In[24]:


ml_pca.train(x=x, training_frame=train)


# In[25]:


ml_pca.varimp()


# In[26]:


ml_pca.summary()


# In[81]:


ml_pca


# In[27]:


pca_train = ml_pca.predict(train)


# In[28]:


pca_train['target'] = train['target']


# In[29]:


pca_test = ml_pca.predict(test)


# In[30]:


dave


# # h2o AutoML

# In[12]:


from h2o.automl import H2OAutoML
import re


# In[31]:


x = pca_train.columns
y = 'target'
x.remove(y)


# ### Run AutoML for 20 base models (limited to 1 hour max runtime by default)

# In[32]:


aml20 = H2OAutoML(max_models=20, seed=303)
aml20.train(x=x, y=y, training_frame=pca_train, leaderboard_frame=pca_test)


# ### View the AutoML Leaderboard

# In[33]:


lb = aml20.leaderboard
# Print all rows instead of default (10 rows)
lb.head(rows=lb.nrows) 


# ### Best Model Score - MAE = 0.9677976859852252

# In[34]:


perf = aml20.leader.model_performance(pca_test)
perf


# ### Run AutoML for all base models (limited to 3 hour max runtime)

# In[10]:


aml = H2OAutoML(max_runtime_secs=7200, seed=303)
aml.train(x=x, y=y, training_frame=train)


# ### View the AutoML Leaderboard

# In[11]:


lb = aml.leaderboard
# Print all rows instead of default (10 rows)
lb.head(rows=lb.nrows) 


# ### Best Model Score - MAE = 0.9677976859852252

# In[12]:


perf = aml.leader.model_performance(test)
perf


# ## Running on only numeric data

# In[42]:


x = train.columns


# In[43]:


import re
regex=re.compile(".*(num).*")
x = [m.group(0) for l in x for m in [regex.search(l)] if m]
y = 'target'


# ### Run AutoML for 20 base models (limited to 1 hour max runtime by default)

# In[46]:


aml20num = H2OAutoML(max_models=20, seed=303)
aml20num.train(x=x, y=y, training_frame=train)


# ### View the AutoML Leaderboard

# In[48]:


lb = aml20num.leaderboard
# Print all rows instead of default (10 rows)
lb.head(rows=lb.nrows) 


# ### Best Model Score - MAE = 0.968196242011554

# In[49]:


perf = aml20num.leader.model_performance(test)
perf


# ## Running on only categorical data

# In[18]:


x = train.columns
z=re.compile(".*(cat).*")
x = list(filter(z.match, x))
y = 'target'


# ### Run AutoML for 20 base models (limited to 1 hour max runtime by default)

# In[19]:


aml20cat = H2OAutoML(max_models=20, seed=303)
aml20cat.train(x=x, y=y, training_frame=train)


# ### View the AutoML Leaderboard

# In[20]:


lb = aml20cat.leaderboard
# Print all rows instead of default (10 rows)
lb.head(rows=lb.nrows) 


# ### Best Model Score - MAE = 0.9700703352714898

# In[22]:


perf = aml20cat.leader.model_performance(test)
perf


# ## Use valid data to drive leaderboard

# In[23]:


x = train.columns
y = 'target'
x.remove(y)


# ### Run AutoML for 20 base models (limited to 1 hour max runtime by default)

# In[24]:


aml20 = H2OAutoML(max_models=20, seed=303)
aml20.train(x=x, y=y, training_frame=train,leaderboard_frame=test)


# ### View the AutoML Leaderboard

# In[25]:


lb = aml20.leaderboard
# Print all rows instead of default (10 rows)
lb.head(rows=lb.nrows) 



# coding: utf-8

# # Using Keras API

# In[14]:


import matplotlib.pyplot as plt
from tensorflow import keras
from sklearn.model_selection import train_test_split
import pandas as pd

import warnings
warnings.filterwarnings('ignore')



# In[173]:


# need all the variables to be numeric for neural networks
df2 =pd.get_dummies(df)


# In[174]:


# splitting target variabler into separate object
train_data = df2.drop('target', axis=1)
train_labels = df2['target']


# In[175]:


# create training and testing data
X_train, X_test, y_train, y_test = train_test_split(train_data, train_labels, test_size=0.2, random_state=303)


# ### Building a Keras Model

# In[176]:


def build_model():
  model = keras.Sequential([
    keras.layers.Dense(64, activation=tf.nn.relu,
                       input_shape=(X_train.shape[1],)),
    keras.layers.Dense(64, activation=tf.nn.relu),
    keras.layers.Dense(1)
  ])

  optimizer = tf.train.RMSPropOptimizer(0.001)

  model.compile(loss='mse',
                optimizer=optimizer,
                metrics=['mae'])
  return model

model = build_model()
model.summary()


# In[154]:


# Display training progress by printing a single dot for each completed epoch
class PrintDot(keras.callbacks.Callback):
  def on_epoch_end(self, epoch, logs):
    if epoch % 100 == 0: print('')
    print('.', end='')

EPOCHS = 500

# Store training stats
history = model.fit(X_train, y_train, epochs=EPOCHS,
                    validation_split=0.2, verbose=0,
                    callbacks=[PrintDot()])


# ### Comparing Train vs Validation Error Rates as Epochs increase

# In[155]:


def plot_history(history):
  plt.figure()
  plt.xlabel('Epoch')
  plt.ylabel('Mean Abs Error')
  plt.plot(history.epoch, np.array(history.history['mean_absolute_error']),
           label='Train Loss')
  plt.plot(history.epoch, np.array(history.history['val_mean_absolute_error']),
           label = 'Val loss')
  plt.legend()
  plt.ylim([0, 5])

plot_history(history)


# ### Adding a short stopping parameter to reduce trainig time

# In[177]:


model = build_model()

# The patience parameter is the amount of epochs to check for improvement
early_stop = keras.callbacks.EarlyStopping(monitor='val_loss', patience=20)

history = model.fit(X_train, y_train, epochs=EPOCHS,
                    validation_split=0.2, verbose=0,
                    callbacks=[early_stop, PrintDot()])

plot_history(history)


# In[178]:


[loss, mae] = model.evaluate(X_test, y_test, verbose=0)

print("Testing set Mean Abs Error: ", mae)


# In[54]:


test_predictions = model.predict(X_test).flatten()

plt.scatter(y_test, test_predictions)
plt.xlabel('True Values')
plt.ylabel('Predictions')
plt.axis('equal')
plt.xlim(plt.xlim())
plt.ylim(plt.ylim())
_ = plt.plot([-100, 100], [-100, 100])


# In[ ]:


## Building a model based on guidelines in Introduction to Neural Nets for Java


# In[57]:


input_neurons = len(X_train.columns)
print(input_neurons)


# In[93]:


def build_model():
  model = keras.Sequential([
    keras.layers.Dense(64, activation=tf.nn.relu,
                       input_shape=(X_train.shape[1],)),
    keras.layers.Dense(1)
  ])

  optimizer = tf.train.RMSPropOptimizer(0.001)

  model.compile(loss='mse',
                optimizer=optimizer,
                metrics=['mae'])
  return model

model = build_model()
model.summary()


# In[82]:


# Display training progress by printing a single dot for each completed epoch
class PrintDot(keras.callbacks.Callback):
  def on_epoch_end(self, epoch, logs):
    if epoch % 100 == 0: print('')
    print('.', end='')

EPOCHS = 500

# Store training stats
history = model.fit(X_train, y_train, epochs=EPOCHS,
                    validation_split=0.2, verbose=0,
                    callbacks=[PrintDot()])


# In[83]:


def plot_history(history):
  plt.figure()
  plt.xlabel('Epoch')
  plt.ylabel('Mean Abs Error')
  plt.plot(history.epoch, np.array(history.history['mean_absolute_error']),
           label='Train Loss')
  plt.plot(history.epoch, np.array(history.history['val_mean_absolute_error']),
           label = 'Val loss')
  plt.legend()
  plt.ylim([0, 5])

plot_history(history)


# In[156]:


model = build_model()

# The patience parameter is the amount of epochs to check for improvement
early_stop = keras.callbacks.EarlyStopping(monitor='val_loss', patience=20)

history = model.fit(X_train, y_train, epochs=EPOCHS,
                    validation_split=0.2, verbose=0,
                    callbacks=[early_stop, PrintDot()])

plot_history(history)


# In[157]:


[loss, mae] = model.evaluate(X_test, y_test, verbose=0)

print("Testing set Loss: ", loss)
print("Testing set Mean Abs Error: ", mae)


# In[158]:


test_predictions = model.predict(X_test).flatten()

plt.scatter(y_test, test_predictions)
plt.xlabel('True Values')
plt.ylabel('Predictions')
plt.axis('equal')
plt.xlim(plt.xlim())
plt.ylim(plt.ylim())
_ = plt.plot([-100, 100], [-100, 100])


# In[13]:


from IPython.core.display import display, HTML
display(HTML("<style>.container { width:100% !important; }</style>"))


# # Using TF Estimators API

# In[2]:


import numpy as np
import pandas as pd
import tensorflow as tf
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error


# In[108]:


train = pd.read_csv("../data/train_data.csv")
test_data = pd.read_csv("../data/valid_data.csv")


# In[109]:


train_data = train.drop('target', axis=1)
train_labels = train['target']


# In[110]:


# create training and testing vars
X_train, X_test, y_train, y_test = train_test_split(train_data, train_labels, test_size=0.2, random_state=303)


# ### get a list of column names and df of continuous data

# In[111]:


num_dat = X_train.select_dtypes(include=[np.number])
num_cols = num_dat.columns.tolist()


# ### get a list of column names and df of discrete data

# In[112]:


cat_cols = [item for item in X_train.columns if item not in num_cols]


# In[113]:


scaler_model = MinMaxScaler()


# In[114]:


scaler_model.fit(X_train.loc[:,num_cols])

X_train_num_scaled = pd.DataFrame(scaler_model.transform(X_train.loc[:,num_cols]),columns=X_train.loc[:,num_cols].columns,index=X_train.loc[:,num_cols].index)

scaler_model.fit(X_test.loc[:,num_cols])

X_test_num_scaled = pd.DataFrame(scaler_model.transform(X_test.loc[:,num_cols]),columns=X_test.loc[:,num_cols].columns,index=X_test.loc[:,num_cols].index)


# In[115]:


X_train_scaled = pd.merge(X_train_num_scaled, X_train.loc[:,cat_cols], on=[X_train_num_scaled.index, X_train_num_scaled.index])
X_train_scaled.drop(columns=['key_0', 'key_1'], axis=1, inplace=True)
len(X_train_scaled.columns)


# In[116]:


X_test_scaled = pd.merge(X_test_num_scaled, X_test.loc[:,cat_cols], on=[X_test_num_scaled.index, X_test_num_scaled.index])
X_test_scaled.drop(columns=['key_0', 'key_1'], axis=1, inplace=True)
len(X_test_scaled.columns)


# In[117]:


# need to drop cat1 and cat2 columns from cat_cols because they have more than 2 levels
cat_cols.remove('cat1')
cat_cols.remove('cat2')


# In[118]:


X_train_final = pd.get_dummies(X_train_scaled, columns=cat_cols)
X_test_final = pd.get_dummies(X_test_scaled, columns=cat_cols)


# Colomns were renamed by making dummies and names need to match up to feature_columns defined in the model

# In[119]:


num_dat = X_train_final.select_dtypes(include=[np.number])
num_cols = num_dat.columns.tolist()


# In[107]:


df_feat_cols = num_cols + cat_cols
print(df_feat_cols)


# In[123]:


#Creating Feature Columns
feat_cols=[]
for cols in num_cols:
    column=tf.feature_column.numeric_column(cols)
    feat_cols.append(column)


# In[124]:


cat1_uniq = X_train.cat1.unique()
cat2_uniq = X_train.cat2.unique()


# In[125]:


cat1_uniq


# In[126]:


# getting the two feature columns embedded correctly
cat1_cat_feat_col = tf.feature_column.categorical_column_with_vocabulary_list('cat1', vocabulary_list=cat1_uniq) 
cat2_cat_feat_col = tf.feature_column.categorical_column_with_vocabulary_list('cat2', vocabulary_list=cat2_uniq) 

cat1_emb_col = tf.feature_column.embedding_column(cat1_cat_feat_col, 2)
cat2_emb_col = tf.feature_column.embedding_column(cat2_cat_feat_col, 2)
cat_cols = [cat1_emb_col, cat2_emb_col]


# In[127]:


print(feat_cols[1])
print(cat1_emb_col)


# In[128]:


feat_cols.extend([cat1_emb_col, cat2_emb_col])


# Need to merge the cat1 and cat 2 columns back to the dataframe

# In[130]:


X_train_final['cat1'] = X_train.cat1
X_train_final['cat2'] = X_train.cat2


# In[135]:


#The estimator model
model=tf.estimator.DNNRegressor(hidden_units=[64],feature_columns=feat_cols, 
                                optimizer='RMSProp', model_dir='tf_w_outliers')

#The estimator model
model=tf.estimator.DNNLinearCombinedRegressor(dnn_hidden_units=[64],linear_feature_columns=[cat1_emb_col, cat2_emb_col], dnn_feature_columns=feat_cols, dnn_optimizer='RMSProp')
# In[132]:


n = len(X_train_final.index)
X_train_final = X_train_final.reindex(range(n))
y_train = y_train.reindex(range(n))


# In[136]:


#the input function
input_func=tf.estimator.inputs.pandas_input_fn(X_train_final,y_train, 
                                               batch_size=100,num_epochs=500,
                                               shuffle=True)


# In[137]:


#Training the model
model.train(input_fn=input_func,steps=1000)


# In[32]:


#Evaluating the model
train_metrics=model.evaluate(input_fn=input_func,steps=1000)


# In[33]:


#Now to predict values we do the following
pred_input_func=tf.estimator.inputs.pandas_input_fn(x=X_test,y=y_test,batch_size=10,num_epochs=1,shuffle=False)
preds=model.predict(input_fn=pred_input_func)


# In[34]:


predictions=list(preds)
final_pred=[]
for pred in predictions:
    final_pred.append(pred["predictions"])


# In[35]:


test_metric=model.evaluate(input_fn=pred_input_func,steps=1000)   


# # Leaky Relu with Isolation Forest

# In[46]:


import pandas as pd
import tensorflow as tf


# In[2]:


train = pd.read_csv("../data/train_data.csv")
valid = pd.read_csv("../data/valid_data.csv")


# In[4]:


X_train = train.drop('target', axis=1)
y_train = train.target

X_test = valid.drop('target', axis=1)
y_test = valid.target


# Going to extract continuous features to filter outliers and scale

# In[9]:


X_train = X_train.select_dtypes(exclude=['object', 'bool'])
X_test = X_test.select_dtypes(exclude=['object', 'bool'])


# Using an Isolation forest to remove outliers before scaling

# In[13]:


from sklearn.ensemble import IsolationForest

clf = IsolationForest(max_samples = 100, random_state = 42)
clf.fit(X_train)

y_out = clf.predict(X_train)
y_out = pd.DataFrame(y_out, columns = ['Top'])
y_out[y_out['Top'] == 1].index.values

X_train = X_train.iloc[y_out[y_out['Top'] == 1].index.values]
X_train.reset_index(drop = True, inplace = True)
print("Number of Outliers:", y_out[y_out['Top'] == -1].shape[0])
print("Number of rows without outliers:", X_train.shape[0])


# Building a scaler to fit on the X training data and then apply to the X testing data

# In[15]:


from sklearn.preprocessing import MinMaxScaler

scaler = MinMaxScaler()
X_train_scaled = scaler.fit_transform(X_train)

X_test_scaled = scaler.transform(X_test)


# In[34]:


X_train_cat = train.drop('target', axis=1)

X_test_cat = valid.drop('target', axis=1)

X_train_cat = X_train_cat.select_dtypes(include=['object', 'bool'])
X_test_cat = X_test_cat.select_dtypes(include=['object', 'bool'])


# In[35]:


X_train_cat.shape


# Need to drop the same rows as the outliers

# In[36]:


X_train_cat = X_train_cat.iloc[y_out[y_out['Top'] == 1].index.values]
X_train_cat.reset_index(drop = True, inplace = True)
print("Number of Outliers:", y_out[y_out['Top'] == -1].shape[0])
print("Number of rows without outliers:", X_train_cat.shape[0])


# Need the column names for each data type

# In[44]:


continuous_features = X_train.columns
categorical_features = X_train_cat.columns
LABEL_COLUMN = 'target'


# Putting the two dataframes back together

# In[38]:


training_set = pd.DataFrame(X_train).merge(X_train_cat, left_index = True, right_index = True)


# Next we need to add back the target column

# In[41]:


y_train = pd.DataFrame(y_train)
y_train = y_train.iloc[y_out[y_out['Top'] == 1].index.values]
y_train.reset_index(drop = True, inplace = True)
print("Number of Outliers:", y_out[y_out['Top'] == -1].shape[0])
print("Number of rows without outliers:", y_train.shape[0])
training_set['target'] = y_train


# Building an input function to feed to the NN

# In[42]:


# Converting Data into Tensors
def input_fn(df, training = True):
    # Creates a dictionary mapping from each continuous feature column name (k) to
    # the values of that column stored in a constant Tensor.
    feat_cols=[]
    for cols in df_feat_cols:
        column=tf.feature_column.numeric_column(cols)
        feat_cols.append(column)
    
    continuous_cols = {k: tf.constant(df[k].values)
                       for k in continuous_features}

    # Creates a dictionary mapping from each categorical feature column name (k)
    # to the values of that column stored in a tf.SparseTensor.
    categorical_cols = {k: tf.SparseTensor(
        indices=[[i, 0] for i in range(df[k].size)],
        values=df[k].values,
        shape=[df[k].size, 1])
        for k in categorical_features}

    # Merges the two dictionaries into one.
    feature_cols = dict(list(continuous_cols.items()) +
                        list(categorical_cols.items()))

    if training:
        # Converts the label column into a constant Tensor.
        label = tf.constant(df[LABEL_COLUMN].values)

        # Returns the feature columns and the label.
        return feature_cols, label
    
    # Returns the feature columns    
    return feature_cols

def train_input_fn():
    return input_fn(training_set)

def eval_input_fn():
    return input_fn(testing_set)

def test_input_fn():
    return input_fn(test_df, False)


# In[47]:


engineered_features = []

for continuous_feature in continuous_features:
    engineered_features.append(
        tf.contrib.layers.real_valued_column(continuous_feature))


for categorical_feature in categorical_features:
    sparse_column = tf.contrib.layers.sparse_column_with_hash_bucket(
        categorical_feature, hash_bucket_size=1000)

    engineered_features.append(tf.contrib.layers.embedding_column(sparse_id_column=sparse_column, dimension=16,
                                                                  combiner="sum"))


# In[48]:


regressor = tf.contrib.learn.DNNRegressor(
    feature_columns=engineered_features, hidden_units=[10, 10], model_dir='tf_mod')


# In[49]:


# Training Our Model
wrap = regressor.fit(input_fn=train_input_fn, steps=500)




# coding: utf-8

# # Using Dummy Vars for Cat Vars

# In[1]:


import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
import pandas as pd
from auto_ml import Predictor
from auto_ml.utils import get_boston_dataset
from auto_ml.utils_models import load_ml_model
import numpy as np


# In[48]:


df_train = pd.read_csv("../data/train_data.csv")
df_valid = pd.read_csv("../data/valid_data.csv")


# In[49]:


df_train = pd.get_dummies(df_train)
df_valid = pd.get_dummies(df_valid)


# In[50]:


column_descriptions = {'target': 'output'}


# In[51]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## Building with default regressor (GradientBoostingRegressor)

# In[52]:


ml_predictor.train(df_train)


# ### Scores - 0.9698717260463539

# In[53]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## Testing Tensorflow and Keras

# In[54]:


ml_predictor.train(df_train, model_names='DeepLearningRegressor')


# ### Scores - 0.9705594118300551

# In[55]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## Testing with XGBoost

# In[56]:


ml_predictor.train(df_train, model_names='XGBRegressor')


# ### Scores - 0.9747633272193728
# 

# In[57]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# # Using original categorical variables instead of dummy vars

# In[63]:


df_train = pd.read_csv("../data/train_data.csv")
df_valid = pd.read_csv("../data/valid_data.csv")


# In[64]:


cat_cols = df_train.filter(regex=("cat*")).columns
column_descriptions = {'target': 'output'}
for col in cat_cols:
    column_descriptions[col] = 'categorical'


# In[65]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## GradientBoostingRegressor

# In[66]:


ml_predictor.train(df_train)


# ### Scores - 0.9691783705681796

# In[67]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## RandomForestRegression

# In[68]:


ml_predictor.train(df_train, model_names='RandomForestRegressor')


# ### Scores - 0.9907481468737971

# In[69]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## XGBoost

# In[70]:


ml_predictor.train(df_train, model_names='XGBRegressor')


# ### Scores - 0.9747633272193728

# In[71]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## DeepLearningRegressor

# In[72]:


ml_predictor.train(df_train, model_names='DeepLearningRegressor')


# ### Scores - 0.9697089788319505

# In[74]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# # Using feature clustering with categorical variables instead of dummy vars

# In[13]:


# create training and testing data
df_train = pd.read_csv('../data/fa_train.csv')
df_valid = pd.read_csv('../data/fa_valid.csv')


# In[14]:


cat_cols = df_train.filter(regex=("cat*")).columns
column_descriptions = {'target': 'output'}
for col in cat_cols:
    column_descriptions[col] = 'categorical'


# In[15]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## Testing with XGBoost

# In[16]:


ml_predictor.train(df_train, model_names='XGBRegressor')


# ### Scores - 0.970495985580132

# In[17]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# # Using pca with categorical variables instead of dummy vars

# In[18]:


# create training and testing data
df_train = pd.read_csv('../data/pca_train.csv')
df_valid = pd.read_csv('../data/pca_valid.csv')


# In[19]:


cat_cols = df_train.filter(regex=("cat*")).columns
column_descriptions = {'target': 'output'}
for col in cat_cols:
    column_descriptions[col] = 'categorical'


# In[20]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## Testing with XGBoost

# In[21]:


ml_predictor.train(df_train, model_names='XGBRegressor')


# ### Scores - 0.9765462957666592

# In[22]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# # Using feature clustering with dummy vars

# In[41]:


# create training and testing data
df_train = pd.read_csv('../data/fa_train.csv')
df_valid = pd.read_csv('../data/fa_valid.csv')


# In[42]:


df_train = pd.get_dummies(df_train)
df_valid = pd.get_dummies(df_valid)


# In[43]:


column_descriptions = {'target': 'output'}


# In[44]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## Testing with XGBoost

# In[27]:


ml_predictor.train(df_train, model_names='XGBRegressor')


# ### Scores - 0.970495985580132

# In[28]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## Testing with Keras

# In[39]:


ml_predictor.train(df_train, model_names='DeepLearningRegressor')


# ### Scores - 1.0139653945363443

# In[40]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# # Using pca with dummy vars

# In[29]:


# create training and testing data
df_train = pd.read_csv('../data/pca_train.csv')
df_valid = pd.read_csv('../data/pca_valid.csv')


# In[30]:


df_train = pd.get_dummies(df_train)
df_valid = pd.get_dummies(df_valid)


# In[31]:


column_descriptions = {'target': 'output'}


# In[32]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## Testing with XGBoost

# In[33]:


ml_predictor.train(df_train, model_names='XGBRegressor')


# ### Scores - 0.9765462957666592

# In[34]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# ## Testing with Keras

# In[45]:


ml_predictor.train(df_train, model_names='DeepLearningRegressor')


# ### Scores - 0.9699720016035425

# In[46]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# # Using all the arguments available in automl with cat vars

# In[75]:


# create training and testing data
df_train = pd.read_csv('../data/train_data.csv')
df_valid = pd.read_csv('../data/valid_data.csv')


# In[76]:


cat_cols = df_train.filter(regex=("cat*")).columns
column_descriptions = {'target': 'output'}
for col in cat_cols:
    column_descriptions[col] = 'categorical'


# In[20]:


ml_predictor = Predictor(type_of_estimator='regressor', column_descriptions=column_descriptions)


# ## Testing with GradientBoostingRegressor

# In[77]:


ml_predictor.train(df_train, optimize_final_model=True, perform_feature_selection=True, cv=9)


# ### Scores - 0.9668935376190846

# In[78]:


# Score the model on test data
test_score = ml_predictor.score(df_valid, df_valid.target)


# In[79]:


ml_predictor.save('automl_model.dill', verbose=True)


# coding: utf-8
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

# In[68]:


import pandas as pd
import numpy as np


# In[58]:


from sklearn.ensemble import GradientBoostingRegressor as GBM
from sklearn.metrics import mean_absolute_error as mae


# In[101]:


train = pd.read_csv("../data/train_data.csv")
valid = pd.read_csv("../data/valid_data.csv")


# In[102]:


X_train = train.drop('target', axis=1)
y_train = train.target

X_test = valid.drop('target', axis=1)
y_test = valid.target


# Using an Isolation forest to remove outliers before scaling

# In[62]:


from sklearn.ensemble import IsolationForest

clf = IsolationForest(max_samples = 100, random_state = 42)
clf.fit(X_train)

y_out = clf.predict(X_train)
y_out = pd.DataFrame(y_out, columns = ['Top'])
y_out[y_out['Top'] == 1].index.values

X_train = X_train.iloc[y_out[y_out['Top'] == 1].index.values]
X_train.reset_index(drop = True, inplace = True)
print("Number of Outliers:", y_out[y_out['Top'] == -1].shape[0])
print("Number of rows without outliers:", X_train.shape[0])



# In[63]:


y_train = y_train.iloc[y_out[y_out['Top'] == 1].index.values]
y_train.reset_index(drop = True, inplace = True)
print("Number of Outliers:", y_out[y_out['Top'] == -1].shape[0])
print("Number of rows without outliers:", X_train.shape[0])


# Building a scaler to fit on the X training data and then apply to the X testing data

# In[64]:


from sklearn.preprocessing import MinMaxScaler

scaler = MinMaxScaler()
X_train = scaler.fit_transform(X_train)

X_test = scaler.transform(X_test)


# In[65]:


mod = GBM(**params)


# In[66]:


mod.fit(X_train, y_train)
y_pred = mod.predict(X_test)
y_pred = y_pred ** (1. / 3)
mae(y_test, y_pred)


# In[ ]:


import xgboost as xgb
from sklearn.model_selection import KFold, train_test_split, GridSearchCV
from sklearn.metrics import mean_absolute_error
import numpy as np


# In[ ]:


rng = np.random.RandomState(31337)


# In[ ]:


X_test_scaled.shape


# In[ ]:


evallist = [(dtest, 'eval'), (dtrain, 'train')]


# In[ ]:


kf = KFold(n_splits=5, shuffle=True, random_state=rng)
for train_index, test_index in kf.split(X_train):
    xgb_model = xgb.XGBRegressor().fit(X_train[train_index], y_train[train_index])
    predictions = xgb_model.predict(X_train[test_index])
    actuals = y_train[test_index]
    print(mean_squared_error(actuals, predictions))


# In[ ]:


xgb_model = xgb.XGBRegressor()
clf = GridSearchCV(xgb_model,
                   {'max_depth': [2,4,6],
                    'n_estimators': [50,100,200]}, verbose=1)
clf.fit(X_train,y_train)
print(clf.best_score_)
print(clf.best_params_)


# In[ ]:


xgb_model = xgb.XGBRegressor(max_depth=2, n_estimators=50, random_state=303)
xgb_model.fit(X_train, y_train)


# In[ ]:


y_pred = xgb_model.predict(X_test)


# In[ ]:


print(mean_absolute_error(y_test, y_pred))


# In[30]:


from sklearn.preprocessing import PowerTransformer
pt = PowerTransformer()


# In[54]:


pt.fit(X_train)
X_train = pt.transform(X_train)
X_test = pt.transform(X_test)


# In[55]:


X_train = pd.DataFrame(X_train)
X_test = pd.DataFrame(X_test)


# In[56]:


y_train = y_train**3


# In[59]:


mod = GBM(**params)


# In[61]:


mod.fit(X_train, y_train)
y_pred = mod.predict(X_test)
y_pred = y_pred ** (1. / 3)
mae(y_test, y_pred)


# # Final Code to be used to generate testing predictions

# base model to compare against

# In[74]:


y_mean = np.repeat(np.mean(y_train), y_test.shape[0])
mae(y_test, y_mean)


# Going to extract continuous features to filter outliers and scale

# In[86]:


params = {'max_features': 'log2', 'learning_rate': 0.2, 'subsample': 0.5, 'max_depth': 3, 
 'loss': 'ls', 'n_estimators': 15, 'random_state': 999}


# In[112]:


X_train = pd.get_dummies(X_train)
X_test = pd.get_dummies(X_test)


# In[97]:


X_train.shape


# In[88]:


mod = GBM(**params)


# In[89]:


mod.fit(X_train, y_train)
y_pred = mod.predict(X_test)
mae(y_test, y_pred)


# In[120]:


x_holdout = pd.read_csv('../data/testData.csv')


# In[121]:


x_holdout.drop(['Unnamed: 0'], axis=1, inplace=True)


# In[109]:


print(np.sort(X_train.cat2.unique()))
print(np.sort(x_holdout.cat2.unique()))


# In[113]:


X_train.columns


# need to create missing columns for A, B, C, D, E, F, K, L

# In[122]:


x_holdout = pd.get_dummies(x_holdout)


# In[114]:


missing_columns = ['A', 'B', 'C', 'D', 'E', 'F', 'K', 'L']
n = x_holdout.shape[0]


# In[123]:


for col in missing_columns:
    col_name = 'cat2_' + col
    x_holdout[col_name] = np.repeat(0, n)


# In[125]:


print(x_holdout.columns)
print(x_holdout.shape)


# In[172]:


yhat = mod.predict(x_holdout)
yhat


# In[187]:


y_out = pd.Series(yhat)


# In[185]:


y_out['Prediction'] = yhat


# In[188]:


y_out.index.name = 'Row'


# In[193]:


y_out.to_csv('orange4.csv', header=True, index=True)


# In[192]:


y_out

