
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

