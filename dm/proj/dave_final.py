
# coding: utf-8

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

