
# coding: utf-8

# # Using Keras API

# In[14]:


import matplotlib.pyplot as plt
from tensorflow import keras
from sklearn.model_selection import train_test_split
import pandas as pd

import warnings
warnings.filterwarnings('ignore')


# In[172]:


df = pd.read_csv("../data/MLProjectData.csv", header=0)


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

