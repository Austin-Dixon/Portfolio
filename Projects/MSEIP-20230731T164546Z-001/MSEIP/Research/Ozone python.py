# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 10:36:45 2019

@author: nitsu
"""

#Linear Regression Method
import pandas as pd
from statistics import stdev
import tensorflow
import keras
import sklearn
from sklearn import linear_model
from sklearn.utils import shuffle
import pathlib
import seaborn as sns

#Importing of data
df=pd.read_csv("py2014.csv")
print(df.head())

#Removing of extreme outlier data
df=df[df['RH']<=100];df=df[df['WindSpeed']<=100];df=df[df['Ozone']>=0]

#Subgrouping of data by location
Grant=df[df['Site']==1]
SWIC=df[df['Site']==2]
Planetarium=df[df['Site']==3]

#The parameters being used to predict a variable in a separate location.
#We're using the information from SWIC in this instance
data= SWIC[['Ozone','Temp','Press','RH','WindSpeed','WindDir','Rainfall','VPD']]

#Sets the variable being predicted and creations of our X and y datasets. we're predicting
#Ozone at Grant's farm in this instance
predict = 'Ozone'
X=array(data)
y=array(Grant['Ozone'])

#An algorithm for a linear regression is performed 10,000 times for training the data
#and is tested for the accuracy of its results. The average of these accuracies are then
#displayed at the end of the loop.
accuracy=[]
for n in range(10000):
    X_train,X_test,y_train,y_test= sklearn.model_selection.train_test_split(X,y,test_size=.3)
    linear=linear_model.LinearRegression()
    linear.fit(X_train,y_train)
    accuracy.append(linear.score(X_test,y_test))
print(mean(accuracy))

#The final linear model is then used on the unused testing data to see how well that
#it performs on predicting unknown y (ozone) values. These predictions are shown
#by side of the actual ozones recorded in these entries.
predictions=linear.predict(X_test)
for x in range(len(predictions)):
    print(format(predictions[x],',.2f'),'\t',format(y_test[x],',.2f'))

######################################################################
    
#Network Regression Method
import pandas as pd
from statistics import stdev
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from sklearn import linear_model
from sklearn.utils import shuffle
import pathlib
import seaborn as sns

#Importing and formation of data
dataset=pd.read_csv("py2014col.csv")
dataset.pop('Unnamed: 0')
dataset.tail()

#Creates a subsets of data to train the neural network and data used for testing (Our 'X')
train_dataset = dataset.sample(frac=0.8,random_state=0)
test_dataset = dataset.drop(train_dataset.index)

#Create an assembly of scatter and density plots to view trends between selected variables
sns.pairplot(train_dataset[["Ozone", "OzoneG", "RH","RHG"]], diag_kind="kde")
show()

#Gives summary statistics for training parameters
train_stats = train_dataset.describe()
train_stats.pop("OzoneP")
train_stats = train_stats.transpose()
print(train_stats)

#Creates training and testing planetarium ozone list for the algorithm to predict (our 'Y')
train_labels = train_dataset.pop('OzoneP')
test_labels = test_dataset.pop('OzoneP')

#Normalizes data for better interpretation on how the magnitudes of each variable
#directly affects ozone outcomes
def norm(x):
  return (x - train_stats['mean']) / train_stats['std']
normed_train_data = norm(train_dataset)
normed_test_data = norm(test_dataset)
normed_train_data.head()

#Defines the formation of the neural network model. Can add or remove layers or layer
#size to accommodate for processing times by the computer being used.
def build_model():
  model = keras.Sequential([
    layers.Dense(289, activation=tf.nn.elu, input_shape=[len(train_dataset.keys())]),
    layers.Dense(289, activation=tf.nn.elu),
    layers.Dense(289, activation=tf.nn.elu),
    layers.Dense(289, activation=tf.nn.elu),
    layers.Dense(289, activation=tf.nn.elu),
    layers.Dense(289, activation=tf.nn.elu),
    layers.Dense(17, activation=tf.nn.elu),
    layers.Dense(1)
  ])

  optimizer = tf.keras.optimizers.Nadam(.001)

  model.compile(loss='mean_squared_error',
                optimizer=optimizer,
                metrics=['mean_absolute_error', 'mean_squared_error'])
  return model

#Builds an instance of the neural network and displays the attributes of the network
model=build_model()
model.summary()

#examples of raw outcomes for ozone after running data through the network
example_batch = normed_train_data[:10]
example_result = model.predict(example_batch)
print(example_result)

#Defines the representation for when a single instance of parameter training is complete 
class PrintDot(keras.callbacks.Callback): 
  def on_epoch_end(self, epoch, logs):
    if epoch % 100 == 0: print('')
    print('.', end='')

EPOCHS = 1000 #Number of iterations for training the algorithm

#Actually trains the network using 80% of the training data.
history = model.fit(
  normed_train_data, train_labels,
  epochs=EPOCHS, validation_split = 0.2, verbose=0,
  callbacks=[PrintDot()])

#Displays the progression of accuracy provide by the network over each iteration
#Of training
hist = pd.DataFrame(history.history)
hist['epoch'] = history.epoch
hist.tail()

#Displays the previous progression in the form of plots by average absolute and squared
#errors by training iteration
def plot_history(history):
  hist = pd.DataFrame(history.history)
  hist['epoch'] = history.epoch

  figure()
  xlabel('Epoch')
  ylabel('Mean Abs Error [Ozone]')
  plot(hist['epoch'], hist['mean_absolute_error'],
           label='Train Error')
  plot(hist['epoch'], hist['val_mean_absolute_error'],
           label = 'Val Error')
  ylim([0,5])
  legend()

  figure()
  xlabel('Epoch')
  ylabel('Mean Square Error [$Ozone^2$]')
  plot(hist['epoch'], hist['mean_squared_error'],
           label='Train Error')
  plot(hist['epoch'], hist['val_mean_squared_error'],
           label = 'Val Error')
  ylim([0,25])
  legend()
  show()
plot_history(history)

#Resetting our model to train using a method of training based on the progression
# of the model
model = build_model()

# The patience parameter is the amount of epochs to check for improvement
early_stop = keras.callbacks.EarlyStopping(monitor='val_loss', patience=30)


#The model now stops training when the value loss stops decreasing after a minimum of 30
#iterations
history = model.fit(normed_train_data, train_labels, epochs=EPOCHS,
                    validation_split = 0.2, verbose=0, callbacks=[early_stop, PrintDot()])
hist = pd.DataFrame(history.history)
hist['epoch'] = history.epoch
hist.tail()
plot_history(history)

#Plots ozone predictions made by the model in correlation to the actual reading for the 
#unused data entries  
test_predictions = model.predict(normed_test_data).flatten()
scatter(test_labels, test_predictions)
xlabel('True Values [Ozone]')
ylabel('Predictions [Ozone]')
axis('equal')
axis('square')
xlim([0,xlim()[1]])
ylim([0,ylim()[1]])
_ = plot([-100, 100], [-100, 100])
#model.get_weights()
