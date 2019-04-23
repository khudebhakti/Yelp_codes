from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import SGD
from keras.callbacks import ModelCheckpoint
from sklearn.cross_validation import train_test_split
import numpy as np
import pandas as pd

#X = np.load("9026_features.npy")
#X.resize((9026,4096))
x_train = np.load("train_features.npy")
x_train.resize((6946,4096))
x_test = np.load("test_features.npy")
x_test.resize((2080,4096))
y_train = np.array(pd.read_csv("y_train.csv"))
y_test = np.array(pd.read_csv("y_test.csv"))
#Y = np.array(pd.read_csv("images_with_classes2.csv"))
print("data load complete")
#x_train,x_test,y_train,y_test = train_test_split(X,Y,test_size=0.1, random_state=10)

model = Sequential()
model.add(Dense(output_dim=4096, input_dim=4096))
model.add(Activation("relu"))
model.add(Dense(1000))
model.add(Activation("relu"))
model.add(Dense(output_dim=2))
model.add(Activation("sigmoid"))

sgd = SGD(lr=0.01, momentum=0.9, nesterov=False)
model.compile(loss='binary_crossentropy', optimizer=sgd, metrics=['accuracy'])


c = ModelCheckpoint("weights.{epoch:02d}-{val_acc:.4f}.hdf5", monitor='val_acc', verbose=1, save_best_only=True, mode='auto')
model.fit(x_train,y_train,nb_epoch=100, batch_size=128,validation_data=(x_test,y_test),callbacks=[c])

#y = model.predict_proba(x_test)
#y = pd.Dataframe(y)
#y1 =np.round(y)

