import pickle
from sklearn.cross_validation import train_test_split

#load pickle files

print 'loading pickle dumps'

features = pickle.load(open('features.p','rb'))
labels = pickle.load(open('labels.p','rb'))

#split

features_train, features_test, labels_train, labels_test = train_test_split(features, labels, test_size = 0.2)

pickle.dump(features_train, open('features_train.p','wb'))
pickle.dump(features_test, open('features_test.p','wb'))
pickle.dump(labels_train, open('labels_train.p','wb'))
pickle.dump(labels_test, open('labels_test.p','wb'))
