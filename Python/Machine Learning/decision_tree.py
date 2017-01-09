import pickle
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score, recall_score, precision_score
from sklearn import tree

#load pickle files

features_train = pickle.load(open('features_train.p','rb'))
features_test = pickle.load(open('features_test.p','rb'))
labels_train = pickle.load(open('labels_train.p','rb'))
labels_test = pickle.load(open('labels_test.p','rb'))

sSplit = [2, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100]
recList = []
preList = []
f1List = []

#s value of 10 is best
for s in sSplit:
    
    clf = DecisionTreeClassifier(criterion = 'entropy',
                                 splitter = 'best',
                                 max_features = 'log2',
                                 max_depth = None,
                                 max_leaf_nodes = None,
                                 min_samples_leaf = 6,
                                 min_samples_split = s, 
                                 presort = False)
    clf.fit(features_train, labels_train)
    
    pred = clf.predict(features_test)
    
    acc = accuracy_score(labels_test,pred)
    rec = recall_score(labels_test,pred)
    pre = precision_score(labels_test,pred)
    f1 = (2*(pre*rec))/(pre+rec)
    
    print 'min sample split is: ', s
    print 'accuracy is: ', acc
    print 'recall is: ', rec
    print 'precision is: ', pre
    print 'f1 is: ', f1
    print '\n'

    f1List.append(f1)
    recList.append(rec)
    preList.append(pre)
    
print 'max recall: ', max(recList)

dot_data = tree.export_graphviz(clf, 
                                out_file='tree_named.dot')#,
                                #feature_names = names)

