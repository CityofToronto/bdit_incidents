import pandas as pd
import numpy as np
import pickle
import datetime
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
from sklearn_pandas import DataFrameMapper


def incTypeFunc(df):
    '''
    maps incident types to a float value for machine learning 
    '''
    if df['incType'] == 'collision':
        return 1.
    if df['incType'] == 'stopped':
        return 2.
    if df['incType'] == 'maintenance':
        return 3.
    else:
        return 0.

def mirFunc(df):
    '''
    Yes/No -> 0/1
    '''
    if df['MIR'] == 'No':
        return 0
    if df['MIR'] == 'Yes':
        return 1
    else:
        return None
        
def nLanesFunc(df):
    if np.isnan(df['nLanes']):
        return 0
    else:
        return int(df['nLanes'])
        
def deltaDrop(df):
    if np.isnan(df['delta']):
        return 0
    else:
        return df[2]


    #read inc file and convert datatypes
print 'Reading files'
df = pd.read_csv('incidents_MVP_v3.csv')
df['StartDateTime'] = pd.to_datetime(df['StartDateTime'])
df['EndDateTime'] = pd.to_datetime(df['EndDateTime'])

#create MIR DataFrame using function above
dfMir = df.apply(lambda x: mirFunc(x), axis = 1)

    
#concatenate features for machine learning into one DataFrame
dfMap = pd.concat([df.incType, 
                   df.nLanes, 
                   df.EventLocation, 
                   df.EventSource, 
                   pd.to_datetime(df.StartTime).dt.time, 
                   df['EndDateTime'] - df['StartDateTime'], 
                   dfMir],axis=1)
dfMap.columns = ['incType','nLanes','EventLocation','EventSource','StartTime','delta','mir'] 
dfMap['delta'] = dfMap['delta'].dt.seconds
df['nLanes'] = df.apply(lambda x: nLanesFunc(x), axis = 1)
dfMap.dropna(inplace = True)
dfMir = dfMap['mir'].copy()
del dfMap['mir']

#drop np.nan rows

#dfMap = dfMap[(!np.isnan(dfMap['incType'])) & (!np.isnan(dfMap['nLanes'])) & (!np.isnan(dfMap['delta'])) & (!np.isnan(dfMap['mir']))]

#delta values of np.nan are assumed to be zero
#dfNew[2] = dfNew.apply(lambda x: deltaDrop(x), axis = 1)

df['nLanes'] = df.apply(lambda x: nLanesFunc(x), axis = 1)

mapper = DataFrameMapper([(['incType'], LabelEncoder()),
                          (['nLanes'], OneHotEncoder()), 
                          (['EventLocation'], LabelEncoder()),
                          (['EventSource'], LabelEncoder()),
                          (['StartTime'],LabelEncoder()),
                          (['delta'],None) 
                          ])

features = mapper.fit_transform(dfMap.copy())

labels = dfMir.as_matrix()
    
pickle.dump(features, open('features.p','wb'))
pickle.dump(labels, open('labels.p','wb'))
      
