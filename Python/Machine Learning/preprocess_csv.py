import pandas as pd
import numpy as np
import pickle
from sklearn.preprocessing import LabelEncoder


def incType(df):
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

def mir(df):
    '''
    Yes/No -> 0/1
    '''
    if df['MIR'] == 'No':
        return 0
    if df['MIR'] == 'Yes':
        return 1
    else:
        return None
        
def nLanes(df):
    if np.isnan(df['nLanes']):
        return 0
    else:
        return df['nLanes']
        
def deltaDrop(df):
    if np.isnan(df[2]):
        return 0
    else:
        return df[2]

if __name__ == '__main__':
    #read inc file and convert datatypes
    print 'Reading files'
    df = pd.read_csv('incidents_MVP_v3.csv')
    df['StartDateTime'] = pd.to_datetime(df['StartDateTime'])
    df['EndDateTime'] = pd.to_datetime(df['EndDateTime'])
    
    #create new columns
    print 'Creating New Columns'
    incType = df.apply(lambda x: incType(x), axis=1)
    incType = incType.astype('float32')
    
    mir = df.apply(lambda x: mir(x), axis = 1)
    mir = mir.astype('float32')
    
    nLanes = df.apply(lambda x: nLanes(x), axis = 1)
    nLanes = nLanes.astype('float32')
    
    delta = df['EndDateTime'] - df['StartDateTime']
    delta = delta.dt.seconds #convert to seconds so everything is of type float
    delta = delta.astype('float32')
    #call deltadrop here
    
    #concat columns into one dataframe, create features and labels arrays
    print 'Concatinating Dataframes'
    
    dfNew = pd.concat([incType, nLanes, delta], axis = 1)
    dfMap = pd.concat([df.incType,df.nLanes,df['EndDateTime'] - df['StartDateTime']],axis=1)

    #delta values of np.nan are assumed to be zero
    dfNew[2] = dfNew.apply(lambda x: deltaDrop(x), axis = 1)
    
    features = dfNew.as_matrix()
    labels = mir.as_matrix()
    
#    pickle.dump(features, open('features.p','wb'))
#    pickle.dump(labels, open('labels.p','wb'))
    
    

    
                
