from __future__ import division
import pandas as pd
import numpy as np
import random
from difflib import SequenceMatcher

df = pd.read_csv("C:\Users\dolejar\Documents\Incident Analysis\original_incident_file.csv", low_memory = False)
nRowsOriginal = len(df.index)

def similar(a, b):
    return SequenceMatcher(None, a, b).ratio()

def clean(dfIn):
    """
    -> Drop all rows with no description
    -> Force event description, offset, atsteet to lowercase
    -> Get offset for events that don't already have an offset field
    -> 
    -> Divide into 3 types of df.EventDescription
    """
    df = dfIn.copy(deep = True) #new dataframe
    nRowsOriginal = len(df.index)
    
    # (1)
    df['EventDescription'] = df['EventDescription'].replace(np.nan, 
                                                            '', 
                                                            regex = True)
    df['OffsetDirection'] = df['OffsetDirection'].replace(np.nan, 
                                                          '', 
                                                          regex = True)
    df['AtStreet'] = df['AtStreet'].replace(np.nan, 
                                            '', 
                                            regex = True)
                                            
    df['EventDescription'] = df['EventDescription'].str.lower()
    df['OffsetDirection'] = df['OffsetDirection'].str.lower()
    df['AtStreet'] = df['AtStreet'].str.lower()
    
    
    #CREATING NEW COLUMNS
    df['Respondents'] = df.apply(lambda x: respondents(x), axis=1)
    df['Offset'] = df.apply(lambda x: offset(x), axis=1)
    df['Street2'] = df.apply(lambda x: atStreet(x), axis=1)
    df['Street2'] = df.apply(lambda x: dropDir(x), axis=1)

    
    df = latLong(df)    
    
    df.to_csv('debug.csv', index=False)
    
    dfErr = df[df['Street2'] == 'ERROR']
    nRowsErr = len(dfErr.index)
    
    return (1 - nRowsErr/nRowsOriginal)

def respondents(df):
    if 'police' in df['EventDescription']:
        return 'police'
    if 'fire' in df['EventDescription']:
        return 'fire'
    if 'ambulance' in df['EventDescription']:
        return 'ambulance'
    return None

def offset(df):
    if df['OffsetDirection'] == '':
        if 'south of' in df['EventDescription']:
            return 'south'
        if 'north of' in df['EventDescription']:
            return 'north'
        if 'east of' in df['EventDescription']:
            return 'east'
        if 'west of' in df['EventDescription']:
            return 'west'
        if 'ramp' in df['EventDescription']:
            return 'ramp'
        return 'at'
    else:
        if 'south of' in df['OffsetDirection']:
            return 'south'
        if 'north of' in df['OffsetDirection']:
            return 'north'
        if 'east of' in df['OffsetDirection']:
            return 'east'
        if 'west of' in df['OffsetDirection']:
            return 'west'
        if 'ramp' in df['OffsetDirection']:
            return 'ramp'
        return 'at'

def atStreet(df):
    '''
    (pd.DataFrame) -> str
    
    extracts the at street information from df['EventDescription'] 
    '''
    dirs = ['southbound','sb', 's.b.','northbound','nb', 'n.b.','eastbound','eb', 'e.b.','westbound','wb', 'w.b.']
    if (df['AtStreet'] == '' and df['OffsetDirection'] == ''):
        try:
            #entries that are NEWS of an intersection
            if (df['Offset'] != 'at' and df['Offset'] != 'ramp'):
                #take the 2 words after offset and 'of', unless either of them are directions
                tempList = df['EventDescription'][df['EventDescription'].index(df['Offset']) + len(df['Offset']): ].split()[1:3]
                try:
                    if tempList[0] in dirs:
                        tempStr = tempList[1]
                    elif tempList [1] in dirs:
                        tempStr = tempList[0]
                    else:
                        tempStr = tempList[0] + ' ' + tempList[1]
                    if ';' in tempStr:
                        tempStr = tempStr.split(';')[0]
                    return tempStr
                except IndexError:
                    return 'ERROR'
            
            # index starting at 0 here since there is no 'of' when 'at' is the offset
            elif (df['Offset'] == 'at'):
                tempList = df['EventDescription'][df['EventDescription'].index(df['Offset']) + len(df['Offset']): ].split()[0:2]
                try:
                    if tempList[0] in dirs:
                        tempStr = tempList[1]
                    elif tempList [1] in dirs:
                        tempStr = tempList[0]
                    else:
                        tempStr = tempList[0] + ' ' + tempList[1]

                    if ';' in tempStr:
                        tempStr = tempStr.split(';')[0]
                    return tempStr
                except IndexError:
                    return 'ERROR'
                
            elif (df['Offset'] == 'ramp'):
                tempList = df['EventDescription'][df['EventDescription'].index(df['Offset']) + len(df['Offset']): ].split()[1:3]
                try:
                    if tempList[0] in dirs:
                        tempStr = tempList[1]
                    elif tempList [1] in dirs:
                        tempStr = tempList[0]
                    else:
                        tempStr = tempList[0] + ' ' + tempList[1]
                        
                    if ';' in tempStr:
                        tempStr = tempStr.split(';')[0]
                    return tempStr
                except IndexError:
                    return 'ERROR'
                
        except ValueError:
            return 'ERROR'
    else:
        return df['AtStreet']

def dropDir(df):
    '''
    drops the direction for offset and at
    changes 'off ramp to' to 'ramp'
    changes 'NA' to 'at'
    '''
    dirs = ['north','n','south','s','east','e','west','w']
    #drop the direction component if it is in the 
    try:
        if df['Street2'].split()[-1] in dirs:
            return df['Street2'][:-1]
        elif 'ramp' in df['Street2']:
            return 'ramp'
        elif ('NA' in df['Street2'] or 'na' in df['Street2'] or 'at' in df['Street2']):
            return 'at'
        else:
            return df['Street2']
    except IndexError:
        return df['Street2']

def levCol(df, string):
    return similar(df['Street2'],string)
    
#use np.isnan(lat) NOT lat == np.nan
def latLong(df):
    for i in df.index:
        if np.isnan(df['I_Lattitude'][i]):
            print(i)
            print('in the if statement!!')
            #create column that is lev ratio of streetname compared to current street
            df['Lev'] = df.apply(lambda x: levCol(x,df['Street2'][i]), axis=1)
            
            #Filter df 
            dfFilt = df[(df['Lev'] > 0.65) & (df['Offset'] == df['Offset'][i]) & (np.isnan(df['I_Lattitude']) == False)]
            
            #if there is at least one entry that meets the criteria
            if len(list(dfFilt.index)) > 0:
                #Select random index in filtered df
                iRandom = random.choice(list(dfFilt.index))
            
                #Assign that shiii
                df['I_Lattitude'][i] = df['I_Lattitude'][iRandom]
                df['I_Longitude'][i] = df['I_Longitude'][iRandom] 
        else:
            continue
    return df
    
    
    