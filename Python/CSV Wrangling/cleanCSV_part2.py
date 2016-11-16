from __future__ import division
import pandas as pd
import numpy as np


df = pd.read_csv("C:\Users\dolejar\Documents\Incident Analysis\incidents_MVP.csv", low_memory = False)

def clean2(df):
    
    df['nLanes'] = df.apply(lambda x: nLanes(x), axis=1)
    df['incType'] = df.apply(lambda x: incType(x), axis=1)
    df.to_csv('debug2.csv', index=False)
    return

def nLanes(df):
    try:
        if 'all' in df['EventDescription'] and 'lanes' in df['EventDescription']:
            return 99
        elif 'three' in df['EventDescription'] or '3' in df['EventDescription']:
            return 3
        elif ('two' in df['EventDescription'] or '2' in df['EventDescription']) and '427' not in df['EventDescription']:
            return 2
        elif 'left' in df['EventDescription'] or 'right' in df['EventDescription']:
            return 1
        elif 'ramp' in df['EventDescription']:
            return 89
        elif 'shoulder' in df['EventDescription']:
            return 79
        else:
            return None
    except:
        return None

def incType(df):
    try:
        if 'maintenance' in df['EventDescription']:
            return 'maintenance'
        elif 'disabled' in df['EventDescription'] or 'stopped' in df['EventDescription']:
            return 'stopped'
        elif 'collision' in df['EventDescription']:
            return 'collision'
        else:
            return None
    except:
        return None