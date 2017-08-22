import datetime
from datetime import timedelta
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pandas.io.sql as pandasql
import ast
from sqlalchemy import create_engine

plt.style.use('ggplot')
NE = [u'A',u'B',u'B1',u'C',u'D',u'E',u'F',u'G',u'H',u'I',u'J',u'K']
SW = NE[::-1]

dbStr = 'postgresql://dolejarz:changethis@10.160.12.47:5432/bigdata'
engine = create_engine(dbStr)

def readInc(engine):
    '''
    Return dataframe of incidents that are major and have an assoicated tmc
    '''
    #sqlStr = 'SELECT * FROM incidents.mvp_2016 WHERE ("MIR" = \'Yes\' AND tmc != \'\')'
    sqlStr = 'SELECT * FROM incidents.mvp_2016 '\
    'WHERE (tmc != \'\' '\
    'AND "StartDateTime" > %(dstart)s '\
    'AND "EndDateTime" < %(dend)s '\
    'AND ("EventLocation" = \'FGG\' OR "EventLocation" = \'DVP\'))'
    
    df = pandasql.read_sql(sql = sqlStr,
                           params={'dstart':datetime.date(2016,1,1),
                                   'dend':datetime.date(2017,1,1)},
                           con = engine)
    df['StartDateTime'] = pd.to_datetime(df['StartDateTime'])
    df['EndDateTime'] = pd.to_datetime(df['EndDateTime'])
    
    return df

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

def dropNegative(df):
    if df['Delay'] < 0:
        return 0.
    else:
        return df['Delay']
    
def pivotdf(dfIn,func):
    df = pd.concat([dfIn['StartDateTime'].dt.month,dfIn['Delay']],axis=1)
    pt = pd.pivot_table(df, 
                        index = 'StartDateTime', 
                        aggfunc = func)
    pt.reset_index(inplace = True)
    return pt

def pivotdfHour(dfIn,func):
    df = pd.concat([dfIn['StartDateTime'].dt.hour,dfIn['Delay']],axis=1)
    pt = pd.pivot_table(df, 
                        index = 'StartDateTime', 
                        aggfunc = func)
    pt.reset_index(inplace = True)
    return pt

def pivotdfWeekday(dfIn,func):
    df = pd.concat([dfIn['StartDateTime'].dt.dayofweek,dfIn['Delay']],axis=1)
    pt = pd.pivot_table(df, 
                        index = 'StartDateTime', 
                        aggfunc = func)
    pt.reset_index(inplace = True)
    return pt
    
def combinePt(df,agg):
    if agg == 'month':
        pt1 = pivotdf(df,'count')
        pt2 = pivotdf(df,'sum')
        pt3 = pivotdf(df,'mean')
        pt4 = pivotdf(df,'std')
        
    
    elif agg == 'hour':
        pt1 = pivotdfHour(df,'count')
        pt2 = pivotdfHour(df,'sum')
        pt3 = pivotdfHour(df,'mean')
        pt4 = pivotdfHour(df,'std')
        pt5 = pivotdfHour(df,lambda x: np.percentile(x, 5))
        pt6 = pivotdfHour(df,lambda x: np.percentile(x, 25))
        pt7 = pivotdfHour(df,lambda x: np.percentile(x, 50))
        pt8 = pivotdfHour(df,lambda x: np.percentile(x, 75))
        pt9 = pivotdfHour(df,lambda x: np.percentile(x, 95))
        df = pd.concat([pt1,pt2.Delay,pt3.Delay,pt4.Delay,pt5.Delay,pt6.Delay,pt7.Delay,pt8.Delay,pt9.Delay],axis=1)
        df.columns = ['Month','Count','Total','Avg','Std','5th','25th','50th','75th','95th']
    
        return df
    
    elif agg == 'dayofweek':
        pt1 = pivotdfWeekday(df,'count')
        pt2 = pivotdfWeekday(df,'sum')
        pt3 = pivotdfWeekday(df,'mean')
        pt4 = pivotdfWeekday(df,'std')
    
    df = pd.concat([pt1,pt2.Delay,pt3.Delay,pt4.Delay],axis=1)
    df.columns = ['Month','Count','Total','Avg','Std']
    
    return df
    


def main():
    con = engine.connect()
    
    dfInc = readInc(engine)
    dfInc['EventDescription'] = dfInc['EventDescription'].str.lower()
    dfInc['incType'] = dfInc.apply(lambda x: incType(x), axis=1)
    dfInc['Delay'] = dfInc.apply(lambda x: dropNegative(x), axis=1)
    
    #dfInc = dfInc[dfInc['incType'] == 'collision']
    #dfInc = dfInc[dfInc['MIR'] == 'Yes']
    
    # agg should be month, hour, or dayofweek
    agg='dayofweek'
    
    df = combinePt(dfInc,agg)
    
    dfIncMir = dfInc[dfInc['MIR'] == 'Yes']
    dfIncCol = dfInc[dfInc['incType'] == 'collision']
    
    dfMir = combinePt(dfIncMir,agg)
    dfCol = combinePt(dfIncCol,agg)
    
    plt.figure(figsize=(18,8),dpi=300)
    plt.bar(df.Month-0.4,df.Count,color = '#094c82',label = 'All Incidents')
    #plt.bar(dfCol.Month,dfCol.Total,color='green', )
    plt.bar(dfMir.Month-0.4,dfMir.Count,color='#f2766d',label = 'Major Incidents')
    plt.xticks(range(7),('M','T','W','R','F','S','U'))#('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))(df.Month, range(25))
    plt.xlim([-1,7])
    plt.savefig('dayofweek_count_wide.png',format='png')
    plt.show()

    
    '''
    plt.figure(figsize=(12,8),dpi=300)
    plt.fill_between(df['Month'].as_matrix(),df['5th'].as_matrix(),df['95th'].as_matrix(),color='k',alpha=0.05)
    plt.fill_between(df['Month'].as_matrix(),df['25th'].as_matrix(),df['75th'].as_matrix(),color='k',alpha=0.05)
    plt.plot(df['Month'].as_matrix(),df['50th'].as_matrix(),color='#f2766d',label = 'Major Incidents')
    plt.xticks(df.Month, range(25))#('M','T','W','R','F','S','U'))#('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))
    plt.xlim([-1,24])
   # plt.savefig('hour_count.png',format='png')
    plt.show()
    '''
    con.close()
    engine.dispose()
    return df
    