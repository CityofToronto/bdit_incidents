from __future__ import division
import datetime
from datetime import timedelta
import numpy as np
import pandas as pd
import pandas.io.sql as pandasql
from sqlalchemy import create_engine
import matplotlib.pyplot as plt
import matplotlib
matplotlib.style.use('ggplot')


engine = create_engine('postgresql://dolejar:dan@137.15.155.38:5432/bigdata_ah')

segments=['A','B','B1','C','D','E','F','G','H','I','J','K']
dvpSegmentsN = ['F','G','H','I','J','K']
namesN = ['Dundas','Bayview','Don Mills','Wynford','Lawrence','York Mills']
dvpSegmentsS = list(reversed(dvpSegmentsN))
fggSegmentsE = ['A','B','B1','C','D','E','F']
fggSegmentsW = list(reversed(fggSegmentsE))

def readSegments(inSegments, month, day, n, engine):
    '''
    Returns list of dataframes 
    Each element is travel time on one segment
    '''
    dfList = []
    startDate = datetime.date(2015,month,day)
    endDate = startDate + timedelta(days=n)
    
    for i in range(len(inSegments)-1):
        print('Query going in...')
        strSQL = 'SELECT bluetooth.raw_data."Timestamp",'\
        'bluetooth.raw_data."AvgMeasuredTime",'\
        'bluetooth.raw_data."StartPointName",'\
        'bluetooth.raw_data."EndPointName" '\
        'FROM bluetooth.raw_data INNER JOIN bluetooth.ref_segments '\
        'ON (bluetooth.raw_data."StartPointName" '\
        '= bluetooth.ref_segments.orig_startpointname '\
        'AND bluetooth.raw_data."EndPointName" '\
        '= bluetooth.ref_segments.orig_endpointname) '\
        'WHERE (bluetooth.ref_segments.startpointname = %(pt1)s '\
        'AND bluetooth.ref_segments.endpointname = %(pt2)s '\
        'AND bluetooth.raw_data."Timestamp" >= %(dstart)s '\
        'AND bluetooth.raw_data."Timestamp" < %(dend)s)'
        dfList.append(pandasql.read_sql(sql=strSQL,
                                        params={'pt1':inSegments[i],
                                                'pt2':inSegments[i+1],
                                                'dstart':startDate,
                                                'dend':endDate},
                                        con=engine))
    return dfList

def selectDateInc(month,day,data):
    '''
    Returns list of incident data dataframes for inputed date on inputted segment in form [selected direction, other direction]
    '''
    data.StartDateTime = pd.to_datetime(data.StartDateTime)
    startDate = datetime.date(2015,month,day)
    endDate = startDate + timedelta(days=1)
    startDate = pd.to_datetime(startDate)
    endDate = pd.to_datetime(endDate)
    return (data[(data.StartDateTime >= startDate) 
                 & (data.StartDateTime < endDate)])
                 
def baseline(dfList,p):
    '''
    takes list of dataframes, pivots to inputed percentile 
    '''    
    
    ptList = []
    for seg in dfList:
        seg = seg[(seg.Timestamp.dt.dayofweek != 6) 
                  & (seg.Timestamp.dt.dayofweek != 5) 
                  & (seg.Timestamp.dt.dayofweek != 4) 
                  & (seg.Timestamp.dt.dayofweek != 0)]
        seg['Time'] = seg.Timestamp.apply(lambda x: x.time())
        ptList.append(pd.pivot_table(seg,
                                     index='Time',
                                     aggfunc=lambda x: np.percentile(x, p)))
                                     
    df = ptList[0]
    for seg in ptList[1:]:
        df.AvgMeasuredTime += seg.AvgMeasuredTime
    return ptList[0]

def baselineSegments(dfList,p):
    '''
    takes list of dataframes, pivots to inputed percentile 
    '''    
    
    ptList = []
    for seg in dfList:
        seg = seg[(seg.Timestamp.dt.dayofweek != 6) 
                  & (seg.Timestamp.dt.dayofweek != 5) 
                  & (seg.Timestamp.dt.dayofweek != 4) 
                  & (seg.Timestamp.dt.dayofweek != 0)]
        seg['Time'] = seg.Timestamp.apply(lambda x: x.time())
        ptList.append(pd.pivot_table(seg,
                                     index='Time',
                                     aggfunc=lambda x: np.percentile(x, p)))
    return ptList

def baselineDrop(dfList):
    '''
    evaluates median of each timestamp, 
    drop all timestamps less than 1.25x that of median, reevaluate median
    returns series of baseline travel time 
    '''
    ptList = []
    for seg in dfList:
        seg = seg[(seg.Timestamp.dt.dayofweek != 6) 
                  & (seg.Timestamp.dt.dayofweek != 5) 
                  & (seg.Timestamp.dt.dayofweek != 4) 
                  & (seg.Timestamp.dt.dayofweek != 0)]
        seg['Time'] = seg.Timestamp.apply(lambda x: x.time())
        ptList.append(pd.pivot_table(seg,
                                     index='Time',
                                     aggfunc=lambda x: np.percentile(x, 50)))
    for i in range(len(dfList)):
        
        dfList[i]['Time'] = dfList[i].Timestamp.apply(lambda x: x.time())
        for t in list(ptList[i].index): #iterate through each minute of a 24h clock                     
            dfList[i] = dfList[i][(dfList[i].Time != t) 
                                   | ((dfList[i].Time == t) 
                                  & (dfList[i].AvgMeasuredTime 
                                     < 1.25*ptList[i].get_value(t,'AvgMeasuredTime')))]
    ptList = []
    for seg in dfList:
        seg = seg[(seg.Timestamp.dt.dayofweek != 6) 
                  & (seg.Timestamp.dt.dayofweek != 5) 
                  & (seg.Timestamp.dt.dayofweek != 4) 
                  & (seg.Timestamp.dt.dayofweek != 0)]
        seg['Time'] = seg.Timestamp.apply(lambda x: x.time())
        ptList.append(pd.pivot_table(seg,
                                     index='Time',
                                     aggfunc=lambda x: np.percentile(x, 50)))
    df = ptList[0]
    for seg in ptList[1:]:
        df.AvgMeasuredTime += seg.AvgMeasuredTime
    return ptList[0]
    
def baselinePlot(inSegments, month, day, baseline, engine):
    '''
    plots inputted day and baseline with fill
    returns difference in area as a timedelta
    '''
    segmentTimes = readSegments(inSegments, month, day,1, engine)
    df = segmentTimes[0]
    df.Timestamp = pd.to_datetime(df.Timestamp)
    for seg in segmentTimes[1:]:
        df.AvgMeasuredTime += seg.AvgMeasuredTime
        
    plt.figure(figsize=(16,12),dpi=300)
    plt.plot(df.Timestamp, df.AvgMeasuredTime/60,'k')
    plt.plot(df.Timestamp, baseline.AvgMeasuredTime/60,'b')
    plt.fill_between(df.Timestamp.as_matrix(),
                     df.AvgMeasuredTime.as_matrix()/60,
                     baseline.AvgMeasuredTime.as_matrix()/60,
                     where
                     = (baseline.AvgMeasuredTime/60 > df.AvgMeasuredTime/60),
                     facecolor='green')    
    plt.fill_between(df.Timestamp.as_matrix(),
                     df.AvgMeasuredTime.as_matrix()/60,
                     baseline.AvgMeasuredTime.as_matrix()/60,
                     where
                     = (baseline.AvgMeasuredTime/60 < df.AvgMeasuredTime/60), 
                     facecolor='red')    
    plt.show()
    integral = np.trapz(y=df.AvgMeasuredTime.as_matrix(), x=df.Timestamp.as_matrix()) - np.trapz(y=baseline.AvgMeasuredTime.as_matrix(),x=df.Timestamp.as_matrix())
    return integral
    
def baselinePlotSegments(inSegments,segNames, month, day, baseline, incData1, incData2, direction, engine):
    
    incDayData1 = selectDateInc(month,day,incData1)
    incDayData2 = selectDateInc(month,day,incData2)
    
    segmentTimes = readSegments(inSegments, month, day,1, engine)
    segmentTimes[0].Timestamp = pd.to_datetime(segmentTimes[0].Timestamp)
    
    #if(direction == 'SB' or direction == 'EB'):
        #inSegments = list(reversed(inSegments))
    if(direction == 'SB' or direction == 'NB'):
        start = 'bt-start'
        end = 'bt-end'
    if(direction == 'EB' or direction == 'WB'):
        start = 'bt-start_1'
        end = 'bt-end_1'
    
    fig,ax = plt.subplots(5,1, figsize=(15,12),sharex=True,dpi=600)
    
    for i in range(len(inSegments)-1):
        ax[i].plot(segmentTimes[0].Timestamp, 
                   segmentTimes[i].AvgMeasuredTime/60, 
                   'k',
                   linewidth=2)
        ax[i].plot(segmentTimes[0].Timestamp,
                   baseline[i].AvgMeasuredTime/60,
                   'k',
                   linewidth=0.5)
        ax[i].fill_between(segmentTimes[0].Timestamp.as_matrix(), 
                           segmentTimes[i].AvgMeasuredTime.as_matrix()/60, 
                           baseline[i].AvgMeasuredTime.as_matrix()/60, 
                           where 
                           = (baseline[i].AvgMeasuredTime/60 
                           < segmentTimes[i].AvgMeasuredTime/60), 
                           facecolor='red',
                           alpha = 0.2)        
        ax[i].fill_between(segmentTimes[0].Timestamp.as_matrix(), 
                           segmentTimes[i].AvgMeasuredTime.as_matrix()/60, 
                           baseline[i].AvgMeasuredTime.as_matrix()/60, 
                           where 
                           = (baseline[i].AvgMeasuredTime/60 
                           > segmentTimes[i].AvgMeasuredTime/60), 
                           facecolor='green',
                           alpha = 0.2)
        if(direction == 'SB' or direction == 'EB'):                 
            incDayData1temp = incDayData1[(incDayData1[start] == inSegments[i]) 
                                          & (incDayData1[end] == inSegments[i+1])]
            incDayData2temp = incDayData2[(incDayData2[start] == inSegments[i]) 
                                          & (incDayData2[end] == inSegments[i+1])]
        else:
            incDayData1temp = incDayData1[(incDayData1[start] == inSegments[i+1]) 
                                          & (incDayData1[end] == inSegments[i])]
            incDayData2temp = incDayData2[(incDayData2[start] == inSegments[i+1]) 
                                          & (incDayData2[end] == inSegments[i])]
        for inc in incDayData1temp.StartDateTime:
            ax[i].axvline(x=inc,
                          color = 'k',  
                          linewidth=1.5)
        for inc in incDayData2temp.StartDateTime:
            ax[i].axvline(x=inc, 
                          color = 'k',
                          linestyle='dashed', 
                          linewidth=0.2)
                          
        ax[i].title.set_text(segNames[i]+' to '+segNames[i+1])
        ax[i].set_ylim([0,20])
    
    plt.savefig(direction+'_'+str(month)+'_'+str(day)+'.png',format='png',transparent=True)
    plt.show()
    return
    
#dvpN = readSegments(dvpSegmentsN,1,1,365,engine)    
#baselineN = baselineSegments(dvpN,50)

dvpIncNB = pd.read_csv('dvp-north-inc.csv')
dvpIncSB = pd.read_csv('dvp-south-inc.csv')
fggIncEB = pd.read_csv('fgg-east-inc.csv')
fggIncWB = pd.read_csv('fgg-west-inc.csv')

#baselinePlotSegments(dvpSegmentsN, namesN, 5, 28, baselineN, dvpIncNB, dvpIncSB, 'NB', engine)
