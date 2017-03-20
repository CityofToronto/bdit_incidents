import datetime
from datetime import timedelta
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pandas.io.sql as pandasql
from sqlalchemy import create_engine

NE = ['A','B','B1','C','D','E','F','G','H','I','J','K']
SW = NE[::-1]

dbStr = ''
engine = create_engine(dbStr)

def readSegments(inSegments, startDate, engine):
    '''
    Returns list of list of dataframes
    first list is list of baseline travel times dataframes on inputted segments on startDate
    second list is list of travel time baseline dataframes
    
    '''
    if inSegments == []:
        return None
        
    con = engine.connect()
    dfList = []
    dfListBinned = []

    startDate = startDate.date()
    endDate = startDate + timedelta(days=1)
    
    for i in range(len(inSegments)-1):
        print('Query going in...')
        strSQL = 'SELECT bluetooth.observations_201511.measured_time, '\
        'bluetooth.observations_201511.measured_timestamp '\
        'FROM bluetooth.observations_201511 INNER JOIN bluetooth.ref_segments '\
        'ON (bluetooth.observations_201511.analysis_id '\
        '= bluetooth.ref_segments.analysis_id) '\
        'WHERE (bluetooth.ref_segments.startpointname = %(pt1)s '\
        'AND bluetooth.ref_segments.endpointname = %(pt2)s '\
        'AND bluetooth.observations_201511.measured_timestamp >= %(dstart)s '\
        'AND bluetooth.observations_201511.measured_timestamp < %(dend)s)'
        dfList.append(pandasql.read_sql(sql=strSQL,
                                        params={'pt1':inSegments[i],
                                                'pt2':inSegments[i+1],
                                                'dstart':startDate,
                                                'dend':endDate},
                                        con=engine))
    for df in dfList:
        df['measured_timestamp'] = pd.to_datetime(df['measured_timestamp'])
        dfListBinned.append(df.groupby(
        pd.TimeGrouper('5min',key='measured_timestamp')).mean().interpolate())
    
    for df in dfListBinned:
        df.reset_index(inplace = True)
        df['measured_timestamp'] = df['measured_timestamp'].dt.time
    
    baselineList = []
    baselineListBinned = []
    for i in range(len(inSegments)-1):
        print('Query going in...')
        strSQL = 'SELECT bluetooth.observations_201511.measured_time, '\
        'bluetooth.observations_201511.measured_timestamp '\
        'FROM bluetooth.observations_201511 INNER JOIN bluetooth.ref_segments '\
        'ON (bluetooth.observations_201511.analysis_id '\
        '= bluetooth.ref_segments.analysis_id) '\
        'WHERE (bluetooth.ref_segments.startpointname = %(pt1)s '\
        'AND bluetooth.ref_segments.endpointname = %(pt2)s)'\

        baselineList.append(pandasql.read_sql(sql=strSQL,
                                              params={'pt1':inSegments[i],
                                                      'pt2':inSegments[i+1],
                                                      'dstart':startDate,
                                                      'dend':endDate},
                                              con=engine))
    
    for df in baselineList:
        df['measured_timestamp'] = pd.to_datetime(df['measured_timestamp'])
        temp = df.groupby(
        pd.TimeGrouper('5min',key='measured_timestamp')).mean().interpolate()
        temp.reset_index(inplace=True)
        temp['measured_timestamp'] = temp['measured_timestamp'].dt.time
        pt = pd.pivot_table(temp, 
                            index = 'measured_timestamp', 
                            aggfunc = lambda x: np.percentile(x, 50))
        pt.reset_index(inplace = True)
        baselineListBinned.append(pt)
        
    con.close()
    
    return [dfListBinned,baselineListBinned]


def createSegmentListPandas(dfIn, engine):
    '''
    take an incident tmc and direction, return the bt segment and the one directly upstream(if it exists)
    '''
    con = engine.connect()
    segList = []    
    strSQL = 'SELECT * FROM incidents.bt WHERE tmc = %(tmc)s'
    df = pandasql.read_sql(sql=strSQL,
                           params= {'tmc':dfIn['tmc']}, 
                           con=con)
    con.close()
    if len(df) == 2: # len of 2 means the incident is on a tmc that is on the border between 2 bt segments, use those segments for analysis
        #segments in order
        if dfIn['eventdirection'] == 'EB' or dfIn['eventdirection'] == 'NB':
            segList = list(df['bt1'])[::-1] # bt1 column in reversed order
            segList.append(list(df['bt2'])[0]) # 1st element of bt2
            return segList
        
        #segments reversed    
        if dfIn['eventdirection'] == 'WB' or dfIn['eventdirection'] == 'SB':
            segList = list(df['bt1']) #
            segList.append(list(df['bt2'])[-1])
            return segList
            
    elif len(df) == 1: # len of 1 mens the incident is on a tmc that is wholly in a single bt segment, we need to know what the upstream segment is too if it exists
        if dfIn['eventdirection'] == 'EB' or dfIn['eventdirection'] =='NB':
            segList = list(df['bt1'])[::-1] # bt1 column in reversed order
            segList.append(list(df['bt2'])[0])            
            index = NE.index(segList[0])
            if index == 0: 
                return segList
            else:
                segList = [NE[index - 1]] + segList
                return segList

        #segments reversed    
        if dfIn['eventdirection'] == 'WB' or dfIn['eventdirection'] == 'SB':
            segList = list(df['bt1']) # bt1 column in reversed order
            segList.append(list(df['bt2'])[0]) 
            index = SW.index(segList[0])
            if index == 0:
                return segList
            else:
                segList = [SW[index - 1]] + segList
                return segList
    else:
        return segList

    
def readInc(engine):
    '''
    Return dataframe of incidents that are major and have an assoicated tmc
    '''
    sqlStr = 'SELECT * FROM incidents.mvp_temp WHERE (mir = \'Yes\' AND tmc != \'\')'
    return pandasql.read_sql(sql = sqlStr, con = engine)

def integrateDelay(dfList, startDatetime, endDatetime, segLisst):
    
    if dfList == None:
        return 0.

    delay = 0.
   
    for i in range(len(dfList[0])):
        
        y1df = dfList[0][i][(dfList[0][i]['measured_timestamp'] >= startDatetime.time()) 
                            & (dfList[0][i]['measured_timestamp'] <= endDatetime.time())] 
        y2df = dfList[1][i][(dfList[1][i]['measured_timestamp'] >= startDatetime.time()) 
                            & (dfList[1][i]['measured_timestamp'] <= endDatetime.time())] 
        y1 = y1df['measured_time'].as_matrix() 
        y2 = y2df['measured_time'].as_matrix() 
        
        if y1.shape[0] != y2.shape[0]:
            index = min(y1.shape[0],y2.shape[0])
            y1 = y1[:index]
            y2 = y2[:index]
        
        y = y1 - y2 
        
        y = y * 1800. * (5./60.) * 3. *(1./3600.) # 1800 veh/hr * 5/60 mins * 3 lanes * 1hr/3600s
        
        delay += np.sum(y)

    return delay

def plotDelay(dfList, startDatetime, endDatetime, delay, segList):
    
    if dfList == None:
        return None
    
    startTime = startDatetime.time()
    print startTime
    endTime = endDatetime.time()
    print endTime
    
    fig, ax = plt.subplots(2, 1, figsize=(8,8), sharex=False, dpi=300)
    ax[0].set_title(str(segList) + ' - ' + str(startDatetime.date()) + ' - ' + str(int(delay)) + ' vehicle-hours of delay')
    
    for i in range(len(dfList[0])):
        ax[i].plot(dfList[0][i]['measured_timestamp'], dfList[0][i]['measured_time'],'k') #specific day
        ax[i].plot(dfList[1][i]['measured_timestamp'], dfList[1][i]['measured_time'],'b') #baseline
        ax[i].axvline(x=startTime,color='red')
        ax[i].axvline(x=endTime,color='green')
    
    #plt.savefig(str(segList) + ' - ' + str(startDatetime.date()) + ' - ' + str(int(delay)) + ' vehicle-hours of delay'+'.png',format='png')
    plt.show()
    
    return None
    
      
def main():
    
    #read from db and take only DVP and FGG 
    dfInc = readInc(engine)
    dfInc = dfInc[(dfInc['eventlocation'] == 'DVP') | (dfInc['eventlocation'] == 'FGG')]
    
    #create segment list column for reading of travel time
    dfInc['segList'] = dfInc.apply(lambda x: createSegmentListPandas(x,engine), axis=1)
    
    #typecast from str to datetime
    dfInc['startdatetime'] = pd.to_datetime(dfInc['startdatetime'])
    dfInc['enddatetime'] = pd.to_datetime(dfInc['enddatetime'])
    
    #temp filter only for nov 2015 incidents
    dfInc = dfInc[(dfInc['startdatetime'] > datetime.date(2015,11,1)) & (dfInc['enddatetime'] < datetime.date(2015,12,1))]

    delay = 0.
    
    for startDatetime, endDatetime, segList in zip(dfInc['startdatetime'], dfInc['enddatetime'], dfInc['segList']):
        dfList = readSegments(segList, startDatetime, engine)       
        integral = integrateDelay(dfList, startDatetime, endDatetime, segList)
        plotDelay(dfList, startDatetime, endDatetime, integral, segList)
        delay += integral
        
    return delay 
    
if __name__ == '__main__':
    main()