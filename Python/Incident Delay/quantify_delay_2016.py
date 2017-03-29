import datetime
from datetime import timedelta
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pandas.io.sql as pandasql
import ast
from sqlalchemy import create_engine

NE = [u'A',u'B',u'B1',u'C',u'D',u'E',u'F',u'G',u'H',u'I',u'J',u'K']
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
        
    
    dfList = []

    startDate = startDate.date()
    endDate = startDate + timedelta(days=1)
    
    for i in range(len(inSegments)-1):
        print('Query going in...')
        strSQL = 'SELECT bluetooth.aggr_5min.datetime_bin, '\
        'bluetooth.aggr_5min.tt '\
        'FROM bluetooth.aggr_5min INNER JOIN bluetooth.ref_segments '\
        'ON (bluetooth.aggr_5min.analysis_id '\
        '= bluetooth.ref_segments.analysis_id) '\
        'WHERE (bluetooth.ref_segments.startpointname = %(pt1)s '\
        'AND bluetooth.ref_segments.endpointname = %(pt2)s '\
        'AND bluetooth.aggr_5min.datetime_bin >= %(dstart)s '\
        'AND bluetooth.aggr_5min.datetime_bin < %(dend)s)'
        dfList.append(pandasql.read_sql(sql=strSQL,
                                        params={'pt1':inSegments[i],
                                                'pt2':inSegments[i+1],
                                                'dstart':startDate,
                                                'dend':endDate},
                                        con=engine))
    
    for df in dfList:
        df['datetime_bin'] = pd.to_datetime(df['datetime_bin'])
        df['datetime_bin'] = df['datetime_bin'].dt.time
    
    baselineList = []
    baselineListBinned = []

    for i in range(len(inSegments)-1):
        print('Query going in...')
        strSQL = 'SELECT bluetooth.aggr_5min.datetime_bin, '\
        'bluetooth.aggr_5min.tt '\
        'FROM bluetooth.aggr_5min INNER JOIN bluetooth.ref_segments '\
        'ON (bluetooth.aggr_5min.analysis_id '\
        '= bluetooth.ref_segments.analysis_id) '\
        'WHERE (bluetooth.ref_segments.startpointname = %(pt1)s '\
        'AND bluetooth.ref_segments.endpointname = %(pt2)s '\
        'AND bluetooth.aggr_5min.datetime_bin >= %(dstart)s '\
        'AND bluetooth.aggr_5min.datetime_bin < %(dend)s)'

        baselineList.append(pandasql.read_sql(sql=strSQL,
                                              params={'pt1':inSegments[i],
                                                      'pt2':inSegments[i+1],
                                                      'dstart':datetime.date(2015,1,1),
                                                      'dend':datetime.date(2016,1,1)},
                                              con=engine))
    
    for df in baselineList:
        df['datetime_bin'] = pd.to_datetime(df['datetime_bin'])
        df['datetime_bin'] = df['datetime_bin'].dt.time
        pt = pd.pivot_table(df, 
                            index = 'datetime_bin', 
                            aggfunc = lambda x: np.percentile(x, 50))
        pt.reset_index(inplace = True)
        baselineListBinned.append(pt)
  
    return [dfList,baselineListBinned]


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
        if dfIn['EventDirection'] == 'EB' or dfIn['EventDirection'] == 'NB':
            segList = list(df['bt1'])[::-1] # bt1 column in reversed order
            segList.append(list(df['bt2'])[0]) # 1st element of bt2         
            return str(segList)
        
        #segments reversed    
        if dfIn['EventDirection'] == 'WB' or dfIn['EventDirection'] == 'SB':
            segList = list(df['bt1']) #
            segList.append(list(df['bt2'])[-1])
            return str(segList)
            
    elif len(df) == 1: # len of 1 mens the incident is on a tmc that is wholly in a single bt segment, we need to know what the upstream segment is too if it exists
        if dfIn['EventDirection'] == 'EB' or dfIn['EventDirection'] =='NB':
            segList = list(df['bt1'])[::-1] # bt1 column in reversed order
            segList.append(list(df['bt2'])[0])            
            index = NE.index(segList[0])
            if index == 0: 
                return str(segList)
            else:
                segList = [NE[index - 1]] + segList
                return str(segList)

        #segments reversed    
        if dfIn['EventDirection'] == 'WB' or dfIn['EventDirection'] == 'SB':
            segList = list(df['bt1']) # bt1 column in reversed order
            segList.append(list(df['bt2'])[0]) 
            index = SW.index(segList[0])
            if index == 0:
                return str(segList)
            else:
                segList = [SW[index - 1]] + segList
                return str(segList)
    else:
        return str(segList)
    
def readInc(engine):
    '''
    Return dataframe of incidents that are major and have an assoicated tmc
    '''
    sqlStr = 'SELECT * FROM incidents.mvp_2016 WHERE ("MIR" = \'Yes\' AND tmc != \'\')'
    return pandasql.read_sql(sql = sqlStr, con = engine)

def readVolume(startPoint, endPoint, hour, engine):
    sqlStr = 'Select "volume" from incidents.volumes '\
             'WHERE ( "startpointname" = %(start)s '\
             'AND "endpointname" = %(end)s '\
             'AND "hour" = %(hour)s)'
    df = pandasql.read_sql(sql=sqlStr, 
                           params = {'start':startPoint,
                                     'end':endPoint,
                                     'hour':hour},
                           con=engine)
    return df.volume[0]

def integrateDelay(dfList, startDatetime, endDatetime, segList, engine):
    
    if dfList == None:
        return 0.

    delay = 0.
   
    for i in range(len(dfList[0])):
        
        y1df = dfList[0][i][(dfList[0][i]['datetime_bin'] >= startDatetime.time()) 
                            & (dfList[0][i]['datetime_bin'] <= endDatetime.time())] 
        y2df = dfList[1][i][(dfList[1][i]['datetime_bin'] >= startDatetime.time()) 
                            & (dfList[1][i]['datetime_bin'] <= endDatetime.time())] 
        y1 = y1df['tt'].as_matrix() 
        y2 = y2df['tt'].as_matrix() 
        
        if y1.shape[0] != y2.shape[0]:
            index = min(y1.shape[0],y2.shape[0])
            y1 = y1[:index]
            y2 = y2[:index]
        
        y = y1 - y2 
        
        volume = readVolume(segList[i], 
                            segList[i+1], 
                            startDatetime.time().hour, 
                            engine)
                            
        y = y * volume * (5./60.) * (1./3600.) # volume * 5/60 mins * 1hr/3600s
        
        delay += np.sum(y)

    return delay

def plotDelay(dfList, startDatetime, endDatetime, delay, segList):
    
    if dfList == None:
        print 'Empty dfList'
        return None
    
    startTime = startDatetime.time()
    endTime = endDatetime.time()
   
    fig, ax = plt.subplots(2, 1, figsize=(8,8), sharex=False, dpi=300)
    ax[0].set_title(str(segList) 
                    + ' - ' 
                    + str(startDatetime.date()) 
                    + ' - ' 
                    + str(int(delay)) 
                    + ' vehicle-hours of delay')
    
    for i in range(len(dfList[0])):
        ax[i].plot(dfList[0][i]['datetime_bin'], dfList[0][i]['tt'],'k') #specific day
        ax[i].plot(dfList[1][i]['datetime_bin'], dfList[1][i]['tt'],'b') #baseline
        ax[i].axvline(x=startTime,color='red')
        ax[i].axvline(x=endTime,color='green')
    
    #plt.savefig(str(segList) + ' - ' + str(startDatetime.date()) + ' - ' + str(int(delay)) + ' vehicle-hours of delay'+'.png',format='png')
    plt.show()
    
    return None
    
      
def main():
    con = engine.connect()
    
    #read from db and take only DVP and FGG 
    dfInc = readInc(con)
    dfInc = dfInc[(dfInc['EventLocation'] == 'DVP') | (dfInc['EventLocation'] == 'FGG')]
    
    #take only 2016 data
    dfInc['StartDateTime'] = pd.to_datetime(dfInc['StartDateTime'])
    dfInc['EndDateTime'] = pd.to_datetime(dfInc['EndDateTime'])
    dfInc = dfInc[(dfInc['StartDateTime'] > datetime.date(2016,12,1)) & (dfInc['EndDateTime'] < datetime.date(2017,1,1))]

    #create the segList column
    dfInc['segList'] = dfInc.apply(lambda x: createSegmentListPandas(x,con), axis=1)
    
    delay = 0.
    
    for startDatetime, endDatetime, segListStr in zip(dfInc['StartDateTime'], dfInc['EndDateTime'], dfInc['segList']):
        segList = ast.literal_eval(segListStr)
        dfList = readSegments(segList, startDatetime, con)       
        integral = integrateDelay(dfList, startDatetime, endDatetime, segList, con)
        #plotDelay(dfList, startDatetime, endDatetime, integral, segList)
        delay += integral
    
    con.close()
    engine.dispose()
    
    return delay

if __name__ == '__main__':
    main()

    

