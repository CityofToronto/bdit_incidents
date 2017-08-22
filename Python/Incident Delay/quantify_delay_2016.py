import datetime
from datetime import timedelta
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pandas.io.sql as pandasql
import ast
import ConfigParser
from sqlalchemy import create_engine

plt.style.use('ggplot')
NE = [u'A',u'B',u'B1',u'C',u'D',u'E',u'F',u'G',u'H',u'I',u'J',u'K']
SW = NE[::-1]

config = ConfigParser.ConfigParser()
config.read("C:\Users\dolejar\default.cfg")
dbset = config._sections['DBSETTINGS']
dbStr = 'postgresql://'+dbset['user']+':'+dbset['password']+'@'+dbset['host']+':'+dbset['port']+'/'+dbset['database']
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
    #sqlStr = 'SELECT * FROM incidents.mvp_2016 WHERE ("MIR" = \'Yes\' AND tmc != \'\')'
    sqlStr = 'SELECT * FROM incidents.mvp_2016 WHERE tmc != \'\''
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
   
    fig, ax = plt.subplots(2, 1, figsize=(22,16), sharex=False, dpi=300)
    #fig.text(0.0, 0.5, 'Travel Time (minutes)', va='center', rotation='vertical')
    #plt.setp(ax, xticks=[datetime.time(0,0,0),datetime.time(1,0,0),datetime.time(2,0,0),datetime.time(3,0,0),datetime.time(4,0,0),datetime.time(5,0,0),datetime.time(6,0,0),datetime.time(7,0,0),datetime.time(8,0,0),datetime.time(9,0,0),datetime.time(10,00,00),datetime.time(11,00,00),datetime.time(12,00,00),datetime.time(13,00,00),datetime.time(14,00,00),datetime.time(15,00,00),datetime.time(16,00,00),datetime.time(17,00,00),datetime.time(18,00,00),datetime.time(19,00,00),datetime.time(20,00,00),datetime.time(21,00,00),datetime.time(22,00,00),datetime.time(23,00,00)])
    plt.setp(ax, xticks=[datetime.time(0,0,0),datetime.time(6,0,0),datetime.time(12,00,00),datetime.time(18,00,00)])
        
    '''
    ax[0].set_title(str(segList) 
                    + ' - ' 
                    + str(startDatetime.date()) 
                    + ' - ' 
                    + str(int(delay)) 
                    + ' vehicle-hours of delay')
    '''
    ax[0].set_title('Incident Segment')
    ax[1].set_title('Upstream Segment')    
    for i in range(len(dfList[0])):
        ax[i].plot(dfList[0][i]['datetime_bin'], dfList[0][i]['tt']/60., color='#094c82',lw=3) #specific day
        ax[i].plot(dfList[1][i]['datetime_bin'], dfList[1][i]['tt']/60.,'k',lw=3) #baseline
        #ax[i].fill_between(dfList[0][i]['datetime_bin'], 
        #                   dfList[0][i]['tt']/60., 
        #                   dfList[1][i]['tt']/60.)
        ax[i].axvline(x=startTime,color='#f2766d',lw=3)
        ax[i].axvline(x=endTime,color='#f2766d',lw=3)
        ax[i].set_xlim([datetime.time(00,00,00),datetime.time(23,55,00)])
        ax[i].set_xlabel('')
    
    #plt.savefig(str(segList) + ' - ' + str(startDatetime.date()) + ' - ' + str(int(delay)) + ' vehicle-hours of delay'+'.png',format='png')
    plt.savefig('incident_poster_plot2.png',format='png')    
    plt.show()
    
    return None
    
      
def main():
    con = engine.connect()
    
    print '1'
    #read from db and take only DVP and FGG 
    dfInc = readInc(con)
    dfInc['Delay'] = None
    dfInc = dfInc[(dfInc['EventLocation'] == 'DVP') | (dfInc['EventLocation'] == 'FGG')]
    
    #dfInc = dfInc[df['EventDescription'].str.contains("collision")]
    
    print '2'
    #take only 2016 data
    dfInc['StartDateTime'] = pd.to_datetime(dfInc['StartDateTime'])
    dfInc['EndDateTime'] = pd.to_datetime(dfInc['EndDateTime'])
    dfInc = dfInc[(dfInc['StartDateTime'] > datetime.date(2016,2,1)) & (dfInc['EndDateTime'] < datetime.date(2017,1,1))]

    print '3'
    #create the segList column
    dfInc['segList'] = dfInc.apply(lambda x: createSegmentListPandas(x,con), axis=1)
    
    delay = 0.
    
    '''
    for startDatetime, endDatetime, segListStr in zip(dfInc['StartDateTime'], dfInc['EndDateTime'], dfInc['segList']):
        segList = ast.literal_eval(segListStr)
        dfList = readSegments(segList, startDatetime, con)       
        integral = integrateDelay(dfList, startDatetime, endDatetime, segList, con)
        #plotDelay(dfList, startDatetime, endDatetime, integral, segList)
        delay += integral
        
    print '4'
    for i in [dfInc.index[0]]:
        print dfInc['StartDateTime'][i]
        segList = ast.literal_eval(dfInc['segList'][i])
        dfList = readSegments(segList, dfInc['StartDateTime'][i], con)       
        integral = integrateDelay(dfList, dfInc['StartDateTime'][i], dfInc['EndDateTime'][i], segList, con)
        dfInc['Delay'][i] = integral
        plotDelay(dfList, dfInc['StartDateTime'][i], dfInc['EndDateTime'][i], integral, segList)
        delay += integral
    '''
    #dfInc.to_csv('feb-decInc.csv')
    con.close()
    engine.dispose()
    
    return dfInc


def plotLoop(dfInc, index, engine):
    con = engine.connect()
    delay = 0.
    for i in [dfInc.index[index]]:
        print dfInc['StartDateTime'][i]
        segList = ast.literal_eval(dfInc['segList'][i])
        dfList = readSegments(segList, dfInc['StartDateTime'][i], con)       
        integral = integrateDelay(dfList, dfInc['StartDateTime'][i], dfInc['EndDateTime'][i], segList, con)
        dfInc['Delay'][i] = integral
        plotDelay(dfList, dfInc['StartDateTime'][i], dfInc['EndDateTime'][i], integral, segList)
        delay += integral
    con.close()
    engine.dispose()
    return 
    

'''
if __name__ == '__main__':
    main()
'''

    

