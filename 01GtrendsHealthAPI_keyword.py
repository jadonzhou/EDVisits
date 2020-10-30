##
##
##Each data point is a single number representing a term at a certain point in time. The number of data points in total is calculated as (number of terms * time * resolution). For example a call specifying 5 terms over 3 weeks in day resolution ==> 5 * 3 * 7 = 105 data points.
##
##Examples of acceptable calls - remember, you can issue 5000 of these per day.
##* 5 terms, 1 year, day resolution
##* 30 terms, 1 year, week resolution
##* 30 terms, 5 years, month resolution


#!/usr/bin/python
import datetime
import pandas as pd
import time

from apiclient.discovery import build


# ------ Insert your API key in the string below. -------
API_KEY = ' AIzaSyBO33r2QFMwl2S0h-FM6JiajPfa6bxbvTQ'

SERVER = 'https://www.googleapis.com'
API_VERSION = 'v1beta'
DISCOVERY_URL_SUFFIX = '/discovery/v1/apis/trends/' + API_VERSION + '/rest'
DISCOVERY_URL = SERVER + DISCOVERY_URL_SUFFIX

MAX_QUERIES = 30


def DateToISOString(datestring):
  """Convert date from (eg) 'Jul 04 2004' to '2004-07-11'.

  Args:
    datestring: A date in the format 'Jul 11 2004', 'Jul 2004', or '2004'

  Returns:
    The same date in the format '2004-11-04'

  Raises:
     ValueError: when date doesn't match one of the three expected formats.
  """

  try:
    new_date = datetime.datetime.strptime(datestring, '%b %d %Y')
  except ValueError:
    try:
      new_date = datetime.datetime.strptime(datestring, '%b %Y')
    except ValueError:
      try:
        new_date = datetime.datetime.strptime(datestring, '%Y')
      except:
        raise ValueError("Date doesn't match any of '%b %d %Y', '%b %Y', '%Y'.")

  return new_date.strftime('%Y-%m-%d')


def GetQueryVolumes(queries, start_date, end_date,
                    geo='US', geo_level='dma', frequency='year'):
  """Extract query volumes from Flu Trends API.

  Args:
    queries: A list of all queries to use.
    start_date: Start date for timelines, in form YYYY-MM-DD.
    end_date: End date for timelines, in form YYYY-MM-DD.
    geo: The code for the geography of interest which can be either country
         (eg "US"), region (eg "US-NY") or DMA (eg "501").
    geo_level: The granularity for the geo limitation. Can be "country",
               "region", or "dma"
    frequency: The time resolution at which to pull queries. One of "day",
               "week", "month", "year".

  Returns:
    A list of lists (one row per date) that can be output by csv.writer.

  Raises:
    ValueError: when geo_level is not one of "country", "region" or "dma".
  """

  if not API_KEY:
    raise ValueError('API_KEY not set.')

  service = build('trends', API_VERSION,
                  developerKey=API_KEY,
                  discoveryServiceUrl=DISCOVERY_URL)

  dat = {}

  # Note that the API only allows querying 30 queries in one request. In
  # the event that we want to use more queries than that, we need to break
  # our request up into batches of 30.
  batch_intervals = range(0, len(queries), MAX_QUERIES)

  for batch_start in batch_intervals:
    batch_end = min(batch_start + MAX_QUERIES, len(queries))
    query_batch = queries[batch_start:batch_end]

    # Make API query
    if geo_level == 'country':
      # Country format is ISO-3166-2 (2-letters), e.g. 'US'
      req = service.getTimelinesForHealth(terms=query_batch,
                                          time_startDate=start_date,
                                          time_endDate=end_date,
                                          timelineResolution=frequency,
                                          geoRestriction_country=geo)
    elif geo_level == 'dma':
      # See https://support.google.com/richmedia/answer/2745487
      req = service.getTimelinesForHealth(terms=query_batch,
                                          time_startDate=start_date,
                                          time_endDate=end_date,
                                          timelineResolution=frequency,
                                          geoRestriction_dma=geo)
    elif geo_level == 'region':
      # Region format is ISO-3166-2 (4-letters), e.g. 'US-NY' (see more examples
      # here: en.wikipedia.org/wiki/ISO_3166-2:US)
      req = service.getTimelinesForHealth(terms=query_batch,
                                          time_startDate=start_date,
                                          time_endDate=end_date,
                                          timelineResolution=frequency,
                                          geoRestriction_region=geo)
    else:
      raise ValueError("geo_type must be one of 'country', 'region' or 'dma'")

    res = req.execute()

    # Sleep for 1 second so as to avoid hittting rate limiting.
    time.sleep(1)

    # Convert the data from the API into a dictionary of the form
    # {(query, date): count, ...}
    res_dict = {(line[u'term'], DateToISOString(point[u'date'])):
                point[u'value']
                for line in res[u'lines']
                for point in line[u'points']}

    # Update the global results dictionary with this batch's results.
    dat.update(res_dict)

  # Make the list of lists that will be the output of the function
  res = [['date'] + queries]
  for date in sorted(list(set([x[1] for x in dat]))):
    vals = [dat.get((term, date), 0) for term in queries]
    res.append([date] + vals)

  return res


def main():

    keyWords = ['Apache','Apache Indian','Heroin','Dope','Brown Sugar','White Horse',
                'Golden Girls','China White','Black Tar','Big H','Grey Death',
                'Murder 8','Black Stuff','Dance fever','Hillbilly Heroin','Speed Balling',
                'Speedballing','Hell Dust','Goodfella',
                
                'Tramadol','Hydrocodone','Oxycodone','Morphine','Methadone',
                'Codeine','Fentanyl','Meperidine','Kadian','Oxymorphone',
                'Avinza'
                ]

    stateList = ["US-AL","US-AK","US-AZ","US-AR","US-CA",
                "US-CO","US-CT","US-DE","US-FL","US-GA",
                "US-HI","US-ID","US-IL","US-IN","US-IA",
                "US-KS","US-KY","US-LA","US-ME","US-MD",
                "US-MA","US-MI","US-MN","US-MS","US-MO",
                "US-MT","US-NE","US-NV","US-NH","US-NJ",
                "US-NM","US-NY","US-NC","US-ND","US-OH",
                "US-OK","US-OR","US-PA","US-RI","US-SC",
                "US-SD","US-TN","US-TX","US-UT","US-VT",
                "US-VA","US-WA","US-WV","US-WI","US-WY"]

    dmaList = ['506',#Boston
               '602',#Chicago
               '751',#Denver
               '505',#Detroit
               '613',#Minneapolis
               '501',#New York
               '753',#Phoenix
               '807',#San Francisco
               '819']#Seattle

##              '500','501','502','503','504','505','506','507','508','509','510','511','512','513','514','515','516','517','518','519',
##              '520','521','522','523','524','525','526','527','528','529','530','531','532','533','534','535','536','537','538','539',
##              '540','541','542','543','544','545','546','547','548','549','550','551','552','553','554','555','556','557','558','559',
##              '560','561','563','564','565','566','567','569','570','571','573','574','575','576','577','581','582','583','584','588',
##              '592','596','597','598','600','602','603','604','605','606','609','610','611','612','613','616','617','618','619','622',
##              '623','624','625','626','627','628','630','631','632','633','634','635','636','637','638','639','640','641','642','643',
##              '644','647','648','649','650','651','652','656','657','658','659','661','662','669','670','671','673','675','676','678',
##              '679','682','686','687','691','692','693','698','702','705','709','710','711','716','717','718','722','724','725','734',
##              '736','737','740','743','744','745','746','747','749','751','752','753','754','755','756','757','758','759','760','762',
##              '764','765','766','767','770','771','773','789','790','798','800','801','802','803','804','807','810','811','813','819',
##              '820','821','825','828','839','855','862','866','868','881']

    df = []


  
    for geography in dmaList:#['US']:
        trends = GetQueryVolumes(keyWords,
            start_date='2004-01-01',
            end_date='2016-12-31',
            geo=geography,
            geo_level='dma', #Can be "country", "region" (state), or "dma"
            frequency='year') # Can be "day","week", "month", "year"


        trends = pd.DataFrame(trends[1:],columns=trends[0])
        trends.insert(0,'geo',pd.Series(geography, index=trends.index))
        #trends.insert(1, 'weekcode', range(0, 0 + len(trends)))
        df.insert(-1,trends)
    

    data = pd.concat(df, ignore_index=True)
    #data.rename(columns={'geo':'state','date':'weekstart'}, inplace=True)
    data['geo']=data['geo'].str.replace('US-',"")
    data['date'] = data['date'].apply(lambda x: int(str(x)[:4])) #changes date to year
    #print(data)
    data.to_csv(r"C:\Users\jurata\Desktop\Opioids_YearDMA9.csv",sep=",",index=False)

if __name__ == '__main__':
  main()
