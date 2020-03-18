import ee
import datetime
import dateutil
import dateutil.parser
import pandas as pd
import time
import re

ee.Initialize()

reducers = ee.Reducer.mean()\
  .combine(
    reducer2 = ee.Reducer.percentile(
      percentiles = [0,25,50,75,100], 
      outputNames = ["0","25","50","75","100"],
      maxRaw = 1000),
    sharedInputs = True)


# Calculates the Normal statistics
# and writes them to a new ImageCollection asset

collection = 'IDAHO_EPSCOR/GRIDMET'
vars = ['pr','tmmn','tmmx']
start_date = "1971-01-01"
end_date = "2001-01-01"
out_path = 'users/bocinsky/GRIDMET_MT_1971-2000_Normals'

# geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
#     .filter(ee.Filter.eq('Name', 'Montana'))
    
geom = ee.FeatureCollection("TIGER/2018/States")\
    .filter(ee.Filter.eq('NAME', 'Montana'))

imgColl = ee.ImageCollection(collection)\
    .select(vars)\
    .filterDate(start_date, end_date)

reducers = ee.Reducer.mean()\
    .combine(
        reducer2 = ee.Reducer.percentile(
            percentiles = [0,25,50,75,100],
            outputNames = ["0","25","50","75","100"],
            maxRaw = 1000),
        sharedInputs = True)
        
complete = []
for value in ee.data.getList({'id': out_path}):
    complete.append(int(value['id'].rsplit('/', 1)[-1]))

for d in range(1,366):
    if d in complete:
      continue
    print(d)
    out = ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
      .reduce(reducer = reducers))\
      .set('day', d)

    ee.batch.Export.image.toAsset(
          image = out,
          description = "GRIDMET_Normals_" + format(d,'03d'),
          # assetId = "users/bocinsky/gridmet_test/" + format(d,'03d'),
          assetId = out_path + '/' + format(d,'03d'),
          region = geom.geometry().coordinates().getInfo(),
          scale = 4000
          ).start()

# The 1981--2010 normals
start_date = "1981-01-01"
end_date = "2011-01-01"
out_path = 'users/bocinsky/GRIDMET_MT_1981-2010_Normals'

imgColl = ee.ImageCollection(collection)\
    .select(vars)\
    .filterDate(start_date, end_date)

complete = []
for value in ee.data.getList({'id': out_path}):
    complete.append(int(value['id'].rsplit('/', 1)[-1]))

for d in range(1,366):
    if d in complete:
      continue
    print(d)
    out = ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
      .reduce(reducer = reducers))\
      .set('day', d)

    ee.batch.Export.image.toAsset(
          image = out,
          description = "GRIDMET_Normals_" + format(d,'03d'),
          # assetId = "users/bocinsky/gridmet_test/" + format(d,'03d'),
          assetId = out_path + '/' + format(d,'03d'),
          region = geom.geometry().coordinates().getInfo(),
          scale = 4000
          ).start()
