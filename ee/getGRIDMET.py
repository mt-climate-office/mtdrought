import ee
import datetime
import dateutil
import dateutil.parser
import pandas as pd
import time

ee.Initialize()

reducers = ee.Reducer.mean()\
  .combine(
    reducer2 = ee.Reducer.percentile(
      percentiles = [0,25,50,75,100], 
      outputNames = ["0","25","50","75","100"],
      maxRaw = 1000),
    sharedInputs = True)

def appendBands( image, previous ):
  return ee.Image(previous).addBands(image)

# Turns imageCollection into multiband image
def stackCollection ( collection ):
  first = ee.Image(collection.first()).select([])
  return ee.Image(collection.iterate(appendBands, first))
  
def clipMT( image ):
  # return image.clip(ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8').filter(ee.Filter.eq('Name', 'Montana')))
  return image.clip(ee.FeatureCollection("TIGER/2018/States").filter(ee.Filter.eq('NAME', 'Montana')))

def getDate( image ):
  return image.get('system:index')

def getGridmet( collection, vars, start_date, end_date ):
    
    days = pd.date_range(start_date, end_date).dayofyear.tolist()
    
    gridmet = ee.ImageCollection(collection)\
    .filterDate(start_date, end_date)
    
    normals = getGridmetNormals( days )
      
    # bounds = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    # .filter(ee.Filter.eq('Name', 'Montana'))\
    # .geometry()
    
    bounds = ee.FeatureCollection("TIGER/2018/States")\
      .filter(ee.Filter.eq('NAME', 'Montana'))\
      .geometry()
    
    bbox = bounds.bounds().getInfo()
    
    value_params = {
        'name': "value",
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    normal_params = {
        'name': "normal",
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    first_date = pd.to_datetime(ee.Image(gridmet.first()).get('system:index').getInfo())
    last_date = pd.to_datetime(ee.Image((gridmet.toList(gridmet.size())).get(gridmet.size().getInfo() - 1)).get('system:index').getInfo())
    dates = pd.date_range(first_date,last_date).to_pydatetime()
    
    out = {
        key: {
            'value': gridmet.select(key).reduce(value).clip(bounds).getDownloadURL(params=value_params),
            'normal': normals.select('^' + key + '.*').reduce(value).clip(bounds).getDownloadURL(params=normal_params)#,
        } for key, value in vars.items()
      }
      
    out['dates'] = dates
    
    return(out)

def getGridmetNormals ( days, collection = "users/bocinsky/GRIDMET_MT_1981-2010_Normals" ):    
    
    imgColl = ee.ImageCollection(collection)

    def getDailyMeans( d ):
      return(imgColl.filterMetadata('day', 'equals', d).first())
      
    out = ee.ImageCollection(ee.List(days).map(getDailyMeans))
    
    return(out)

def getDailySeries ( collection, vars, start_date, end_date ):
    
    # geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    # .filter(ee.Filter.eq('Name', 'Montana'))\
    # .geometry()
    
    geom = ee.FeatureCollection("TIGER/2018/States")\
      .filter(ee.Filter.eq('NAME', 'Montana'))\
      .geometry()
    
    imgColl = ee.ImageCollection(collection)\
    .select(vars)\
    .filterDate(start_date, end_date)
    
    first_date = pd.to_datetime(ee.Image(imgColl.first()).get('system:index').getInfo())
    last_date = pd.to_datetime(ee.Image((imgColl.toList(imgColl.size())).get(imgColl.size().getInfo() - 1)).get('system:index').getInfo())
    dates = pd.date_range(first_date,last_date).to_pydatetime()
    
    return({
      'data': stackCollection(imgColl)\
      .reduceRegion(reducer = ee.Reducer.mean(),
          geometry = geom,
          crs = 'EPSG:4326',
          scale = 4000,
          maxPixels = 1e9)\
          .getInfo(),
          'dates': dates
    })


def getDailyClimatology ( collection, vars, start_date, end_date ):
    
    # geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    # .filter(ee.Filter.eq('Name', 'Montana'))\
    # .geometry()
    
    geom = ee.FeatureCollection("TIGER/2018/States")\
      .filter(ee.Filter.eq('NAME', 'Montana'))\
      .geometry()
    
    imgColl = ee.ImageCollection(collection)\
    .select(vars)\
    .filterDate(start_date, end_date)
  
    def getDailyMeans( d ):
      return(ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
      .reduce(reducer = reducers))\
      .reduceRegion(
          reducer = ee.Reducer.mean(),
          geometry = geom,
          crs = 'EPSG:4326',
          scale = 4000,
          maxPixels = 1e9
          ))
    
    return(ee.List.sequence(1, 366).map(getDailyMeans).getInfo())

def calculateENSOClimatology( dates, name = 'GRIDMET_MT_1981-2010_ENSO_Normals' ):
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
  out_path = 'users/bocinsky/' + name

  # geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
  #   .filter(ee.Filter.eq('Name', 'Montana'))
  
  geom = ee.FeatureCollection("TIGER/2018/States")\
    .filter(ee.Filter.eq('NAME', 'Montana'))
    
  
  imgColl = ee.ImageCollection(collection)\
    .select(vars)\
    .filter(ee.Filter.inList('system:index',  ee.List(dates)))
  
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
          description = name + '_' + format(d,'03d'),
          assetId = out_path + '/' + format(d,'03d'),
          region = geom.geometry().coordinates().getInfo(),
          scale = 4000
          ).start()

def getENSO ( collection, vars, start_date, end_date ):
    
    days = pd.date_range(start_date, end_date).dayofyear.tolist()
    
    enso = getGridmetNormals(days = days, collection = collection)
    
    normals = getGridmetNormals( days = days )
      
    # bounds = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    # .filter(ee.Filter.eq('Name', 'Montana'))\
    # .geometry()
    
    bounds = ee.FeatureCollection("TIGER/2018/States")\
      .filter(ee.Filter.eq('NAME', 'Montana'))\
      .geometry()
    
    bbox = bounds.bounds().getInfo()
    
    value_params = {
        'name': "value",
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    normal_params = {
        'name': "normal",
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    out = {
        key: {
            'value': enso.select('^' + key + '.*').reduce(value).clip(bounds).getDownloadURL(params=value_params),
            'normal': normals.select('^' + key + '.*').reduce(value).clip(bounds).getDownloadURL(params=normal_params)#,
        } for key, value in vars.items()
      }
    
    return(out)

def getENSOSeries ( collection ):
    
    # geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    # .filter(ee.Filter.eq('Name', 'Montana'))\
    # .geometry()
    
    geom = ee.FeatureCollection("TIGER/2018/States")\
    .filter(ee.Filter.eq('NAME', 'Montana'))\
    .geometry()
    
    imgColl = ee.ImageCollection(collection)
  
    def getDailyMeans( d ):
      return(ee.Image(imgColl.filter(ee.Filter.eq( 'day', d )).first())\
        .reduceRegion(
          reducer = ee.Reducer.mean(),
          geometry = geom,
          crs = 'EPSG:4326',
          scale = 4000,
          maxPixels = 1e9
        ))

    return(ee.List.sequence(1, 365).map(getDailyMeans).getInfo())
    
    # days = pd.date_range('2017-01-01', '2017-12-31').dayofyear.tolist()
    # 
    # enso = getGridmetNormals(days = days, collection = collection)
    # 
    # normals = getGridmetNormals( days = days )
    # 
    # 
    # 
    # out = {
    #     key: {
    #         'value': enso.select('^' + key + '.*').reduce(value).reduceRegion(
    #       reducer = ee.Reducer.mean(),
    #       geometry = bounds,
    #       crs = 'EPSG:4326',
    #       scale = 4000,
    #       maxPixels = 1e9
    #       ).getInfo(),
    #         'normal': normals.select('^' + key + '.*').reduce(value).reduceRegion(
    #       reducer = ee.Reducer.mean(),
    #       geometry = bounds,
    #       crs = 'EPSG:4326',
    #       scale = 4000,
    #       maxPixels = 1e9
    #       ).getInfo()#,
    #     } for key, value in vars.items()
    #   }
    # 
    # return(out)
