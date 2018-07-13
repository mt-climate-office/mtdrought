import ee
import datetime
import dateutil
import dateutil.parser
import pandas as pd
import time

ee.Initialize()

# # Calculates the Normal statistics
# # and writes them to a new ImageCollection asset

# collection = 'IDAHO_EPSCOR/GRIDMET'
# vars = ['pr','tmmn','tmmx']
# start_date = "1971-01-01"
# end_date = "2001-01-01"

# geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
#     .filter(ee.Filter.eq('Name', 'Montana'))

# imgColl = ee.ImageCollection(collection)\
#     .select(vars)\
#     .filterDate(start_date, end_date)

# reducers = ee.Reducer.mean()\
#     .combine(
#         reducer2 = ee.Reducer.percentile(
#             percentiles = [0,25,50,75,100],
#             outputNames = ["0","25","50","75","100"],
#             maxRaw = 1000),
#         sharedInputs = True)

# for d in range(1,366):
#     print(d)
#     out = ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
#       .reduce(reducer = reducers))\
#       .set('day', d)

#     ee.batch.Export.image.toAsset(
#           image = out,
#           description = "GRIDMET_Normals_" + format(d,'03d'),
#           assetId = "users/bocinsky/GRIDMET_MT_1971-2000_Normals/" + format(d,'03d'),
#           region = geom.geometry().coordinates().getInfo(),
#           scale = 4000
#           ).start()

def appendBands( image, previous ):
  return ee.Image(previous).addBands(image)

def stackCollection ( collection ):
  first = ee.Image(collection.first()).select([])
  return ee.Image(collection.iterate(appendBands, first))
  
def clipMT( image ):
  return image.clip(ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8').filter(ee.Filter.eq('Name', 'Montana')))

def getDate( image ):
  return image.get('system:index')

def getGridmet( start_date = "2018-06-01", end_date = "2019-08-31", base_name = 'GRIDMET'):
    
    collection = 'IDAHO_EPSCOR/GRIDMET'
    variables = ['pr', 'tmmn', 'tmmx']
    
    days = pd.date_range(start_date, end_date).dayofyear.tolist()
    
    gridmet = ee.ImageCollection(collection)\
    .select(variables)\
    .filterDate(start_date, end_date)
    
    normals = getGridmetNormals( days )
      
    montana = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    .filter(ee.Filter.eq('Name', 'Montana'))\
    .geometry()
    
    bbox = montana.bounds().getInfo()
    
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
    
    dates = pd.date_range(pd.to_datetime(ee.Image(gridmet.first()).get('system:index').getInfo()),pd.to_datetime(ee.Image((gridmet.toList(gridmet.size())).get(gridmet.size().getInfo() - 1)).get('system:index').getInfo())).to_pydatetime()
    
    return({
        'prcp': {
            'value': stackCollection(gridmet.select('pr')).reduce('sum').multiply(0.0393701).getDownloadURL(params=value_params),
            'normal': normals.select('^pr.*').reduce('sum').multiply(0.0393701).getDownloadURL(params=normal_params)#,
            # 'series': {
            #     'value': stackCollection(gridmet.select('pr')).multiply(0.0393701).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            #     'normal': stackCollection(normals.select('pr')).multiply(0.0393701).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            # }
        },
        'tmin': {
            'value': stackCollection(gridmet.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduce('mean').getDownloadURL(params=value_params),
            'normal': normals.select('^tmmn.*').reduce('mean').subtract(273.15).multiply(9/5).add(32).getDownloadURL(params=normal_params)#,
            # 'series': {
            #     'value': stackCollection(gridmet.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            #     'normal': stackCollection(normals.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            # }
        },
        'tmax': {
            'value': stackCollection(gridmet.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduce('mean').getDownloadURL(params=value_params),
            'normal': normals.select('^tmmx.*').reduce('mean').subtract(273.15).multiply(9/5).add(32).getDownloadURL(params=normal_params)#,
            # 'series': {
            #     'value': stackCollection(gridmet.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            #     'normal': stackCollection(normals.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            # }
        },
        'dates': dates
    })


def getGridmetNormals ( days ):    
    
    imgColl = ee.ImageCollection("users/bocinsky/GRIDMET_MT_1981-2010_Normals")

    def getDailyMeans( d ):
      return(imgColl.filterMetadata('day', 'equals', d).first())
      
    out = ee.ImageCollection(ee.List(days).map(getDailyMeans))
    
    return(out)


def getDailyClimatology ( collection, vars, start_date, end_date ):
    
    geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    .filter(ee.Filter.eq('Name', 'Montana'))\
    .geometry()
    
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
    
    return(ee.List.sequence(1, 3).map(getDailyMeans).getInfo())

# getDailyClimatology(geom = ee.Geometry.Point(-122.2806, 37.1209))

# stackCollection(getDailyClimatology()).getDownloadURL(params=params)

# Export.image.toDrive({
#   image: landsat,
#   description: 'imageToDriveExample',
#   scale: 30,
#   region: geometry
# })
# stackCollection(getDailyClimatology()).getDownloadURL(params=params)
