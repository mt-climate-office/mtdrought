import ee
import datetime
import dateutil
import dateutil.parser
import pandas as pd
import time

ee.Initialize()

## Calculates the Normals and Midcentury statistics
## and writes them to new ImageCollection assets

collection = 'IDAHO_EPSCOR/MACAv2_METDATA'
vars = ['pr','tasmin','tasmax']
start_date = "2040-01-01"
end_date = "2070-01-01"

geom = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    .filter(ee.Filter.eq('Name', 'Montana'))
    
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
        

    
imgColl = ee.ImageCollection(collection)\
    .select(vars)\
    .filterDate(start_date, end_date)
    
  
def getDailyStats( d ):
      return(ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
      .reduce(reducer = reducers))

def area_reduce (img):
    return(ee.Image(img).reduceRegion(
          reducer = ee.Reducer.mean(),
          geometry = geom,
          crs = 'EPSG:4326',
          scale = 4000,
          maxPixels = 1e9
          ))

ee.List.sequence(1, 3)\
.map(getDailyMeans)\
.map(area_reduce)






for d in range(1,366):
    print(d)
    out = ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
      .reduce(reducer = reducers))\
      .set('day', d)\
      .clip(geom.geometry())
    
    ee.batch.Export.image.toAsset(
          image = out,
          description = "Midcentury_" + str(d),
          assetId = "users/bocinsky/MACAv2_METDATA_MT_2040-2069/" + str(d),
          region = geom.geometry().coordinates().getInfo(),
          scale = 4000
          ).start()


start_date = "1981-01-01"
end_date = "2011-01-01"

for d in range(1,366):
    print(d)
    out = ee.Image(imgColl.filter(ee.Filter.calendarRange(d))\
      .reduce(reducer = reducers))\
      .set('day', d)\
      .clip(geom.geometry())
    
    ee.batch.Export.image.toAsset(
          image = out,
          description = "Normals_" + str(d),
          assetId = "users/bocinsky/MACAv2_METDATA_MT_1981-2010_Normals/" + str(d),
          region = geom.geometry().coordinates().getInfo(),
          scale = 4000
          ).start()







  
  
  
  
  
  
  
def getGridmet( start_date = "2018-06-01", end_date = "2019-08-31", base_name = 'GRIDMET'):
    
    collection = 'IDAHO_EPSCOR/GRIDMET'
    variables = ['pr', 'tmmn', 'tmmx']
    
    days = pd.date_range(start_date, end_date).dayofyear
    
    gridmet = ee.ImageCollection(collection)\
    .select(variables)\
    .filterDate(start_date, end_date)
    
    normals = getGridmetNormals( days )
      
    montana = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')\
    .filter(ee.Filter.eq('Name', 'Montana'))\
    .geometry()
    
    bbox = montana.bounds().getInfo()
    
    params = {
        'name': "GRIDMET_" + start_date + "_" + end_date,
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    dates = pd.date_range(pd.to_datetime(ee.Image(gridmet.first()).get('system:index').getInfo()),pd.to_datetime(ee.Image((gridmet.toList(gridmet.size())).get(gridmet.size().getInfo() - 1)).get('system:index').getInfo())).to_pydatetime()
    
    return({
        'prcp': {
            'value': stackCollection(gridmet.select('pr')).multiply(0.0393701).reduce('sum').getDownloadURL(params=params),
            'normal': stackCollection(normals.select('pr')).multiply(0.0393701).reduce('sum').getDownloadURL(params=params)#,
            # 'series': {
            #     'value': stackCollection(gridmet.select('pr')).multiply(0.0393701).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            #     'normal': stackCollection(normals.select('pr')).multiply(0.0393701).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            # }
        },
        'tmin': {
            'value': stackCollection(gridmet.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduce('mean').getDownloadURL(params=params),
            'normal': stackCollection(normals.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduce('mean').getDownloadURL(params=params)#,
            # 'series': {
            #     'value': stackCollection(gridmet.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            #     'normal': stackCollection(normals.select('tmmn')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            # }
        },
        'tmax': {
            'value': stackCollection(gridmet.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduce('mean').getDownloadURL(params=params),
            'normal': stackCollection(normals.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduce('mean').getDownloadURL(params=params)#,
            # 'series': {
            #     'value': stackCollection(gridmet.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            #     'normal': stackCollection(normals.select('tmmx')).subtract(273.15).multiply(9/5).add(32).reduceRegion(reducer = ee.Reducer.mean(), geometry = montana, scale = 4000, maxPixels = 1e9).getInfo(),
            # }
        },
        'dates': dates
    })



def getMACAv2Midcentury ( days ):    
    
    collection = "users/bocinsky/MACAv2_METDATA_MT_2040-2069"
    
    out = ee.ImageCollection(collection)\
    .toList(count = ee.ImageCollection(collection).size())\
    .map(area_reduce)
    
    
    \
    .select(["pr", "tasmin", "tasmax"])\
    .filterDate('1981-01-01', '2010-12-31')

    def getDailyMeans( d ):
      return(imgColl.filter(ee.Filter.calendarRange(d))\
      .mean()\
      .set('day', d))
      
    out = ee.ImageCollection(ee.List(list(days)).map(getDailyMeans))
    
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
    
    return(ee.List.sequence(1, 365).map(getDailyMeans).getInfo())

# getDailyClimatology(geom = ee.Geometry.Point(-122.2806, 37.1209))

# stackCollection(getDailyClimatology()).getDownloadURL(params=params)

# Export.image.toDrive({
#   image: landsat,
#   description: 'imageToDriveExample',
#   scale: 30,
#   region: geometry
# })
# stackCollection(getDailyClimatology()).getDownloadURL(params=params)
