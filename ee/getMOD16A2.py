import ee

ee.Initialize()

def appendBands( image, previous ):
  return ee.Image(previous).addBands(image)

def stackCollection ( collection ):
  first = ee.Image(collection.first()).select([])
  return ee.Image(collection.iterate(appendBands, first))
  
def clipMT( image ):
  return image.clip(ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8').filter(ee.Filter.eq('Name', 'Montana')))

def getMOD16A2( start_date, end_date, base_name = 'MOD16A2'):
    bbox = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8').filter(ee.Filter.eq('Name', 'Montana')).geometry().bounds().getInfo()
    
    params = {
        'name': base_name + "_" + start_date + "_" + end_date,
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    MOD16A2 = ee.ImageCollection('MODIS/006/MOD16A2').select('ET').filterDate(start_date, end_date)
    
    dates = MOD16A2.getInfo().get('features')
  
    path = stackCollection(MOD16A2).getDownloadURL(params=params)
    
    return({
        'path': path,
        'dates': dates
    })

def getMOD16A2normals():
    base_name = 'MOD16A2'
    bbox = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8').filter(ee.Filter.eq('Name', 'Montana')).geometry().bounds().getInfo()
    
    params = {
        'name': base_name + "_normals",
        'region': bbox['coordinates'],
        'scale': 4000,
        'crs': "EPSG:4326"
    }
    
    MOD16A2 = ee.ImageCollection('MODIS/006/MOD16A2').select('ET')
    
    days = ee.List.sequence(1, 365)

    def getDailyStats( d ):
      day = MOD16A2.filter(ee.Filter.calendarRange(d, d, 'day_of_year')).set('day', d)
      mean = day.reduce(ee.Reducer.mean())
      min = day.reduce(ee.Reducer.min())
      max = day.reduce(ee.Reducer.max())
      percentiles = day.reduce(ee.Reducer.percentile([0,25,50,75,100], 
      maxRaw = 1000))
      return(ee.Image.cat([min, mean, max, percentiles]))

    byDay = days.map(getDailyStats)
    
    out = []
    dates = []

    for i in range(366):
      if ((i + 8) % 8) != 0:
        continue
      out.append(byDay.get(i))
      dates.append(i + 1)

    out = ee.ImageCollection(out)
    MOD16A2_MT_min = stackCollection(out.select(['ET_min'],['min']))
    MOD16A2_MT_mean = stackCollection(out.select(['ET_mean'],['mean']))
    MOD16A2_MT_max = stackCollection(out.select(['ET_max'],['max']))
    MOD16A2_MT_p0 = stackCollection(out.select(['ET_p0'],['p0']))
    MOD16A2_MT_p25 = stackCollection(out.select(['ET_p25'],['p25']))
    MOD16A2_MT_p50 = stackCollection(out.select(['ET_p50'],['p50']))
    MOD16A2_MT_p75 = stackCollection(out.select(['ET_p75'],['p75']))
    MOD16A2_MT_p100 = stackCollection(out.select(['ET_p100'],['p100']))

    return({
        'min': {
          'path': MOD16A2_MT_min.getDownloadURL(params=params),
          'dates': dates
          },
        'mean': {
          'path': MOD16A2_MT_mean.getDownloadURL(params=params),
          'dates': dates
          },
        'max': {
          'path': MOD16A2_MT_max.getDownloadURL(params=params),
          'dates': dates
          },
        'p0': {
          'path': MOD16A2_MT_p0.getDownloadURL(params=params),
          'dates': dates
          },
        'p25': {
          'path': MOD16A2_MT_p25.getDownloadURL(params=params),
          'dates': dates
          },
        'p50': {
          'path': MOD16A2_MT_p50.getDownloadURL(params=params),
          'dates': dates
          },
        'p75': {
          'path': MOD16A2_MT_p75.getDownloadURL(params=params),
          'dates': dates
          },
        'p100': {
          'path': MOD16A2_MT_p100.getDownloadURL(params=params),
          'dates': dates
          }
    })
