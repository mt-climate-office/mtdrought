import ee

nass = ee.Image('USDA/NASS/CDL/2017').select("cropland")
nass.gte(66).or(nass.lte(80))
