mtd_as_sf_gridmet <- function(gridmet){
  
  gridmet_dates <- attr(gridmet,"dates")
  
  if(is(gridmet[[1]],"RasterBrick")){
    gridmet %<>%
      purrr::map(function(x){
       x %>%
          spex::qm_rasterToPolygons(na.rm = T) %>%
          lwgeom::st_transform_proj(mt_state_plane)
      })
  }
  
  attr(gridmet,"dates") <- gridmet_dates
  
  return(gridmet)
}
