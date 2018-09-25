mtd_as_sf_enso <- function(enso){
  
  if(is(enso[[1]],"RasterBrick")){
    enso %<>%
      purrr::map(function(x){
       x %>%
          spex::qm_rasterToPolygons(na.rm = T) %>%
          lwgeom::st_transform_proj(mt_state_plane)
      }) %>%
      purrr::map(sf::st_intersection,
                 y = mcor::mt_state_simple)
  }
  
  return(enso)
}
