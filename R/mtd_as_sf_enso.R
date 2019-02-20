mtd_as_sf_enso <- function(enso){
  
  if(is(enso[[1]],"RasterBrick")){
    enso %<>%
      purrr::map(function(x){
       x %>%
          spex::qm_rasterToPolygons(na.rm = T) %>%
          lwgeom::st_transform_proj(mt_state_plane)
      }) %>%
      purrr::map(function(x){
        
        sf::st_agr(x) = "constant"
        
        sf::st_intersection(x = x, 
                            y = mcor::mt_state_simple)
      })
  }
  
  return(enso)
}
