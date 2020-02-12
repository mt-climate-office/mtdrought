mtd_aggregate_gridmet <- function(gridmet,
                                  agg_sf,
                                  agg_sf_fun = mean){
  
  gridmet_dates <- attr(gridmet,"dates")
  
  agg_sf_4326 <- agg_sf %>%
    sf::st_transform(raster::projection(gridmet[[1]]))
  
  gridmet %<>%
    purrr::map(function(element){
      
      gridmet.vx <- velox::velox(element)
      
     agg_sf %>%
        dplyr::bind_cols(gridmet.vx$extract(agg_sf_4326,
                                            fun = function(x){
                                              agg_sf_fun(x, na.rm = TRUE)
                                            },
                                            df = TRUE,
                                            small = TRUE) %>%
                           dplyr::select(-ID_sp) %>%
                           magrittr::set_names(names(element))) %>%
        sf::st_as_sf()
      
    })
  
  attr(gridmet,"dates") <- gridmet_dates
  
  return(gridmet)
}
