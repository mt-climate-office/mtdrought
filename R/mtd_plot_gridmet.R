mtd_plot_gridmet <- function(gridmet,
                             element = "tmpc",
                             dates = "latest",
                             data_out = "./data/gridmet",
                             agg_sf,
                             agg_sf_fun = mean,
                             use_normals = FALSE){
  
  if(element == "tmax"){
    gridmet_element = "daily_maximum_temperature"
    unit_symbol <- "ºF"
    long_name <- "Maximum temperature"
  } else if(element == "tmin"){
    gridmet_element = "daily_minimum_temperature"
    unit_symbol <- "ºF"
    long_name <- "Minimum temperature"
  }else if(element == "pcpn"){
    gridmet_element = "precipitation_amount"
    unit_symbol <- "in."
    long_name <- "Net precipitation"
  }else if(element == "tmpc"){
    gridmet_element = c("daily_maximum_temperature","daily_minimum_temperature")
    unit_symbol <- "ºF"
    long_name <- "Average temperature"
  } else {
    stop("Element passed was not allowed!")
  }
  
  if(missing(gridmet)){
    gridmet <- mtd_get_gridmet(dates = dates,
                               data_out = data_out)
  }
  
  gridmet_dates <- attr(gridmet,"dates")
  
  gridmet %<>%
    magrittr::extract(gridmet_element)
  
  if(length(gridmet) > 1){
    gridmet <- (gridmet[[1]] + gridmet[[2]]) / 2
  } else {
    gridmet <- gridmet[[1]]
  }
  
  
  if(use_normals){
    if(element == "pcpn") {
      map_data <- (gridmet$value / gridmet$normals) %>%
        magrittr::multiply_by(100) %>%
        round()
      
      legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                     format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                     long_name,"\nPercent of normal")
    } else {
      map_data <- (gridmet$value - gridmet$normals) %>%
        round(digits = 1)
      legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                     format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                     long_name,"\nDeviation from normal (",unit_symbol,")")
      
    }
  } else {
    map_data <- gridmet$value %>%
      round() %>%
      as.integer()
    
    legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                   format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                   long_name," (",unit_symbol,")")
  }
  
  map_data %<>%
    magrittr::set_names("value")
  
  if(missing(agg_sf)){
    map_data %<>%
      raster::rasterToPolygons() %>%
      sf::st_as_sf() %>%
      lwgeom::st_transform_proj(mt_state_plane) %>%
      dplyr::group_by(value) %>%
      dplyr::summarise() %>%
      sf::st_intersection(mt_state_simple)
  } else {
    agg_sf_4326 <- agg_sf %>%
      lwgeom::st_transform_proj(raster::projection(gridmet))
    
    gridmet.vx <- velox::velox(map_data)
    
    map_data <- agg_sf %>%
      dplyr::bind_cols(gridmet.vx$extract(agg_sf_4326,
                                          fun = function(x){agg_sf_fun(x, na.rm = TRUE)},
                                          df = TRUE,
                                          small = TRUE) %>%
                         dplyr::select(-ID_sp) %>%
                         magrittr::set_names(c("value"))) %>%
      sf::st_as_sf()
    
    rm(gridmet.vx)
  }
  
  sf_column <- attr(map_data, "sf_column") %>%
    rlang::sym()
  
  map_data %<>%
    dplyr::rename(geometry = !!sf_column)
  
  if(use_normals){
    if(element == "pcpn") {
      limits <- c(0,200)
      
      map_data$value[map_data$value > 200] <- 200
      
    } else {
      range <- map_data$value %>%
        abs() %>%
        max() %>%
        ceiling()
      
      limits <- c(-range,range)
    }
  } else {
    limits <- range(map_data$value)
  }
  
  file_name <- stringr::str_c(head(gridmet_dates,1),"_",
                              tail(gridmet_dates,1),"_",
                              element,
                              ifelse(missing(agg_sf),"","-aggregated"),
                              ifelse(use_normals,"-normals",""),
                              ".pdf")
  
  (map_data %>%
                    ggplot2::ggplot() +
                    ggplot2::geom_sf(aes(fill = value),
                                     color = NA) +
                    scale_fill_distiller(name = legend_title,
                                         direction = if(element == "pcpn") 
                                           1 
                                         else 
                                           -1,
                                         limits = limits,
                                         # breaks = breaks,
                                         palette = if(element == "pcpn") 
                                           "BrBG" 
                                         else 
                                           "RdBu",
                                         expand = FALSE,
                                         guide = guide_colourbar(title.position = "bottom")) +
                    add_hillshade() +
                    add_counties() +
                    # add_climate_divisions() +
                    mdt_theme_map()) %T>%
    save_mt_map(file_name)
}
