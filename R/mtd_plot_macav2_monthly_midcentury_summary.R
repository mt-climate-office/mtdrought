mtd_plot_macav2_monthly_midcentury_summary <- function(macav2,
                                                       element = "tmpc",
                                                       months = 1:12,
                                                       data_out = "./data/macav2_monthly",
                                                       agg_sf,
                                                       agg_sf_fun = mean,
                                                       use_normals = FALSE){
  
  if(element == "tmax"){
    macav2_element = "daily_maximum_temperature"
    unit_symbol <- "ºF"
    long_name <- "Maximum temperature"
  } else if(element == "tmin"){
    macav2_element = "daily_minimum_temperature"
    unit_symbol <- "ºF"
    long_name <- "Minimum temperature"
  }else if(element == "pcpn"){
    macav2_element = "precipitation_amount"
    unit_symbol <- "in."
    long_name <- "Net precipitation"
  }else if(element == "tmpc"){
    macav2_element = c("daily_maximum_temperature","daily_minimum_temperature")
    unit_symbol <- "ºF"
    long_name <- "Average temperature"
  } else {
    stop("Element passed was not allowed!")
  }
  
  if(missing(macav2)){
    macav2 <- mtd_get_macav2_monthly_midcentury_summary(months = months,
                                                         data_out = data_out)
  }
  
  months <- attr(macav2,"months")
  
  macav2 %<>%
    magrittr::extract(macav2_element)
  
  if(length(macav2) > 1){
    macav2 <- (macav2[[1]] + macav2[[2]]) / 2
  } else {
    macav2 <- macav2[[1]]
  }
  
  
  if(use_normals){
    if(element == "pcpn") {
      map_data <- (macav2$value / macav2$normals) %>%
        magrittr::multiply_by(100) %>%
        round()
      
      legend_title <- stringr::str_c(head(month.name[months],1),"-",
                                     tail(month.name[months],1),",\n",
                                     "2040-2069\n",
                                     long_name,"\nPercent of normal")
    } else {
      map_data <- (macav2$value - macav2$normals) %>%
        round(digits = 1)
      legend_title <- stringr::str_c(head(month.name[months],1),"-",
                                     tail(month.name[months],1),",\n",
                                     "2040-2069\n",
                                     long_name,"\nDeviation from normal (",unit_symbol,")")
      
    }
  } else {
    map_data <- macav2$value %>%
      round() %>%
      as.integer()
    
    legend_title <- stringr::str_c(head(month.name[months],1),"-",
                                   tail(month.name[months],1),",\n",
                                   "2040-2069\n",
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
      lwgeom::st_transform_proj(raster::projection(map_data))
    
    macav2.vx <- velox::velox(map_data)
    
    map_data <- agg_sf %>%
      dplyr::bind_cols(macav2.vx$extract(agg_sf_4326,
                                          fun = function(x){agg_sf_fun(x, na.rm = TRUE)},
                                          df = TRUE,
                                          small = TRUE) %>%
                         dplyr::select(-ID_sp) %>%
                         magrittr::set_names(c("value"))) %>%
      sf::st_as_sf()
    
    rm(macav2.vx)
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
  
  file_name <- stringr::str_c(head(month.abb[months],1),"_",
                              tail(month.abb[months],1),"_",
                              "midcentury_",
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
      mtd_plot()) %T>%
    save_mt_map(file_name)
}
