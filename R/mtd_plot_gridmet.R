mtd_plot_gridmet <- function(gridmet,
                             element = "tmean",
                             # dates = "latest",
                             # data_out = "./data/gridmet",
                             # agg_sf,
                             # agg_sf_fun = mean,
                             use_normal = FALSE){
  
  if(element == "tmax"){
    gridmet_element = "tmmx"
    unit_symbol <- "ºF"
    long_name <- "Maximum temperature"
  } else if(element == "tmin"){
    gridmet_element = "tmmn"
    unit_symbol <- "ºF"
    long_name <- "Minimum temperature"
  }else if(element == "prcp"){
    gridmet_element = "pr"
    unit_symbol <- "in."
    long_name <- "Net precipitation"
  }else if(element == "tmean"){
    gridmet_element = c("tmmx","tmmn")
    unit_symbol <- "ºF"
    long_name <- "Average temperature"
  } else {
    stop("Element passed was not allowed!")
  }
  
  # if(missing(gridmet)){
  #   gridmet <- mtd_get_gridmet(dates = dates,
  #                              data_out = data_out)
  # }
  
  gridmet_dates <- attr(gridmet,"dates")
  
  gridmet %<>%
    magrittr::extract(gridmet_element)
  
  if(!is(gridmet[[1]],"RasterBrick")){
    gridmet %<>%
      purrr::map(function(x){
        x %>%
          magrittr::extract(c("layer","normal.mean"))
      })
  }
  
  if(length(gridmet) > 1){
    gridmet_out <- gridmet[[1]]
    
    gridmet_out$layer <- (gridmet[[1]]$layer + gridmet[[2]]$layer) / 2
    gridmet_out$normal.mean <- (gridmet[[1]]$normal.mean + gridmet[[2]]$normal.mean) / 2
    gridmet <- gridmet_out
    
  } else {
    gridmet <- gridmet[[1]]
  }
  
  if(is(gridmet,"RasterBrick")){
    gridmet %<>%
      mtd_as_sf_gridmet()
  }
  
  if(use_normal){
    if(element == "prcp") {
      map_data <- gridmet %>%
        dplyr::mutate(layer = (layer / normal.mean) %>%
                        magrittr::multiply_by(100) %>%
                        round()) %>%
        dplyr::select(layer)
      
      legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                     format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                     long_name,"\nPercent of normal")
    } else {
      map_data <- gridmet %>%
        dplyr::mutate(layer = (layer - normal.mean) %>%
                        round(digits = 1)) %>%
        dplyr::select(layer)
      
      legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                     format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                     long_name," (",unit_symbol,")","\n",
                                     "Deviation from normal")
      
    }
  } else {
    map_data <- gridmet %>%
      dplyr::mutate(layer = layer %>%
                      round() %>%
                      as.integer()) %>%
      dplyr::select(layer)
    
    legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                   format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                   long_name," (",unit_symbol,")")
  }
  
  sf_column <- attr(map_data, "sf_column") %>%
    rlang::sym()
  
  map_data %<>%
    dplyr::rename(geometry = !!sf_column)
  
  if(use_normal){
    if(element == "prcp") {
      limits <- c(0,200)
      
      map_data$layer[map_data$layer > 200] <- 200
      
      map_data %<>%
        dplyr::group_by(layer) %>%
        dplyr::summarise()
      
    } else {
      range <- map_data$layer %>%
        abs() %>%
        max() %>%
        ceiling()
      
      limits <- c(-range,range)
    }
  } else {
    limits <- range(map_data$layer)
  }
  
  if(element == "prcp") {
    direction = 1
    if(use_normal)
      palette = "BrBG"
    else
    palette = "BuGn"
  }else {
    direction = -1
    if(use_normal)
      palette = "RdBu"
    else{
      direction = 1
      palette = "Reds"
    }
  }
  
  map_data %<>%
    dplyr::group_by(layer) %>%
    dplyr::summarise()
  
  (map_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = layer),
                       color = "transparent") +
      scale_fill_distiller(name = legend_title,
                           direction = direction,
                           limits = limits,
                           # breaks = breaks,
                           palette = palette,
                           expand = expansion(mult = 0, add = 0),
                           guide = guide_colourbar(title.position = "bottom")) +
      mtd_plot())
}
