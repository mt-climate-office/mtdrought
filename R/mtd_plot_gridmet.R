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
          magrittr::extract(c("value","mean"))
      })
  }
  
  if(length(gridmet) > 1){
    gridmet_out <- gridmet[[1]]
    
    gridmet_out$value <- (gridmet[[1]]$value + gridmet[[2]]$value) / 2
    gridmet_out$mean <- (gridmet[[1]]$mean + gridmet[[2]]$mean) / 2
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
        dplyr::mutate(value = (value / mean) %>%
                        magrittr::multiply_by(100) %>%
                        round()) %>%
        dplyr::select(value)
      
      legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                     format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                     long_name,"\nPercent of normal")
    } else {
      map_data <- gridmet %>%
        dplyr::mutate(value = (value - mean) %>%
                        round(digits = 1)) %>%
        dplyr::select(value)
      
      legend_title <- stringr::str_c(format(head(gridmet_dates,1), '%B %d, %Y')," - \n",
                                     format(tail(gridmet_dates,1), '%B %d, %Y'),"\n",
                                     long_name," (",unit_symbol,")","\n",
                                     "Deviation from normal")
      
    }
  } else {
    map_data <- gridmet %>%
      dplyr::mutate(value = value %>%
                      round() %>%
                      as.integer()) %>%
      dplyr::select(value)
    
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
      
      map_data$value[map_data$value > 200] <- 200
      
      map_data %<>%
        dplyr::group_by(value) %>%
        dplyr::summarise()
      
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
    dplyr::group_by(value) %>%
    dplyr::summarise()
  
  (map_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = value),
                       color = NA) +
      scale_fill_distiller(name = legend_title,
                           direction = direction,
                           limits = limits,
                           # breaks = breaks,
                           palette = palette,
                           expand = FALSE,
                           guide = guide_colourbar(title.position = "bottom")) +
      mtd_plot())
}
