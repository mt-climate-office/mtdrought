mtd_plot_climatology_enso <- function(x,
                                      element,
                                      climates = c("El Niño",
                                                  "ENSO Neutral",
                                                  "La Niña"),
                                 title = NULL,
                                 ybreaks = seq(-25,100,25),
                                 ymin = min(ybreaks, na.rm = TRUE),
                                 ymax = max(ybreaks, na.rm = TRUE),
                                 smooth = TRUE,
                                 polar = FALSE,
                                 mean = TRUE,
                                 col = "black",
                                 ...){
  
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
  
  x %<>%
    dplyr::filter(variable %in% gridmet_element,
                  climate %in% climates)
  
  if(element == "tmean"){
    x %<>%
      group_by(day, climate, statistic) %>%
      summarise(value = mean(value)) %>%
      ungroup()
  }

  if(smooth){
    
    x %<>%
      group_by(climate, statistic) %>%
      dplyr::mutate(value = mtd_gam_smooth(x = day, 
                                           y = value,
                                           ...))
  }
  
  x %<>%
    tidyr::spread(statistic,value)
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(aes(x = 1:365,
                             ymin = ymin,
                             ymax = ymax),
                         fill = "gray95") +
    ggplot2::geom_line(ggplot2::aes(x = day,
                                    y = `0%`,
                                    color = `climate`),
                       data = x,
                       linetype = 2,
                       alpha = 1) +
    ggplot2::geom_line(ggplot2::aes(x = day,
                                    y = `100%`,
                                    color = `climate`),
                       data = x,
                       linetype = 2,
                       alpha = 1) +
    # ggplot2::geom_ribbon(ggplot2::aes(x = day,
    #                                   ymin = `0%`,
    #                                   ymax = `100%`,
    #                                   # color = `climate`,
    #                                   fill = `climate`),
    #                      alpha = 0.5,
    #                      data = x
    #                      ) +
    ggplot2::geom_line(ggplot2::aes(x = day,
                                    y = mean,
                                    color = `climate`),
                       data = x,
                       linetype = 1,
                       alpha = 1) +
    mtd_theme_climatology(ybreaks = ybreaks,
                          title = stringr::str_c(long_name, " (",unit_symbol,")"),
                          polar = polar) +
    scale_colour_brewer(palette = "Dark2",
                        name = "")
}
