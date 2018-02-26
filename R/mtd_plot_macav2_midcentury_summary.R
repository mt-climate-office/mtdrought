mtd_plot_macav2_midcentury_summary <-
  function (months = 1:3,
            element = "tasmax",
            agg_fun = mean)
  {
    
    if(element == "tasmax"){
      unit_symbol <- "ºF"
      long_name <- "Max. Temperature"
    }else if(element == "tasmin"){
      unit_symbol <- "ºF"
      long_name <- "Min. Temperature"
    }else if(element == "tasmean"){
      unit_symbol <- "ºF"
      long_name <- "Avg. Temperature"
    }else if(element == "pr"){
      unit_symbol <- "in."
      long_name <- "Net Precipitation"
    }
    
    if(element == "tasmean") climdiv_element <- "tmpc"
    if(element == "tasmin") climdiv_element <- "tmin"
    if(element == "tasmax") climdiv_element <- "tmax"
    if(element == "pr") climdiv_element <- "pcpn"
    
    if(element == "tasmean"){
      macav2_data <- (
        get_macav2_midcentury_summary(months = months,
                                      element = "tasmax",
                                      agg_fun = agg_fun) + 
          get_macav2_midcentury_summary(months = months,
                                        element = "tasmin",
                                        agg_fun = agg_fun)
      ) / 2
      
    } else {
      macav2_data <- 
        get_macav2_midcentury_summary(months = months,
                                      element = element,
                                      agg_fun = agg_fun)
    } 
    
    climdiv_data <- climdiv_summary(months = months,
                                    element = climdiv_element,
                                    agg_fun = agg_fun)
    
    mt_climate_divisions_simple_4326 <- mt_climate_divisions_simple %>%
      sf::st_transform(4326)
    
    sf::st_geometry(mt_climate_divisions_simple_4326) <- 
      sf::st_geometry(mt_climate_divisions_simple_4326) + c(360,0)
    
    macav2_data.vx <- velox::velox(macav2_data)
    
    macav2_climdiv <- mt_climate_divisions_simple %>%
      dplyr::mutate(Centroid_x = mt_climate_divisions_simple %>%
                      sf::st_centroid() %>%
                      sf::st_coordinates() %>%
                      tibble::as_tibble() %$%
                      X,
                    Centroid_y = mt_climate_divisions_simple %>%
                      sf::st_centroid() %>%
                      sf::st_coordinates() %>%
                      tibble::as_tibble() %$%
                      Y) %>%
      dplyr::bind_cols(macav2_data.vx$extract(mt_climate_divisions_simple_4326,
                                              fun = mean,
                                              df = TRUE) %>%
                         dplyr::select(-ID_sp) %>%
                         magrittr::set_names(c("25%","50%","75%"))) %>%
      dplyr::left_join(climdiv_data$normals,
                       by = "Division code")
    
    rm(macav2_data.vx)
    
    macav2_climdiv_map <- (macav2_climdiv %>%
                             ggplot2::ggplot() +
                             geom_sf(aes(geometry = Shape,
                                         fill = `50%`-Normal),
                                     color = "white") +
                             add_hillshade() +
                             add_counties() +
                             add_climate_divisions() +
                             # Plot the labels
                             geom_label(aes(x = Centroid_x,
                                            y = Centroid_y,
                                            label = stringr::str_c(round(`50%`, digits = 1)," ",unit_symbol,"\n",
                                                                   print_sign(`50%`-Normal), round(`50%`-Normal, digits = 1), " from norm.")),
                                        alpha = 1,
                                        size = 2.25) +
                             scale_fill_distiller(name = stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],", ",
                                                                        year,"\n",long_name,"\nDeviation from Norm. (",unit_symbol,")"),
                                                  direction = if(element == "pr") 1 else -1,
                                                  limits = c(0-range,range),
                                                  breaks = c(0-range,0,range),
                                                  palette = if(element == "pr") "BrBG" else "RdBu",
                                                  expand = FALSE,
                                                  guide = guide_colourbar(title.position = "bottom")) +
                             mdt_theme_map()) %T>%
      save_mt_map(stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],"-midcentury-",element,".pdf"))
    
    return(list(data = macav2_climdiv,
                map = macav2_climdiv_map))
    
  }
