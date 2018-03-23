mtd_plot_macav2_midcentury_summary <-
  function (months = 1:3,
            year = 2018,
            element = "tasmean",
            agg_sf = mt_counties_simple,
            agg_fun = mean,
            normal = FALSE,
            data_out = "../data/macav2_monthly")
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
                                      agg_fun = agg_fun,
                                      data_out = data_out) + 
          get_macav2_midcentury_summary(months = months,
                                        element = "tasmin",
                                        agg_fun = agg_fun,
                                        data_out = data_out)
      ) / 2
      
    } else {
      macav2_data <- 
        get_macav2_midcentury_summary(months = months,
                                      element = element,
                                      agg_fun = agg_fun,
                                      data_out = data_out)
    } 
    
    climdiv_data <- climdiv_summary(months = months,
                                    year = year,
                                    element = climdiv_element,
                                    agg_fun = agg_fun)
    
    agg_sf_4326 <- agg_sf %>%
      lwgeom::st_transform_proj(4326)
    
    sf::st_geometry(agg_sf_4326) <- 
      sf::st_geometry(agg_sf_4326) + c(360,0)
    
    macav2_data.vx <- velox::velox(macav2_data)
    
    macav2_agg <- agg_sf %>%
      dplyr::mutate(Centroid_x = agg_sf %>%
                      sf::st_centroid() %>%
                      sf::st_coordinates() %>%
                      tibble::as_tibble() %$%
                      X,
                    Centroid_y = agg_sf %>%
                      sf::st_centroid() %>%
                      sf::st_coordinates() %>%
                      tibble::as_tibble() %$%
                      Y) %>%
      dplyr::bind_cols(macav2_data.vx$extract(agg_sf_4326,
                                              fun = mean,
                                              df = TRUE) %>%
                         dplyr::select(-ID_sp) %>%
                         magrittr::set_names(c("25%","50%","75%"))) %>%
      dplyr::left_join(climdiv_data$latest %>%
                         dplyr::select(`Division code`,
                                       Normal,
                                       Value) %>%
                         sf::st_set_geometry(NULL),
                       by = "Division code")
    
    rm(macav2_data.vx)
    
    
    if(element == "pr"){
      limits <- c(0,200)
      breaks <- c(0,100,200)
      
    }else{
      range <- macav2_agg %$%
      {if(normal) `50%` - Normal else `50%` - Value} %>%
        abs() %>%
        max() %>%
        ceiling()
      
      limits <- c(-range,range)
      breaks <- c(-range,0,range)
      
    }
    
    
    
    macav2_agg_map <- (macav2_agg %>%
                             ggplot2::ggplot() +
                             geom_sf(aes(geometry = Shape,
                                         fill = if(element == "pr"){
                                           if(normal) {
                                             100 * `50%` / Normal
                                           } else {
                                             100 * `50%` / Value
                                           }
                                         } else {
                                           if(normal) `50%` - Normal else `50%` - Value
                                         }),
                                     color = "white") +
                             add_hillshade() +
                             add_counties() +
                             add_climate_divisions() +
                             # Plot the labels
                             geom_label(aes(x = Centroid_x,
                                            y = Centroid_y,
                                            label = 
                                              if(element == "pr"){
                                                stringr::str_c(round(`50%`, digits = 1)," ",unit_symbol,"\n",
                                                               round(
                                                                 if(normal) {
                                                                   100 * `50%` / Normal
                                                                 } else {
                                                                   100 * `50%` / Value
                                                                 }, 
                                                                 digits = 0), 
                                                               "% of ",ifelse(normal, "norm.", year))
                                              } else {
                                                stringr::str_c(round(`50%`, digits = 1)," ",unit_symbol,"\n",
                                                               print_sign(if(normal) `50%` - Normal else `50%` - Value), 
                                                               round(if(normal) `50%` - Normal else `50%` - Value, digits = 1), 
                                                               " from ",ifelse(normal, "norm.", year))
                                              }),
                                        alpha = 1,
                                        size = 2.25) +
                             scale_fill_distiller(name = 
                                                    if(element == "pr"){
                                                      stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],", ",
                                                                     "2040-2069\n",long_name,"\nPercent of ",ifelse(normal, "Normal", year))
                                                    }else{
                                                      stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],", ",
                                                                     "2040-2069\n",long_name,"\nDeviation from ",ifelse(normal, "Normal", year)," (",unit_symbol,")")
                                                    },
                                                  direction = if(element == "pr") 1 else -1,
                                                  limits = limits,
                                                  breaks = breaks,
                                                  palette = if(element == "pr") "BrBG" else "RdBu",
                                                  expand = FALSE,
                                                  guide = guide_colourbar(title.position = "bottom")) +
                             mdt_theme_map()) %T>%
      save_mt_map(stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],"-",
                                 ifelse(normal, "normal", year),"-midcentury-",element,".pdf"))
    
    return(list(data = macav2_climdiv,
                map = macav2_climdiv_map))
    
  }
