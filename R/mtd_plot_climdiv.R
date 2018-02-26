mtd_plot_climdiv <- function (months,
                              year,
                              element = "tmax",
                              agg_fun = mean){
  
  if(element == "tmax"){
    unit_symbol <- "ºF"
    long_name <- "Max. Temperature"
  }else if(element == "pcpn"){
    unit_symbol <- "in."
    long_name <- "Net Precipitation"
  }else if(element == "tmpc"){
    unit_symbol <- "ºF"
    long_name <- "Avg. Temperature"
  }
  
  climdiv_data <- climdiv_summary(months = months,
                                  year = year,
                                  element = element,
                                  agg_fun = agg_fun)
  
  range <- climdiv_data %$%
    latest %$%
    {Value-Normal} %>%
    abs() %>%
    max() %>%
    ceiling()
  
  climdiv_map <- (climdiv_data$latest %>%
                    ggplot2::ggplot() +
                    geom_sf(aes(geometry = Shape,
                                fill = Value-Normal),
                            color = "white") +
                    add_hillshade() +
                    add_counties() +
                    add_climate_divisions() + 
                    # Plot the labels
                    geom_label(aes(x = Centroid_x,
                                   y = Centroid_y,
                                   label = stringr::str_c(round(Value, digits = 1)," ",unit_symbol,"\n",
                                                          print_sign(Value-Normal), round(Value-Normal, digits = 1), " from norm.")),
                               alpha = 1,
                               size = 2.25) +
                    scale_fill_distiller(name = stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],", ",
                                                               year,"\n",long_name,"\nDeviation from Norm. (",unit_symbol,")"),
                                         #limits = c(0,1),
                                         direction = if(element == "pcpn") 1 else -1,
                                         limits = c(0-range,range),
                                         breaks = c(0-range,0,range),
                                         palette = if(element == "pcpn") "BrBG" else "RdBu",
                                         expand = FALSE,
                                         guide = guide_colourbar(title.position = "bottom")) +
                    mdt_theme_map()) %T>%
    save_mt_map(stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],"-",
                               year,"-",element,".pdf"))
  
  return(list(data = climdiv_data,
              map = climdiv_map))
  
}
