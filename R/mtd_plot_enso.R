mtd_plot_enso <- function (months,
                           element = "tmax",
                           agg_fun = mean,
                           enso = "ENSO Neutral"
){
  
  if(length(months) != 3)
    stop("Months must be a sequence of three months.")
  
  season <- month.abb[months] %>%
    stringr::str_sub(1,1) %>%
    stringr::str_c(collapse = "")
  
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
                                  element = element,
                                  agg_fun = agg_fun)
  
  oni <- mtd_get_oni()
  
  enso_average <- climdiv_data$data %>%
    dplyr::filter(Year %in% (oni %>%
                               dplyr::filter(Season == season,
                                             ENSO == enso) %$%
                               Year)) %>%
    dplyr::group_by(`Division code`) %>%
    dplyr::summarise(`ENSO` = mean(Value))
  
  climdiv_data <- dplyr::left_join(climdiv_data$latest,
                                   enso_average,
                                   by = "Division code") %>%
    dplyr::mutate(Anomaly = `ENSO` - Normal)
  
  
  range <- climdiv_data %$%
    {`ENSO` - Normal} %>%
    abs() %>%
    max() %>%
    ceiling()
  
  climdiv_map <- (climdiv_data %>%
                    ggplot2::ggplot() +
                    geom_sf(aes(geometry = Shape,
                                fill = `ENSO`-Normal),
                            color = "white") +
                    scale_fill_distiller(name = stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],", ",
                                                               enso,"\n",long_name,"\nDeviation from Norm. (",unit_symbol,")"),
                                         #limits = c(0,1),
                                         direction = if(element == "pcpn") 1 else -1,
                                         limits = c(0-range,range),
                                         breaks = c(0-range,0,range),
                                         palette = if(element == "pcpn") "BrBG" else "RdBu",
                                         expand = FALSE,
                                         guide = guide_colourbar(title.position = "bottom")) +
                    mtd_plot() +
                    # Plot the labels
                    geom_label(aes(x = Centroid_x,
                                   y = Centroid_y,
                                   label = stringr::str_c(round(Value, digits = 1)," ",unit_symbol,"\n",
                                                          print_sign(`ENSO`-Normal), round(`ENSO`-Normal, digits = 1), " from norm.")),
                               alpha = 1,
                               size = 2.25)) %T>%
    save_mt_map(stringr::str_c(month.abb[[head(months,1)]],"-",month.abb[[tail(months,1)]],"-",
                               enso,"-",element,".pdf"))
  
  return(list(data = climdiv_data,
              map = climdiv_map))
  
}
