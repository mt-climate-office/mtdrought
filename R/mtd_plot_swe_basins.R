mtd_plot_swe_basins <- function(date = "latest",
                                huc = 6){
  
  if(date >= Sys.Date())
    date <- "latest"
  
  swe <- mco_get_swe_basins(date = date,
                            huc = huc)
  
  if(date == "latest")
    date <- Sys.Date()
  
  swe_map <- (swe %>%
                dplyr::mutate(Centroid_x = swe %>%
                                sf::st_centroid() %>%
                                sf::st_coordinates() %>%
                                tibble::as_tibble() %$%
                                X,
                              Centroid_y = swe %>%
                                sf::st_centroid() %>%
                                sf::st_coordinates() %>%
                                tibble::as_tibble() %$%
                                Y) %>%
                ggplot2::ggplot() +
                # Plot the polygon fills
                geom_sf(aes(fill = `Percent SWE`),
                        color = NA) +
                add_hillshade() +
                add_counties() +
                # Plot the polygon boundaries
                geom_sf(fill = NA,
                        color = "white",
                        size = 0.5) +
                # add_climate_divisions() +
                # Plot the labels
                geom_label(aes(x = Centroid_x,
                               y = Centroid_y,
                               label = `Percent SWE` %>%
                                 paste0("%")),
                           alpha = 1,
                           size = 2.25) +
                scale_fill_distiller(name = stringr::str_c("\nSnow Water Equivalent\n% of 1981-2010 median\nas of",
                                                           format(lubridate::ymd(date), '%B %d, %Y')),
                                     #limits = c(0,1),
                                     direction = 1,
                                     limits = c(0,200),
                                     breaks = c(0,200),
                                     palette = "RdBu",
                                     expand = FALSE,
                                     guide = guide_colourbar(title.position = "bottom")) +
                mdt_theme_map()) %T>%
    save_mt_map(stringr::str_c(date,"-swe.pdf"))
  
  
  return(list(data = swe,
              map = swe_map))
}
