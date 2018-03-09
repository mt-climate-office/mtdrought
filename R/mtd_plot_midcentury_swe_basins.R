mtd_plot_midcentury_swe_basins <- function(month,
                                huc = 6,
                                data_out = "./data/BCSD_mon_VIC"){
  
  # if(lubridate::month(Sys.Date()) < month)
  #   month <- lubridate::month(Sys.Date())
  
  date <-
    lubridate::as_date(stringr::str_c(lubridate::year(Sys.Date()) - 1,"-",month,"-","01"))
  
  swe <- 
    mco_get_swe_basins(date = format(lubridate::ymd(date), '%Y-%m-%d'),
                       huc = huc)
  
  midcentury_swe <- 
    get_bcsd_swe_midcentury_summary(month = month,
                                    data_out = data_out)
  
  
  midcentury_swe.vx <- velox::velox(midcentury_swe)
  midcentury_swe_basins <- 
    midcentury_swe.vx$extract(swe %>%
                                sf::st_cast() %>%
                                sf::st_transform(4326),
                              fun = mean,
                              small = TRUE,
                              df = TRUE) %>%
    magrittr::extract(-1) %>%
    magrittr::set_names(c("Mid-century 25%",
                          "Mid-century 50%",
                          "Mid-century 75%")) %>%
    dplyr::bind_cols(swe,.)
  rm(midcentury_swe.vx,
     midcentury_swe)
  
  
  midcentury_swe_basins_map <- (midcentury_swe_basins %>%
                dplyr::mutate(Centroid_x = midcentury_swe_basins %>%
                                sf::st_centroid() %>%
                                sf::st_coordinates() %>%
                                tibble::as_tibble() %$%
                                X,
                              Centroid_y = midcentury_swe_basins %>%
                                sf::st_centroid() %>%
                                sf::st_coordinates() %>%
                                tibble::as_tibble() %$%
                                Y,
                              `Percent SWE` = (100 * (`Mid-century 50%` / `SWE 1981-2010 Median (in)`)) %>%
                                round()) %>%
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
                scale_fill_distiller(name = stringr::str_c("\nSnow Water Equivalent\n% of 1981-2010 median\nas of ",
                                                           format(lubridate::ymd(date), '%B %d, %Y')),
                                     #limits = c(0,1),
                                     direction = 1,
                                     limits = c(0,200),
                                     breaks = c(0,200),
                                     palette = "RdBu",
                                     expand = FALSE,
                                     guide = guide_colourbar(title.position = "bottom")) +
                mdt_theme_map()) %T>%
    save_mt_map(stringr::str_c(date,"-midcentury-swe.pdf"))
  
  
  return(list(data = midcentury_swe_basins,
              map = midcentury_swe_basins_map))
}
