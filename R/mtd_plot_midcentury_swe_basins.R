mtd_plot_midcentury_swe_basins <- function(month,
                                           huc = 6,
                                           data_out = "./data/BCSD_mon_VIC"){
  
  # if(lubridate::month(Sys.Date()) < month)
  #   month <- lubridate::month(Sys.Date())
  
  date <-
    lubridate::as_date(stringr::str_c(lubridate::year(Sys.Date()) - 1,"-",month,"-","01"))
  
  snotel_inventory <- mco_get_snotel_inventory() %>%
    dplyr::left_join(mco_get_snotel_huc() %>%
                       dplyr::mutate(`WBD code` = stringr::str_sub(`WBD code`,1,huc)) %>%
                       dplyr::distinct()) %>%
    dplyr::filter(`WBD code` %in% (mt_watersheds_simple %>%
                                     dplyr::filter(`Hydrologic Unit` %in% c(huc)) %$%
                                     `WBD code`)) %>%
    sf::st_as_sf() %>%
    lwgeom::st_transform_proj(mt_state_plane) %>%
    dplyr::select(-`Station Id`:-`End Date`)
  
  snotel_data <- mco_get_snotel_data(stations = snotel_inventory$Station %>%
                                       unique(),
                                     variables = c('WTEQ::value',
                                                   'WTEQ::median_1981'),
                                     start_date = date,
                                     end_date = date)
  
  swe <- snotel_inventory %>%
    dplyr::left_join(snotel_data,
                     by = c("Station")) %>%
    dplyr::arrange(Station) %>%
    stats::na.omit() %>%
    dplyr::select(`WBD code`,
                  `Snow Water Equivalent (in) Start of Day Values`,
                  `Median Snow Water Equivalent (1981-2010) (in) Start of Day Values`) %>%
    dplyr::rename(value = `Snow Water Equivalent (in) Start of Day Values`,
                  normal = `Median Snow Water Equivalent (1981-2010) (in) Start of Day Values`)
  
  midcentury_swe <- 
    get_bcsd_swe_midcentury_summary(month = month,
                                    data_out = data_out)
  
  midcentury_swe.vx <- velox::velox(midcentury_swe)
  
    swe %<>%
      dplyr::ungroup() %>%
    dplyr::mutate(midcentury = midcentury_swe.vx$extract_points(swe %>%
                                sf::st_cast() %>%
                                lwgeom::st_transform_proj(4326))[,2])

  rm(midcentury_swe.vx,
     midcentury_swe)
  
  
  swe_basins <- swe %>%
    dplyr::group_by(`WBD code`) %>%
    dplyr::summarise(`Stations Count` = n(),
                     value = mean(value),
                     normal = mean(normal),
                     midcentury = mean(midcentury)) %>%
    dplyr::filter(`Stations Count` >= 3) %>%
    dplyr::mutate(value_percent = round(100 * value / normal) %>%
                    as.integer(),
                  midcentury_percent = round(100 * midcentury / normal) %>%
                    as.integer()) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::left_join(mt_watersheds_simple) %>%
    sf::st_sf() %>%
    dplyr::select(-`Hydrologic Unit`) %>%
    dplyr::select(`WBD code`, Watershed, dplyr::everything())
  
  swe_basins %<>%
    sf::st_intersection(mt_state_simple)
  
  swe_basins %<>%
    dplyr::filter(sf::st_area(swe_basins) > units::set_units(1000000000,m^2))
  
  swe_basins_map <- (swe_basins %>%
                                  dplyr::mutate(Centroid_x = swe_basins %>%
                                                  sf::st_centroid() %>%
                                                  sf::st_coordinates() %>%
                                                  tibble::as_tibble() %$%
                                                  X,
                                                Centroid_y = swe_basins %>%
                                                  sf::st_centroid() %>%
                                                  sf::st_coordinates() %>%
                                                  tibble::as_tibble() %$%
                                                  Y,
                                                `Percent SWE` = midcentury_percent) %>%
                                  ggplot2::ggplot() +
                                  # Plot the polygon fills
                                  geom_sf(aes(fill = midcentury_percent),
                                          color = NA) +
                                  scale_fill_distiller(name = stringr::str_c(format(lubridate::ymd(date), '%B %d') %>%
                                                                               stringr::str_replace(" 0"," "),", ",
                                                                             "2040-2069\n",
                                                                             "Snow water equivalent","\n",
                                                                             "Percent of normal"),
                                                       #limits = c(0,1),
                                                       direction = 1,
                                                       limits = c(0,200),
                                                       # breaks = c(0,200),
                                                       palette = "RdBu",
                                                       expand = FALSE,
                                                       guide = guide_colourbar(title.position = "bottom")) +
                                  mtd_plot() +
                                  # Plot the polygon boundaries
                                  geom_sf(fill = NA,
                                          color = "white",
                                          size = 0.5) +
                                  # Plot the labels
                                  geom_label(aes(x = Centroid_x,
                                                 y = Centroid_y,
                                                 label = `Percent SWE` %>%
                                                   paste0("%")),
                                             alpha = 1,
                                             size = 2.25)) %T>%
    save_mt_map(stringr::str_c(date,"-midcentury-swe.pdf"))
  
  
  return(list(data = swe_basins,
              map = swe_basins_map))
}
