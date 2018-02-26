mtd_plot_smap <- function(date,
                          data_out = "./data/SMAP"){
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(date >= Sys.Date())
    date <- "latest"
  
  soil_moisture <- mcor::mco_get_smap(id = "SPL4SMGP",
                                      dates = date,
                                      raw_dir = data_out)
  
  soil_moisture_date <- soil_moisture %>%
    names() %>%
    stringr::str_extract("\\_[0-9]+") %>%
    unique() %>%
    stringr::str_replace("\\_","") %>%
    lubridate::as_date()
  
  soil_moisture %<>%
    # Average over the layers (which represent time slices during the day)
    raster::calc(mean) %>%
    # Bin into soil moisture categories
    raster::cut(breaks = c(0,
                           0.1,
                           0.2,
                           0.3,
                           0.35,
                           0.4,
                           0.45,
                           0.5,
                           0.6,
                           1),
                ordered_result = TRUE) %>%
    # Convert to polygons
    raster::rasterToPolygons() %>%
    sf::st_as_sf() %>%
    sf::st_transform(mt_state_plane) %>%
    dplyr::mutate(layer = factor(layer,
                                 levels = 1:9,
                                 labels = c("<10%",
                                            "10-20%",
                                            "20-30%",
                                            "30-35%",
                                            "35-40%",
                                            "40-45%",
                                            "45-50%",
                                            "50-60%",
                                            ">60%"))) %>%
    sf::st_intersection(mt_state_simple) %>%
    dplyr::group_by(layer) %>%
    dplyr::summarise()
  
  smap_map <- (soil_moisture %>%
                 ggplot2::ggplot() +
                 ggplot2::geom_sf(aes(fill = layer),
                                  color = NA) +
                 scale_fill_manual(name = stringr::str_c("% Soil Saturation\nas of ",
                                                         format(lubridate::ymd(soil_moisture_date), '%B %d, %Y')),
                                   limits = c("<10%",
                                              "10-20%",
                                              "20-30%",
                                              "30-35%",
                                              "35-40%",
                                              "40-45%",
                                              "45-50%",
                                              "50-60%",
                                              ">60%"),
                                   values = c("<10%" = rgb(115, 0, 0, maxColorValue = 255),
                                              "10-20%" = rgb(255, 0, 0, maxColorValue = 255),
                                              "20-30%" = rgb(255, 170, 0, maxColorValue = 255),
                                              "30-35%" = rgb(215, 194, 158, maxColorValue = 255),
                                              "35-40%" = rgb(255, 255, 0, maxColorValue = 255),
                                              "40-45%" = rgb(255, 255, 255, maxColorValue = 255),
                                              "45-50%" = rgb(190, 232, 255, maxColorValue = 255),
                                              "50-60%" = rgb(0, 112, 255, maxColorValue = 255),
                                              ">60%" = rgb(0, 38, 115, maxColorValue = 255)),
                                   guide = guide_legend(title.position = "bottom")) +
                 add_hillshade() +
                 add_counties() +
                 add_climate_divisions() +
                 mdt_theme_map()) %T>%
    save_mt_map(stringr::str_c(soil_moisture_date,"-soil-moisture.pdf"))
  
  return(list(data = soil_moisture,
              map = smap_map))
}
