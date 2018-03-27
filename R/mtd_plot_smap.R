mtd_plot_smap <- function(date,
                          data_out = "./data/SMAP",
                          variable = "sm_rootzone_wetness",
                          agg_sf,
                          agg_fun = mean){
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(date != "latest" && date >= Sys.Date())
    date <- "latest"
  
  soil_moisture <- mcor::mco_get_smap(id = "SPL4SMGP",
                                      # name = "sm_rootzone",
                                      name = variable,
                                      dates = date,
                                      raw_dir = data_out)
  
  soil_moisture_date <- soil_moisture %>%
    names() %>%
    stringr::str_extract("\\_[0-9]+") %>%
    unique() %>%
    stringr::str_replace("\\_","") %>%
    lubridate::as_date()
  
  if (variable == "sm_rootzone")
    var_label <- stringr::str_c(format(lubridate::ymd(soil_moisture_date), '%B %d, %Y'),"\n",
                                "Root zone soil moisture","\n",
                                "Volumetric percent")
  else if (variable == "sm_rootzone_wetness")
    var_label <- stringr::str_c(format(lubridate::ymd(soil_moisture_date), '%B %d, %Y'),"\n",
                                "Root zone soil moisture","\n",
                                "Relative saturation")
  
  soil_moisture %<>%
    # Average over the layers (which represent time slices during the day)
    mean()
  
  if(missing(agg_sf)){
    soil_moisture %<>%
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
      lwgeom::st_transform_proj(mt_state_plane) %>%
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
                   scale_fill_manual(name = var_label,
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
                   mtd_plot() +
                   ggplot2::theme(legend.key.height = unit(0.15,"in"))) %T>%
      save_mt_map(stringr::str_c(soil_moisture_date,"-soil-moisture-",variable,".pdf"))
    
    return(list(data = soil_moisture,
                map = smap_map))
  }
  
  agg_sf_4326 <- agg_sf %>%
    lwgeom::st_transform_proj(raster::projection(soil_moisture))
  
  soil_moisture.vx <- velox::velox(soil_moisture)
  
  soil_moisture_agg <- agg_sf %>%
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
    dplyr::bind_cols(soil_moisture.vx$extract(agg_sf_4326,
                                              fun = function(x){agg_fun(x, na.rm = TRUE)},
                                              df = TRUE,
                                              small = TRUE) %>%
                       dplyr::select(-ID_sp) %>%
                       magrittr::set_names(c("Value"))) %>%
    sf::st_as_sf()
  
  rm(soil_moisture.vx)
  
  smap_map <- (soil_moisture_agg %>%
                      dplyr::mutate(Value = cut(Value,
                                                breaks = c(0,
                                                           0.1,
                                                           0.2,
                                                           0.3,
                                                           0.35,
                                                           0.4,
                                                           0.45,
                                                           0.5,
                                                           0.6,
                                                           1),
                                                labels = c("<10%",
                                                           "10-20%",
                                                           "20-30%",
                                                           "30-35%",
                                                           "35-40%",
                                                           "40-45%",
                                                           "45-50%",
                                                           "50-60%",
                                                           ">60%"),
                                                ordered_result = TRUE)) %>%
                      ggplot2::ggplot() +
                      ggplot2::geom_sf(aes(geometry = Shape,
                                           fill = Value),
                                       color = NA) +
                      # add_hillshade() +
                      # add_counties() +
                    scale_fill_manual(name = var_label,
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
                 mtd_plot() +
                 ggplot2::theme(legend.key.height = unit(0.15,"in"))) %T>%
    save_mt_map(stringr::str_c(soil_moisture_date,"-soil-moisture-",variable,"-aggregated.pdf"))
  
  return(list(data = soil_moisture,
              map = smap_map))
}
