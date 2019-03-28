mtd_plot_smap <- function(date,
                          data_out = "./data/SMAP",
                          variable = "sm_rootzone_wetness"){
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(date != "latest" && date >= Sys.Date())
    date <- "latest"
  
  soil_moisture <- mcor::mco_get_smap_appeears(name = variable,
                                               start_date = date,
                                               end_date = date,
                                               raw_dir = data_out)
  
  soil_moisture_date <- soil_moisture %>%
    names() %>%
    stringr::str_remove("X") %>%
    # stringr::str_extract("\\_[0-9]+") %>%
    # unique() %>%
    # stringr::str_replace("\\_","") %>%
    lubridate::as_date()
  
  image_query_title = "Root zone soil moisture (%)"

    var_label <- stringr::str_c(format(lubridate::ymd(soil_moisture_date), '%B %d, %Y'),"\n",
                                "Root zone soil moisture","\n",
                                "Relative saturation")
  
  
  
  # soil_moisture %<>%
  #   # Average over the layers (which represent time slices during the day)
  #   mean()
  
  soil_moisture_rast <- soil_moisture
  
  
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
                                            ">60%")))
  
  sf::st_agr(soil_moisture) = "constant"
  
  soil_moisture %<>%
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
                                              "10-20%" = rgb(230, 0, 0, maxColorValue = 255),
                                              "20-30%" = rgb(255, 170, 0, maxColorValue = 255),
                                              "30-35%" = rgb(252, 211, 127, maxColorValue = 255),
                                              "35-40%" = rgb(255, 255, 0, maxColorValue = 255),
                                              "40-45%" = rgb(240, 240, 240, maxColorValue = 255),
                                              "45-50%" = rgb(190, 232, 255, maxColorValue = 255),
                                              "50-60%" = rgb(0, 112, 255, maxColorValue = 255),
                                              ">60%" = rgb(0, 38, 115, maxColorValue = 255)),
                                   guide = guide_legend(title.position = "bottom")) +
                 mtd_plot()# +
               # ggplot2::theme(legend.key.height = unit(0.15,"in"))
  )# %T>%
  # save_mt_map(stringr::str_c(soil_moisture_date,"-soil-moisture-",variable,".pdf"))
  
  var_label <- stringr::str_c(format(lubridate::ymd(soil_moisture_date), '%B %d, %Y'),"<br>",
                              "<a href='../reference.html' target='_blank'>Root zone soil moisture</a>","<br>",
                              "Relative saturation")
  
  attribution = "Soil moisture data by <a href='https://smap.jpl.nasa.gov/' target='_blank'>NASA SMAP</a>"
  
  tm_out <- (soil_moisture_rast %>%
               tm_shape() + 
               tm_raster(title = "",
                         alpha = 1,
                         legend.is.portrait = TRUE,
                         legend.reverse = TRUE,
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
                         palette = c("<10%" = rgb(115, 0, 0, maxColorValue = 255),
                                     "10-20%" = rgb(230, 0, 0, maxColorValue = 255),
                                     "20-30%" = rgb(255, 170, 0, maxColorValue = 255),
                                     "30-35%" = rgb(252, 211, 127, maxColorValue = 255),
                                     "35-40%" = rgb(255, 255, 0, maxColorValue = 255),
                                     "40-45%" = rgb(240, 240, 240, maxColorValue = 255),
                                     "45-50%" = rgb(190, 232, 255, maxColorValue = 255),
                                     "50-60%" = rgb(0, 112, 255, maxColorValue = 255),
                                     ">60%" = rgb(0, 38, 115, maxColorValue = 255)),
                         labels = c("<10%",
                                    "10-20%",
                                    "20-30%",
                                    "30-35%",
                                    "35-40%",
                                    "40-45%",
                                    "45-50%",
                                    "50-60%",
                                    ">60%")) +
    tm_layout(title = var_label) +
    tm_view(view.legend.position = c("left","bottom"))) %>%
  tmap_leaflet()
  
  tm_out$x$calls[[4]]$args[[5]] <- image_query_title
  
  out <- mtd_leaflet_base(attribution = attribution) %>%
    # leaflet::addRasterImage(x) %>%
    mapview::addImageQuery(soil_moisture_rast*100,
                           # type = "click",
                           layerId = image_query_title,
                           prefix = "",
                           digits = 1,
                           position = "bottomleft",
    )
  
  # Reverse
  # if(reverse){
    # tm_out$x$calls[[5]]$args[[1]]$labels %<>% rev()
  # }
  
  
  # tm_out$x$calls[[4]]$args[[4]]$pane <- "background"
  # tm_out$x$calls[[4]]$args[[4]]$attribution <- ""
  # tm_out$x$calls[[4]]$args[[6]] <- ""
  # out$x$calls[[length(out$x$calls)]]$args[[4]]$pane <- "foreground"
  
  
  
  out$x$calls <- c(out$x$calls,tm_out$x$calls[4:5])
  
  out$title <- tm_out$title
  
  # tm_out$jsHooks$render[[1]]$code %<>%
  #   stringr::str_replace("document.getElementsByClassName","el.getElementsByClassName")
  
  out$jsHooks$render <- c(out$jsHooks$render, tm_out$jsHooks$render)
  
  out$jsHooks$render %<>%
    purrr::map(function(x){
      x$code %<>%
        stringr::str_remove_all("\\t") %>%
        stringr::str_remove_all("\\n")
      
      return(x)
    })
  
  stars <- out$dependencies %>%
    purrr::keep(~ .x$name == "stars") %>%
    magrittr::extract2(1) %$%
    paste0(src$file,"/",script[[1]]) %>%
    readr::read_file() %>%
    tags$script()
  
  out$dependencies %<>%
    purrr::discard(~ .x$name == "stars")
  
  # out$jsHooks$render <- c(out$jsHooks$render,
  #                         list(list(code = stars,
  #                              data = NULL)))
  
  out %<>%
    appendContent(stars)

return(list(data = soil_moisture_rast,
            leaflet = out,
            map = smap_map))

}
