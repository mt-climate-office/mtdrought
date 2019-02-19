mtd_plot_swe_basins <- function(date = "latest",
                                huc = 6,
                                attribution = "<a href='https://www.wcc.nrcs.usda.gov/snow/' target='_blank'>SNOTEL data by NRCS</a>"){
  
  if(date != "latest" && date >= Sys.Date())
    date <- "latest"
  
  swe <- mcor::mco_get_swe_basins(date = date,
                                  huc = huc,
                                  min_stations = 3) %>%
    dplyr::rename(value = `SWE (in)`,
                  normals = `SWE 1981-2010 Median (in)`,
                  percent = `Percent SWE`) %>%
    sf::st_intersection(mt_state_simple)
  
  swe %<>%
    dplyr::filter(sf::st_area(swe) > units::set_units(1000000000,m^2)) %>%
    dplyr::mutate(Watershed = as.character(Watershed),
                  percent = as.numeric(percent)) %>%
    dplyr::arrange(Watershed)
  
  range_swe <- 
    swe$percent %>% 
    range() %>% 
    abs() %>%
    max() %>%
    magrittr::subtract(100) %>%
    magrittr::add(1) %>%
    abs() %>%
    magrittr::multiply_by(c(-1,1)) %>%
    magrittr::subtract(100,.) %>%
    sort()
    
  
  if(date == "latest")
    date <- Sys.Date()
  
  swe_map <- (swe %>%
                ggplot2::ggplot() +
                # Plot the polygon fills
                ggplot2::geom_sf(ggplot2::aes(fill = value / normals * 100),
                                 color = NA) +
                ggplot2::scale_fill_distiller(
                  name = stringr::str_c(format(lubridate::ymd(date), '%B %d, %Y'),"\n",
                                        "Snow water equivalent","\nPercent of normal"),
                  #limits = c(0,1),
                  direction = 1,
                  limits = range_swe,
                  palette = "RdBu",
                  expand = FALSE,
                  guide = ggplot2::guide_colourbar(title.position = "bottom")
                ) +
                mtd_plot() +
                # Plot the polygon boundaries
                ggplot2::geom_sf(fill = NA,
                                 color = "white",
                                 size = 0.5)# +
              # # Plot the labels
              # geom_label(aes(x = Centroid_x,
              #                y = Centroid_y,
              #                label = percent %>%
              #                  paste0("%")),
              #            alpha = 1,
              #            size = 2.25)
  ) %T>%
    save_mt_map(stringr::str_c(date,"-swe.pdf"))
  
  swe %<>%
    dplyr::mutate(Label = paste0("<b>",Watershed,"</b><br>",
                                 "SWE % of Normal: ",percent,"%"))
  
  tm_out <- (swe %>%
               tm_shape() + 
               tm_polygons(col = "percent",
                           border.col = "white",
                           id = "Label",
                           # popup.vars = FALSE,
                           lwd = 2,
                           title = "",
                           alpha = 1,
                           style = "cont",
                           palette = "RdBu",
                           # breaks = seq(range_swe[1],
                           #              range_swe[2],
                           #              1),
                           midpoint = 100,
                           legend.reverse = TRUE,
                           legend.is.portrait = TRUE) +
               tm_layout(title = stringr::str_c(format(lubridate::ymd(date), '%B %d, %Y'),"<br>",
                                                "<a href='../reference.html' target='_blank'>Snow water equivalent, % of normal</a>")) +
               tm_view(view.legend.position = c("left","bottom"))) %>%
    tmap_leaflet()
  
  out <- mtd_leaflet_base(attribution = attribution)
  
  tm_out$x$calls[[6]]$args[[1]]$labels %<>% rev()
  
  # correcting problem with duplicated polygons
  dups <- !duplicated(tm_out$x$calls[[5]]$args[[7]])
  tm_out$x$calls[[5]]$args[[7]] <- tm_out$x$calls[[5]]$args[[7]][dups]
  tm_out$x$calls[[5]]$args[[5]] <- tm_out$x$calls[[5]]$args[[5]][dups]
  tm_out$x$calls[[5]]$args[[4]]$fillColor <- tm_out$x$calls[[5]]$args[[4]]$fillColor[dups]
  tm_out$x$calls[[5]]$args[[4]]$fillOpacity <- tm_out$x$calls[[5]]$args[[4]]$fillOpacity[dups]
  tm_out$x$calls[[5]]$args[[2]] <- tm_out$x$calls[[5]]$args[[2]][dups]
  tm_out$x$calls[[5]]$args[[1]] <- tm_out$x$calls[[5]]$args[[1]][dups]
  
  tm_out$x$calls[[5]]$args[[5]] <- tm_out$x$calls[[5]]$args[[7]] <- swe$Label
  tm_out$x$calls[[5]]$args[[4]] <- tm_out$x$calls[[5]]$args[[4]][-3]
  
  tm_out$x$calls[[5]]$args[[4]]$pane <- "background"
  
  out$x$calls <- c(out$x$calls,tm_out$x$calls[5:6])
  
  out$title <- tm_out$title
  
  out$jsHooks$render <- c(out$jsHooks$render, tm_out$jsHooks$render)
  
  # setwd("./figures")
  
  out %>%
    saveWidgetFix(stringr::str_c("./figures/",date,"-swe.html"),
                  selfcontained = FALSE,
                  libdir = "./site_libs")
  
  # setwd("../")
  
  return(list(data = swe,
              static = list(map = swe_map,
                            path = stringr::str_c("./figures/",date,"-swe.pdf")),
              leaflet = list(map = out,
                             path = stringr::str_c("./figures/",date,"-swe.html"))))
}
