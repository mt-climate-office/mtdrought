mtd_leaflet <- function(x,
                        pal =  RColorBrewer::brewer.pal(11,"RdBu"),
                        legend_title = stringr::str_c(format(head(attr(x,"dates"),1), '%B %d, %Y')," - ",
                                                      format(tail(attr(x,"dates"),1), '%B %d, %Y'),"<br>",
                                                      "Average temperature"," (ºF)",", ",
                                                      "deviation from normal"),
                        image_query_title = "Temperature Deviation (ºF)",
                        midpoint = 0,
                        digits = 1){
  
  tm_out <- (x %>%
               tm_shape() + 
               tm_raster(title = "",
                         # legend.show = FALSE,
                         alpha = 0.5,
                         style= "pretty", 
                         n = 10, 
                         palette = rev(pal), 
                         midpoint = midpoint,
                         legend.reverse = TRUE,
                         legend.is.portrait = FALSE) +
               tm_layout(title = legend_title) +
               tm_view(view.legend.position = c("left","bottom"))) %>%
    tmap_leaflet()
  
  tm_out$x$calls[[4]]$args[[5]] <- image_query_title
  
  out <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    addRasterImage(x) %>%
    addProviderTiles("Stamen.TonerLines") %>%
    addProviderTiles("Stamen.TonerLabels") %>%
    addImageQuery(x,
                  layerId = image_query_title,
                  prefix = "",
                  digits = digits,
                  position = "bottomleft") %>%
    # addHomeButton(ext = raster::extent((gridmet$tmean$value - gridmet$tmean$normal.mean)), 
    #               layer.name = "Reset Zoom",
    #               position = "topleft") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(metric = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {
                          L.control.zoom({ position: 'topright' }).addTo(this)
}") %>%
    leaflet.extras::addFullscreenControl(position = "topright") 
  
  
  out$x$calls[[2]] <- tm_out$x$calls[[4]]
  out$x$calls <- c(out$x$calls, tm_out$x$calls[5])
  out$title <- tm_out$title
  tm_out$jsHooks$render[[1]]$code %<>% 
    stringr::str_remove_all("\t") %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove(stringr::fixed("function(el, x) {"))
  
  out$jsHooks$render[[1]]$code %<>%
    stringr::str_remove_all("\t") %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_replace_all("  "," ") %>%
    stringr::str_replace_all("  "," ") %>%
    stringr::str_replace_all("  "," ") %>%
    stringr::str_remove("\\}$") %>%
    stringr::str_c(tm_out$jsHooks$render[[1]]$code)
  
  out
  
}
