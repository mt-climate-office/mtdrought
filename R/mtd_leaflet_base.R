mtd_leaflet_base <- function(attribution = ""){
  out <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    leaflet::addPolygons(data = mcor::mt_state %>%
                           sf::st_transform(4326)) %>%
    leaflet::addMapPane("background", zIndex = 410) %>%
    leaflet::addMapPane("middleground", zIndex = 420) %>%
    leaflet::addMapPane("foreground", zIndex = 430) %>%
    leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a",
             attribution = attribution,
             options = leaflet::tileOptions(pane = "foreground")) %>%
    leaflet::addProviderTiles("Stamen.TonerLines",
                              options = leaflet::tileOptions(pane = "foreground")) %>%
    leaflet::addProviderTiles("Stamen.TonerLabels",
                              options = leaflet::tileOptions(pane = "foreground")) %>%
    leaflet::addScaleBar(position = "bottomright",
                options = leaflet::scaleBarOptions(metric = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
    leaflet.extras::addFullscreenControl(position = "topright") %>%
    leaflet::addEasyButton(easyButton(icon = "ion-arrow-shrink", 
                                      title = "Reset View", 
                                      position = "topright",
                                      onClick = JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
    htmlwidgets::onRender(JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>%
    leaflet::addEasyButton(leaflet::easyButton(
      icon="fa-crosshairs", 
      title="Locate Me",
      position = "topright",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    leaflet.extras::addSearchOSM()
  
  out$x$calls <- out$x$calls[-1]
  
  out
}