mtd_leaflet <- function(x,
                        pal =  "RdBu",
                        legend_title = stringr::str_c(format(head(attr(x,"dates"),1), '%B %d, %Y')," - ",
                                                      format(tail(attr(x,"dates"),1), '%B %d, %Y'),"<br>",
                                                      "Average temperature"," (ºF)",", ",
                                                      "deviation from normal"),
                        image_query_title = "Temperature Deviation (ºF)",
                        midpoint = 0,
                        digits = 1,
                        reverse = FALSE,
                        attribution = "Climate data by <a href='http://www.climatologylab.org/gridmet.html' target='_blank'>gridMET</a> via <a href='https://earthengine.google.com/' target='_blank'>Google Earth Engine</a>"){
  
  tm_out <- (x %>%
               tm_shape() + 
               tm_raster(title = "",
                         # legend.show = FALSE,
                         alpha = 1,
                         style= "cont", 
                         # n = 10, 
                         palette = pal, 
                         midpoint = midpoint,
                         legend.reverse = reverse,
                         legend.is.portrait = TRUE) +
               tm_layout(title = legend_title) +
               tm_view(view.legend.position = c("left","bottom"))) %>%
    tmap_leaflet()
  
  tm_out$x$calls[[5]]$args[[5]] <- image_query_title
  
  out <- mtd_leaflet_base(attribution = attribution) %>%
    # leaflet::addRasterImage(x) %>%
    leafem::addImageQuery(x,
                           # type = "click",
                           layerId = image_query_title,
                           prefix = "",
                           digits = digits,
                           position = "bottomleft",
    )
  
  if(reverse){
    tm_out$x$calls[[6]]$args[[1]]$labels %<>% rev()
  }

  
  # tm_out$x$calls[[4]]$args[[4]]$pane <- "background"
  # tm_out$x$calls[[4]]$args[[4]]$attribution <- ""
  # tm_out$x$calls[[4]]$args[[6]] <- ""
  # out$x$calls[[length(out$x$calls)]]$args[[4]]$pane <- "foreground"
  
  
  
  out$x$calls <- c(out$x$calls,tm_out$x$calls[5:6])
  
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
    appendContent(stars) #%>%
    # leaflet.opacity::addOpacitySlider(layerId = image_query_title)
  
  out
  
}
