mtd_plot_climatology <- function(x,
                                 title = NULL,
                                 ybreaks = seq(-25,100,25),
                                 ymin = min(ybreaks, na.rm = TRUE),
                                 ymax = max(ybreaks, na.rm = TRUE),
                                 smooth = FALSE,
                                 polar = TRUE,
                                 mean = FALSE,
                                 col = "black",
                                 family = gaussian(),
                                 ...){
  if(smooth){
    x %<>%
      purrr::map(mtd_smooth_normals, 
                 family = family)
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(aes(x = x[[1]]$date,
                             ymin = ymin,
                             ymax = ymax),
                         fill = "gray95") +
    purrr::map2(x,
                col,
                mtd_plot_add_climatology,
                mean = mean) +
    mtd_theme_climatology(ybreaks = ybreaks,
                           title = title,
                           polar = polar) +
    ggplot2::geom_ribbon(aes(x = x[[1]]$date,
                             ymin = head(ybreaks,1),
                             ymax = tail(ybreaks,1)),
                         fill = NA,
                         color = "grey60", 
                         size = 0.5,
                         na.rm = TRUE)
}
