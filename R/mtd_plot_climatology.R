mtd_plot_climatology <- function(x,
                                 title = NULL,
                                 ybreaks = seq(-25,100,25),
                                 ymin = min(ybreaks, na.rm = TRUE),
                                 ymax = max(ybreaks, na.rm = TRUE),
                                 smooth = FALSE,
                                 polar = TRUE,
                                 mean = FALSE,
                                 col = "black",
                                 ...){
  if(smooth){
    x %<>%
      purrr::map(mtd_smooth_normals, ...)
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(aes(x = 1:365,
                             ymin = ymin,
                             ymax = ymax),
                         fill = "gray95") +
    purrr::map2(x,
                col,
                mtd_plot_add_climatology,
                mean = mean) +
    mtd_theme_climatology(ybreaks = ybreaks,
                          title = title,
                          polar = polar)
}
