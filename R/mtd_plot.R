mtd_plot <- function(legend = TRUE){
  list(
    add_hillshade(),
    add_counties(),
    # add_climate_divisions(),
    if(legend) ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.01))),
    if(legend) ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))),
    ggplot2::labs(x = NULL, y = NULL),
    mdt_theme_map()
  )
}
