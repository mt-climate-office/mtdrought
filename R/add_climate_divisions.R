add_climate_divisions <- function(){
  # Plot the climate division boundaries
  ggplot2::geom_sf(data = mt_climate_divisions_simple,
                   fill = NA,
                   color = "white",
                   size = 0.5)
}
