add_counties <- function(){
  # Plot the county boundaries
  ggplot2::geom_sf(data = mt_counties_simple,
                   fill = NA,
                   color = "white",
                   size = 0.1)
}
