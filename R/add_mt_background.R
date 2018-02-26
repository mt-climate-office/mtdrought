add_mt_background <- function(){
  # Plot the county boundaries
  ggplot2::geom_sf(data = mt_state_simple,
                   fill = "white",
                   color = NA)
}
