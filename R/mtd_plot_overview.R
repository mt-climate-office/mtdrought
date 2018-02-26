mtd_plot_overview <- function(){
  (mt_climate_divisions_simple %>%
     dplyr::mutate(Centroid_x = mt_climate_divisions_simple %>%
                     sf::st_centroid() %>%
                     sf::st_coordinates() %>%
                     tibble::as_tibble() %$%
                     X,
                   Centroid_y = mt_climate_divisions_simple %>%
                     sf::st_centroid() %>%
                     sf::st_coordinates() %>%
                     tibble::as_tibble() %$%
                     Y) %>%
     ggplot2::ggplot() +
     geom_sf(aes(geometry = Shape,
                 text = Division),
             fill = NA,
             color = "white") +
     add_mt_background() +
     add_hillshade() +
     add_counties() +
     add_climate_divisions() +
     #     # Plot the climate division boundaries
     # ggplot2::geom_sf(data = mt_climate_divisions_simple,
     #                  fill = NA,
     #                  color = "black",
     #                  size = 1) +
     # Plot the labels
     geom_label(aes(x = Centroid_x,
                    y = Centroid_y,
                    label = Division),
                alpha = 1,
                size = 2.25) +
     mdt_theme_map()) %T>%
    save_mt_map("climate-divisions.pdf")
}
