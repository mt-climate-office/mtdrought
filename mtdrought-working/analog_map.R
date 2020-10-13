library(magrittr)
library(sf)
library(ggplot2)

states <- 
  tigris::states(cb = FALSE, refresh = TRUE) %>%
  dplyr::filter(STUSPS %in% c("MT", "ID", "WY", "CO", "UT"))


xmin <- 
  states %>%
  dplyr::filter(STUSPS == "ID") %>% 
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::filter(Y > 48) %$%
  X %>%
  min()

xmax <- 
  states %>%
  dplyr::filter(STUSPS == "MT") %>% 
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::filter(Y > 48) %$%
  X %>%
  max()

ymin <-
  states %>%
  dplyr::filter(STUSPS == "CO") %>% 
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::filter(Y < 38) %$%
  Y %>%
  min()

ymax <-
  states %>%
  dplyr::filter(STUSPS == "MT") %>% 
  sf::st_coordinates() %>%
  tibble::as_tibble()  %$%
  Y %>%
  max()

proj <- 
  list(proj = "lcc",
       lon_0 = mean(c(xmin, xmax)),
       lat_0 = mean(c(ymin, ymax)),
       lat_1 = 41,
       lat_2 = 43) %>%
  paste0("+", names(.),"=",., collapse = " ") %>%
  sf::st_crs()

towns <-
  list(MT = c("Frenchtown","Fort Benton", "Billings"),
       ID = c("Lewiston", "Boise City"),
       CO = c("Grand Junction"),
       UT = c("Salem", "St. George")) %>%
  purrr::imap_dfr(function(x,s){
    tigris::places(s) %>%
      dplyr::filter(NAME %in% x) %>%
      sf::st_centroid() %>%
      tibble::as_tibble()
  }) %>%
  sf::st_as_sf()  %>%
  dplyr::mutate(State = ifelse(STATEFP == 30, "MT", "Other")) %>%
  dplyr::select(Name = NAME,
                State)
  sf::st_transform(proj)



(states %>%
  sf::st_transform(proj) %>%
  rmapshaper::ms_simplify() %>%
  ggplot() +
  geom_sf(colour = rgb(29,78,112,255, maxColorValue = 255),
          fill = rgb(207,232,242,255, maxColorValue = 255),
          size = 1) +
    geom_sf(data = towns,
            mapping = aes(shape = State),
            size = 5,
            color = rgb(29,78,112,255, maxColorValue = 255),
            fill = rgb(29,78,112,255, maxColorValue = 255)) +
    scale_shape_manual(values = c(MT = 21,
                                  Other = 24)) +
  theme_void() +
    theme(legend.position = "none")) %>%
  ggsave(filename = "~/Desktop/Analogs/analog_map.pdf",
         plot = .)


