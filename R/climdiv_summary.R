climdiv_summary <- function(months,
                            year = 2017,
                            element,
                            agg_fun){
  climdiv_raw <- mco_get_climdiv(element) %>%
    dplyr::mutate(Value = ifelse(Value == -99.9, NA, Value),
                  Value = ifelse(element == "pcpn" & Value == -9.99, NA, Value))
  
  climdiv_data <- climdiv_raw %>%
    dplyr::filter(Month %in% months) %>%
    dplyr::group_by(`Division code`,
                    Element,
                    Year) %>%
    dplyr::summarise(Value = agg_fun(Value, na.rm = TRUE))
  
  climdiv_normals <- climdiv_data %>%
    dplyr::filter(Year %in% 1981:2010) %>%
    dplyr::group_by(`Division code`) %>%
    dplyr::summarise(Normal = median(Value, na.rm = TRUE))
  
  climdiv_latest <- climdiv_data %>%
    dplyr::filter(Year == year) %>%
    dplyr::select(-Year) %>%
    dplyr::left_join(climdiv_normals,
                     by = c("Division code")) %>%
    left_join(mt_climate_divisions_simple %>%
                dplyr::mutate(Centroid_x = mt_climate_divisions_simple %>%
                                sf::st_centroid() %>%
                                sf::st_coordinates() %>%
                                tibble::as_tibble() %$%
                                X,
                              Centroid_y = mt_climate_divisions_simple %>%
                                sf::st_centroid() %>%
                                sf::st_coordinates() %>%
                                tibble::as_tibble() %$%
                                Y,
                              Area = mt_climate_divisions_simple %>%
                                sf::st_area()),
              .,
              by = "Division code")
  
  climdiv_state_rank <- climdiv_data %>%
    dplyr::left_join(climdiv_latest %>%
                       sf::st_set_geometry(NULL) %>%
                       dplyr::select(`Division code`,Area)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(Value = weighted.mean(Value,Area))
  
  climdiv_state_rank %<>%
    dplyr::mutate(Deviation = Value - (climdiv_state_rank %>%
                                         dplyr::filter(Year %in% 1981:2010) %$%
                                         Value %>%
                                         median(na.rm = TRUE)),
                  N = n(),
                  Rank = rank(Value,
                                    ties.method = "min",
                                    na.last = "keep"),
                  Percentile = percent_rank(Value)) %>%
    dplyr::filter(Year == max(Year))
  
  return(list(data = climdiv_data,
              normals = climdiv_normals,
              latest = climdiv_latest,
              state_rank = climdiv_state_rank))
}
