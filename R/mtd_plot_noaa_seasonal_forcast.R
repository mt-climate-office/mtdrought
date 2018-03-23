mtd_plot_noaa_seasonal_forecast <- function(date,
                                            element,
                                            lead = 1,
                                            data_out = "./data/NOAA_seasonal"){
  
  noaa_url <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/"
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  # List the files available from the Drought Monitor
  noaa_files <- 
    suppressWarnings(
      httr::GET(
        noaa_url,
        httr::authenticate("anonymous", "")
      )
    ) %>%
    httr::content() %>%
    readr::read_table(col_names = FALSE) %$%
    X9 %>%
    stringr::str_subset("seas") %>%
    stringr::str_subset(element) %>%
    stringr::str_subset("^((?!abv_).)*$") %>%
    stringr::str_subset("^((?!bel_).)*$")
  
  noaa_dates <- noaa_files %>%
    stringr::str_extract("_([0-9]+)") %>%
    stringr::str_remove_all("_") %>%
    lubridate::ymd(truncated = 2)
  
  closest_file <- noaa_files[which.min(abs(lubridate::as_date(date)-noaa_dates))]
  
  noaa_date <- closest_file %>%
    stringr::str_extract("_([0-9]+)") %>%
    stringr::str_remove_all("_") %>%
    lubridate::ymd(truncated = 2)
  
  
  # Get data from the US drought monitor
  FedData::download_data(stringr::str_c(noaa_url,closest_file),
                         destdir = data_out)
  unzip(stringr::str_c(data_out,"/",closest_file),
        exdir = stringr::str_c(data_out,"/",closest_file) %>%
          tools::file_path_sans_ext())
  
  noaa_data <- sf::st_read(stringr::str_c(data_out,"/",closest_file) %>%
                             tools::file_path_sans_ext() %>%
                             list.files(pattern = "\\.shp$",
                                        full.names = T) %>%
                             stringr::str_subset(stringr::str_c("lead",lead,"_")),
                           quiet = T) %>%
    lwgeom::st_transform_proj(mt_state_plane) %>%
    sf::st_intersection(mt_counties_simple %>%
                          sf::st_union()) %>%
    dplyr::mutate(Prob = ifelse(Cat == "Below",0 - Prob, Prob),
                  Prob = ifelse(Cat == "EC",0, Prob),
                  Prob = factor(Prob,
                                levels = c(seq(-100,-40,10),
                                           -33,
                                           0,
                                           33,
                                           seq(40,100,10)),
                                ordered = TRUE)) 
  
  noaa_map <- (noaa_data  %>%
                 ggplot2::ggplot() +
                 geom_sf(aes(fill = Prob),
                         color = NA) +
                 scale_fill_manual(name = stringr::str_c("Three-month Forecast\n",
                                                         "Probability Above/Below Normal\nas of ",
                                                         format(lubridate::ymd(noaa_date), '%B %d, %Y')),
                                   limits = rev(c(seq(-100,-40,20),
                                                  -33,
                                                  0,
                                                  33,
                                                  seq(40,100,20))),
                                   labels = rev(c(seq(100,40,-20),
                                                  33,
                                                  "EC",
                                                  33,
                                                  seq(40,100,20))),
                                   values = (if(element == "prcp")
                                                   c(colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")[1:5])(8),
                                                     RColorBrewer::brewer.pal(11,"BrBG")[6],
                                                     colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")[7:11])(8)) else
                                                   c(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu")[11:7])(8),
                                                     RColorBrewer::brewer.pal(11,"RdBu")[6],
                                                     colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu")[5:1])(8))) %>%
                                     magrittr::set_names(c(seq(-100,-40,10),
                                                           -33,
                                                           0,
                                                           33,
                                                           seq(40,100,10))),
                                   guide = guide_legend(title.position = "bottom")) +
                 add_hillshade() +
                 add_counties() +
                 add_climate_divisions() +
                 # # Plot the labels
                 # geom_label(data = climdiv_tmpc$normals %>%
                 #              left_join(mt_climate_divisions_simple %>%
                 #                          dplyr::mutate(Centroid_x = mt_climate_divisions_simple %>%
                 #                                          sf::st_centroid() %>%
                 #                                          sf::st_coordinates() %>%
                 #                                          tibble::as_tibble() %$%
                 #                                          X,
                 #                                        Centroid_y = mt_climate_divisions_simple %>%
                 #                                          sf::st_centroid() %>%
                 #                                          sf::st_coordinates() %>%
                 #                                          tibble::as_tibble() %$%
                 #                                          Y),
                 #                        .,
                 #                        by = "Division code"),
                 #            aes(x = Centroid_x,
                 #                y = Centroid_y,
                 #                label = stringr::str_c("1981-2010 normal: \n",
                 #                                       Normal %>%
                 #                                         round(digits = 1) %>%
                 #                                         paste0(" ºF"))),
                 #            alpha = 1,
                 #            size = 2.25) +
                 mdt_theme_map()) %T>%
    save_mt_map(stringr::str_c(noaa_date,"-",element,"-seasonal-forecast.pdf"))
  
  unlink(stringr::str_c(data_out,"/",closest_file) %>%
           tools::file_path_sans_ext())
  
  return(list(data = noaa_data,
              map = noaa_map))
}
