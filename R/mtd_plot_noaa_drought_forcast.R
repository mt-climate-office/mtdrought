mtd_plot_noaa_drought_outlook <- function(date,
                                            lead = 1,
                                            data_out = "./data/NOAA_droughtlook"){
  
  noaa_url <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/droughtlook/"
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  # List the files available from the Drought Monitor
  noaa_files <- 
    curl::curl(noaa_url) %>%
    readr::read_table(col_names = FALSE) %$%
    X9 %>%
    stringr::str_subset("sdo")
  
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
                                        full.names = T),
                           quiet = T) %>%
    lwgeom::st_transform_proj(mt_state_plane)
  
  sf::st_agr(noaa_data) = "constant"
  
  noaa_data %>%
    sf::st_intersection(mt_state_simple) %>%
    tidyr::gather(key = "Drought Outlook",
                  value = "True?",
                  FID_improv,FID_persis,FID_dev,FID_Remove) %>%
    dplyr::filter(`True?` == 1) %>%
    dplyr::select(-`True?`) %>%
    dplyr::mutate(`Drought Outlook` = factor(`Drought Outlook`,
                                             levels = c("FID_persis",
                                                        "FID_improv",
                                                        "FID_Remove",
                                                        "FID_dev"),
                                             labels = c("Drought persists",
                                                        "Drought remains but improves",
                                                        "Drought removal likely",
                                                        "Drought development likely")))
  
  legend.name <- stringr::str_c("Three-month drought outlook\nas of ",
                                format(lubridate::ymd(noaa_date), '%B %d, %Y'))
  
  noaa_map <- (noaa_data  %>%
                 ggplot2::ggplot() +
                 geom_sf(aes(fill = `Drought Outlook`),
                         color = "transparent") +
                 scale_fill_manual(name = legend.name,
                                   limits = c("Drought persists",
                                              "Drought remains but improves",
                                              "Drought removal likely",
                                              "Drought development likely"),
                                   labels = c("Drought persists",
                                              "Drought remains\nbut improves",
                                              "Drought removal\nlikely",
                                              "Drought development\nlikely"),
                                   values = c("Drought persists" = rgb(255, 170, 0, maxColorValue = 255),
                                              "Drought remains but improves" = rgb(215, 194, 158, maxColorValue = 255),
                                              "Drought removal likely" = rgb(190, 232, 255, maxColorValue = 255),
                                              "Drought development likely" = rgb(255, 255, 0, maxColorValue = 255)),
                                   guide = guide_legend(title.position = "bottom")) +
                 mtd_plot()# + 
                 # ggplot2::theme(legend.key.height = unit(0.15,"in"))
               ) %T>%
    save_mt_map(stringr::str_c(noaa_date,"-seasonal-drought-outlook.pdf"))
  
  unlink(stringr::str_c(data_out,"/",closest_file) %>%
           tools::file_path_sans_ext())
  
  return(list(data = noaa_data,
              map = noaa_map))
}
