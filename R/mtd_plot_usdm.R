mtd_plot_usdm <- function(date = "2017-08-30",
                          data_out = "./data/USDM"){
  usdm_url <- "http://droughtmonitor.unl.edu/data/shapefiles_m/"
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  # List the files available from the Drought Monitor
  drought_monitor_files <- 
    usdm_url %>%
    httr::GET() %>%
    httr::content() %>%
    xml2::xml_child("body") %>%
    xml2::xml_child("pre") %>%
    xml2::xml_find_all("//a") %>%
    xml2::as_list() %>%
    unlist()
  
  drought_monitor_dates <- drought_monitor_files %>%
    stringr::str_extract("_([0-9]+)_") %>%
    stringr::str_remove_all("_") %>%
    lubridate::as_date()
  
  closest_file <- drought_monitor_files[which.min(abs(lubridate::as_date(date)-drought_monitor_dates))]
  usdm_date <- closest_file %>%
    stringr::str_extract("_([0-9]+)_") %>%
    stringr::str_remove_all("_") %>%
    lubridate::as_date()
  
  # Get data from the US drought monitor
  FedData::download_data(stringr::str_c(usdm_url,"/",closest_file),
                         destdir = data_out)
  unzip(stringr::str_c(data_out,"/",closest_file),
        exdir = stringr::str_c(data_out,"/",closest_file) %>%
          tools::file_path_sans_ext())
  
  usdm_data <- sf::st_read(stringr::str_c(data_out,"/",closest_file) %>%
                             tools::file_path_sans_ext() %>%
                             list.files(pattern = "\\.shp$",
                                        full.names = T),
                           quiet = T) %>%
    lwgeom::st_transform_proj(mt_state_plane)
  
  sf::st_agr(usdm_data) = "constant"
  
  usdm_data %<>%
    sf::st_intersection(mt_counties_simple %>%
                          sf::st_union()) %>%
    dplyr::mutate(DM = factor(DM,
                              levels = seq(-1,4,1),
                              labels = c("Near normal\nor wet",
                                         "Abnormally\ndry",
                                         "Moderate\ndrought",
                                         "Severe\ndrought",
                                         "Extreme\ndrought",
                                         "Exceptional\ndrought"))) 
  
  legend.name <- stringr::str_c(format(lubridate::ymd(usdm_date), '%B %d, %Y'),"\n",
                                "Drought intensity")
  
  usdm_cols <- c("Near normal\nor wet" = rgb(240, 240, 240, maxColorValue = 255),
                 "Abnormally\ndry" = rgb(255, 255, 0, maxColorValue = 255),
                 "Moderate\ndrought" = rgb(252, 211, 127, maxColorValue = 255),
                 "Severe\ndrought" = rgb(255, 170, 0, maxColorValue = 255),
                 "Extreme\ndrought" = rgb(230, 0, 0, maxColorValue = 255),
                 "Exceptional\ndrought" = rgb(115, 0, 0, maxColorValue = 255))
  
  usdm_map <- (usdm_data %>%
                 ggplot2::ggplot() +
                 geom_sf(aes(fill = DM),
                         color = "transparent") +
                 scale_fill_manual(name = legend.name,
                                   values = usdm_cols,
                                   guide = guide_legend(title.position = "bottom"),
                                   drop = FALSE) +
                 mtd_plot()# +
               # ggplot2::theme(legend.key.height = unit(0.15,"in"))
  )# %T>%
   # save_mt_map(stringr::str_c(usdm_date,"-drought-intensity.pdf"))
  
  unlink(stringr::str_c(data_out,"/",closest_file) %>%
           tools::file_path_sans_ext())
  
  return(list(data = usdm_data,
              map = usdm_map))
}
