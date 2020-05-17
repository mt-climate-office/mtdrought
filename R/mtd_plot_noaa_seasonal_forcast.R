mtd_plot_noaa_seasonal_forecast <- function(date,
                                            element,
                                            data_out = "./data/NOAA_seasonal"){
  
  noaa_url <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/"
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(element == "temp"){
    unit_symbol <- "ºF"
    long_name <- "Temperature"
    descriptors <- c("warmer","cooler")
  }else if(element == "prcp"){
    unit_symbol <- "in."
    long_name <- "Net precipitation"
    descriptors <- c("wetter","drier")
  } else {
    stop("Element passed was not allowed! Please use either 'temp' or 'prcp'.")
  }
  
  # List the files available from the Drought Monitor
  noaa_files <- 
    suppressWarnings(
      curl::curl("ftp://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/")
    ) %>%
    # httr::content() %>%
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
  
  closest_file <- which.min(abs(lubridate::as_date(date)-noaa_dates))
  if(abs(lubridate::as_date(date)-noaa_dates)[closest_file] < 27)
    closest_file <- closest_file - 1
  
  
  closest_file <- noaa_files[closest_file]
  
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
  
  months <- lubridate::as_date(date) %>%
    lubridate::month() %>%
    magrittr::add(0:2)
  months[months>12] <- months[months>12] - 12
  
  months %<>%
    magrittr::extract(month.name, .) %>%
    stringr::str_sub(1,1) %>%
    stringr::str_flatten()
  
  chance_levels <- c(stringr::str_c(c("Very likely","Likely","Somewhat likely"),descriptors[1], sep = " "),
                     "Equal Chances",
                     stringr::str_c(rev(c("Very likely","Likely","Somewhat likely")),descriptors[2], sep = " "))
  
  noaa_data <- sf::st_read(stringr::str_c(data_out,"/",closest_file) %>%
                             tools::file_path_sans_ext() %>%
                             list.files(pattern = "\\.shp$",
                                        full.names = T) %>%
                             stringr::str_subset(months) %>%
                             stringr::str_subset("lead[1-9]_"),
                           quiet = T) %>%
    sf::st_transform(mt_state_plane)
  
  sf::st_agr(noaa_data) = "constant"
  
  noaa_data %<>%
    sf::st_make_valid() %>%
    sf::st_intersection(mt_counties_simple %>%
                          sf::st_union()) %>%
    dplyr::mutate(Chance = ifelse(Prob > 60, "Very likely", 
                                  ifelse(Prob >= 40, "Likely", "Somewhat likely")),
                  Prob = ifelse(Cat == "Below",0 - Prob, Prob),
                  Prob = ifelse(Cat == "EC",0, Prob),
                  Prob = factor(Prob,
                                levels = c(seq(-100,-40,10),
                                           -33,
                                           0,
                                           33,
                                           seq(40,100,10)),
                                ordered = TRUE),
                  Chance = stringr::str_c(Chance," ", descriptors[match(Cat,c("Above","Below"))]),
                  Chance = ifelse(Cat == "EC", "Equal Chances", Chance),
                  Chance = factor(Chance,
                                  levels = chance_levels),
                  Cat = factor(Cat,
                               levels = c("Above",
                                          "EC",
                                          "Below")) %>%
                    forcats::fct_recode(`Above average` = "Above",
                                         `Equal chances` = "EC",
                                         `Below average` = "Below"))
  
  the.months <- lubridate::as_date(date) %>%
    lubridate::month() %>%
    magrittr::add(c(0,2))
  
  if(any(the.months > 12))
    add.year = TRUE else
      add.year = FALSE
  
  the.months[the.months>12] <- the.months[the.months>12]-12
  
  legend.name <- stringr::str_c(the.months %>%
                            magrittr::extract(month.name, .) %>%
                            stringr::str_flatten(collapse = "-"),
                          ", ",lubridate::year(lubridate::ymd(date)),
                          if(add.year){stringr::str_c("-",lubridate::year(lubridate::ymd(date)) + 1)},"\n",
                          long_name)
  
  # noaa_map <- (noaa_data  %>%
  #                ggplot2::ggplot() +
  #                geom_sf(aes(fill = Prob),
  #                        color = NA) +
  #                scale_fill_manual(name = legend.name,
  #                                  limits = rev(c(seq(-100,-40,20),
  #                                                 -33,
  #                                                 0,
  #                                                 33,
  #                                                 seq(40,100,20))),
  #                                  labels = rev(c(seq(100,40,-20),
  #                                                 33,
  #                                                 "EC",
  #                                                 33,
  #                                                 seq(40,100,20))),
  #                                  values = (if(element == "prcp")
  #                                    c(colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")[1:5])(8),
  #                                      RColorBrewer::brewer.pal(11,"BrBG")[6],
  #                                      colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")[7:11])(8)) else
  #                                        c(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu")[11:7])(8),
  #                                          RColorBrewer::brewer.pal(11,"RdBu")[6],
  #                                          colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu")[5:1])(8))) %>%
  #                                    magrittr::set_names(c(seq(-100,-40,10),
  #                                                          -33,
  #                                                          0,
  #                                                          33,
  #                                                          seq(40,100,10))),
  #                                  guide = guide_legend(title.position = "bottom")) +
  # 
  #                mtd_plot()# +
  #                # ggplot2::theme(legend.key.height = unit(0.15,"in"))
  #              ) %T>%
  #   save_mt_map(stringr::str_c(noaa_date,"-",element,"-seasonal-forecast.pdf"))
  
  noaa_map <- (noaa_data  %>%
                 ggplot2::ggplot() +
                 geom_sf(aes(fill = Chance),
                         color = "transparent") +
                 scale_fill_manual(name = legend.name,
                                   drop = FALSE,
                                   na.translate = FALSE,
                                   values = (if(element == "prcp")
                                     rev(RColorBrewer::brewer.pal(7,"BrBG")) else
                                       RColorBrewer::brewer.pal(7,"RdBu")) %>%
                                     magrittr::set_names(chance_levels),
                                   guide = guide_legend(title.position = "bottom")) +
                 
                 mtd_plot()# + 
               # ggplot2::theme(legend.key.height = unit(0.15,"in"))
  ) #%T>%
    #save_mt_map(stringr::str_c(noaa_date,"-",element,"-seasonal-forecast.pdf"))
  
  unlink(stringr::str_c(data_out,"/",closest_file) %>%
           tools::file_path_sans_ext())
  
  return(list(data = noaa_data,
              map = noaa_map))
}
