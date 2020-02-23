reticulate::source_python("../ee/getGRIDMET.py")

download_ee <- function(path){
  out.zip <- paste0(tempfile(),".zip")
  out.dir <- tempfile()
  out.dir %>%
    dir.create(showWarnings = FALSE,
               recursive = TRUE)
  
  download.file(path,
                destfile = out.zip)
  
  unzip(out.zip,
        exdir = out.dir)
  
  out.dir %>%
    list.files(pattern = ".tif",
               full.names = TRUE) %>%
    raster::stack(quick = TRUE) %>%
    raster::readAll()
  
}

rename_layers <- function(brick){
  names(brick)[grepl("value", names(brick))] <- "value"
  names(brick)[grepl("_0_", names(brick))] <- "normal 000"
  names(brick)[grepl("_25_", names(brick))] <- "normal 025"
  names(brick)[grepl("_50_", names(brick))] <- "normal 050"
  names(brick)[grepl("_75_", names(brick))] <- "normal 075"
  names(brick)[grepl("_100_", names(brick))] <- "normal 100"
  names(brick)[grepl("_mean_", names(brick))] <- "normal mean"
  
  return(brick[[order(names(brick))]])
}

mtd_get_gridmet_ee <- function(collection = "IDAHO_EPSCOR/GRIDMET", 
                               vars = list("pr" = "sum", 
                                           "tmmn" = "mean", 
                                           "tmmx" = "mean"),
                               start_date = "2018-01-01", 
                               end_date = "2018-12-31",
                               raw_dir = "./data/GRIDMET",
                               overwrite = FALSE){
  
  GRIDMET <- getGridmet(collection = collection, 
                        vars = vars,
                        start_date = start_date,
                        end_date = end_date)
  
  dates <- GRIDMET$dates %>%
    purrr::map(as.character) %>%
    unlist() %>%
    lubridate::as_date()
  
  GRIDMET["dates"] <- NULL
  
  start_date <- head(dates,1)
  end_date <- tail(dates,1)
  
  outfile <- paste0(raw_dir, "/", start_date,"_",end_date,
                    ".rds")
  
  if(overwrite || !file.exists(outfile)){
    
    unlink(outfile,
           recursive = TRUE, 
           force = TRUE)
    
    GRIDMET <- GRIDMET %>%
      purrr::map(function(x){
        
        x %>%
          purrr::map(download_ee) %>%
          raster::brick()
        
      })
    
    GRIDMET %<>%
      purrr::map(mcor::mco_mask,
                 mcor::mt_state) %>%
      purrr::map(rename_layers)
    
    attr(GRIDMET,"dates") <- dates
    
    readr::write_rds(GRIDMET, 
                     outfile, 
                     compress = "gz")
    
  }
  
  return(outfile %>%
           readr::read_rds())
  
}

mtd_get_gridmet_series_ee <- function(collection = "IDAHO_EPSCOR/GRIDMET", 
                                      vars = c("pr", "tmmn", "tmmx"),
                                      # geom = mcor::mt_state_simple %>% sf::st_bbox() %>% sf::st_as_sfc(),
                                      start_date = '1981-01-01', 
                                      end_date = '2011-01-01'){
  
  values <- getDailySeries(collection = collection,
                           vars = vars,
                           # geom = geom %>%
                           #   sf::st_geometry() %>%
                           #   geojsonio::geojson_json(convert_wgs84 = TRUE) %>%
                           #   as.character(),
                           start_date = start_date,
                           end_date = end_date)
  
  dates <- values$dates %>%
    purrr::map(as.character) %>%
    unlist() %>%
    lubridate::as_date()
  
  value <- values$data %>% 
    unlist() %>%
    tibble::tibble(variable = names(.), value = .) %>%
    tidyr::separate(variable,
                    into = c("variable","day"),
                    sep="_",
                    fill = "right",
                    extra = "merge") %>%
    tidyr::replace_na(list(day = "0")) %>%
    dplyr::mutate(day = as.integer(day),
                  day = day + 1,
                  date = dates[day]) %>%
    dplyr::arrange(variable, date) %>%
    dplyr::select(date, 
                  variable, 
                  value)
  
  normal <- mtd_get_gridmet_climatology_ee(collection = collection,
                                           vars = vars)
  
  out <- value %>%
    dplyr::mutate(day = lubridate::yday(date)) %>%
    dplyr::right_join(tibble::tibble(date = seq(from = start_date %>% lubridate::as_date(), 
                                                to = end_date %>% lubridate::as_date(), 
                                                by = 1)) %>%
                        dplyr::mutate(day = lubridate::yday(date)) %>%
                        tidyr::crossing(tibble::tibble(variable = vars)),
                      by = c("date","day","variable")) %>%
    
    dplyr::left_join(normal, by = c("day","variable")) %>%
    dplyr::select(date,
                  day,
                  variable,
                  dplyr::everything()) %>%
    tidyr::nest(data = -variable)
  
  names(out$data) <- out$variable
  
  return(out$data)
  
}

mtd_get_gridmet_climatology_ee <- function(collection = "IDAHO_EPSCOR/GRIDMET", 
                                           vars = c("pr", "tmmn", "tmmx"),
                                           # geom = mcor::mt_state_simple %>% sf::st_bbox() %>% sf::st_as_sfc(),
                                           start_date = '1981-01-01', 
                                           end_date = '2011-01-01'){
  
  # days <- seq(from = lubridate::as_date(start_date),
  #             to = min(lubridate::as_date(end_date), lubridate::today() - 2),
  #             by = 1) %>%
  #   lubridate::yday() %>%
  #   unique() %>%
  #   sort()
  
  getDailyClimatology(collection = collection,
                      vars = vars,
                      start_date = start_date,
                      end_date = end_date) %>% 
    purrr::transpose() %>% 
    map(unlist) %>% 
    tibble::as_tibble() %>%
    dplyr::mutate(day = 1:366) %>%
    dplyr::select(day, everything()) %>%
    tidyr::gather(key = "variable", 
                  value = "value",
                  -day) %>%
    tidyr::separate(col = variable, 
                    into = c("variable","statistic"), 
                    sep = "_") %>%
    tidyr::spread(statistic, value) %>%
    dplyr::rename(`normal.000` = `0`,
                  `normal.025` = `25`,
                  `normal.050` = `50`,
                  `normal.075` = `75`,
                  `normal.100` = `100`,
                  `normal.mean` = mean) %>%
    dplyr::select(day, 
                  variable, 
                  `normal.mean`, 
                  `normal.000`,
                  `normal.025`,
                  `normal.050`,
                  `normal.075`,
                  `normal.100`)
  
}

mtd_calculate_gridmet_enso_climatology_ee <- function(){
  
  # Get the Oceanic Niño Index ENSO definition table
  oni <- mtd_get_enso_dates() %>%
    dplyr::filter(Days %in% seq(from = lubridate::as_date(start_date),
                                to = lubridate::as_date(end_date) - 1,
                                by = 1)) %>%
    tidyr::nest(Days)
  
  names(oni$data) <- oni$ENSO
  
  oni <- oni$data %>%
    purrr::map(`[[`,"Days") %>%
    purrr::map(as.character) %>%
    purrr::map(stringr::str_remove_all,"-")
  
  calculateENSOClimatology(dates = oni$`El Niño`,
                           name = 'GRIDMET_MT_1981-2010_ElNino_Normals')
  calculateENSOClimatology(dates = oni$`ENSO Neutral`,
                           name = 'GRIDMET_MT_1981-2010_ENSONeutral_Normals')
  calculateENSOClimatology(dates = oni$`La Niña`,
                           name = 'GRIDMET_MT_1981-2010_LaNina_Normals')
  
}

rename_layers_enso <- function(brick){
  names(brick)[grepl("value", names(brick)) & grepl("_0_", names(brick))] <- "value 000"
  names(brick)[grepl("value", names(brick)) & grepl("_25_", names(brick))] <- "value 025"
  names(brick)[grepl("value", names(brick)) & grepl("_50_", names(brick))] <- "value 050"
  names(brick)[grepl("value", names(brick)) & grepl("_75_", names(brick))] <- "value 075"
  names(brick)[grepl("value", names(brick)) & grepl("_100_", names(brick))] <- "value 100"
  names(brick)[grepl("value", names(brick)) & grepl("_mean_", names(brick))] <- "value mean"
  names(brick)[grepl("normal", names(brick)) & grepl("_0_", names(brick))] <- "normal 000"
  names(brick)[grepl("normal", names(brick)) & grepl("_25_", names(brick))] <- "normal 025"
  names(brick)[grepl("normal", names(brick)) & grepl("_50_", names(brick))] <- "normal 050"
  names(brick)[grepl("normal", names(brick)) & grepl("_75_", names(brick))] <- "normal 075"
  names(brick)[grepl("normal", names(brick)) & grepl("_100_", names(brick))] <- "normal 100"
  names(brick)[grepl("normal", names(brick)) & grepl("_mean_", names(brick))] <- "normal mean"
  
  return(brick[[order(names(brick))]])
}

mtd_get_enso_ee <- function(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_ElNino_Normals',
                            vars = list("pr" = "sum", 
                                        "tmmn" = "mean", 
                                        "tmmx" = "mean"),
                            start_date = start_date, 
                            end_date = end_date){
  getENSO(collection = collection,
          vars = vars,
          start_date = start_date, 
          end_date = end_date) %>%
    purrr::map(function(x){
      
      x %>%
        purrr::map(download_ee) %>%
        raster::brick()
      
    }) %>%
    purrr::map(mcor::mco_mask,
               mcor::mt_state) %>%
    purrr::map(rename_layers_enso)
}

mtd_get_all_enso_ee <- function(start_date = "2018-12-01", 
                                end_date = "2019-03-01"){
  
  list(
    "El Niño" = mtd_get_enso_ee(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_ElNino_Normals',
                                start_date = start_date, 
                                end_date = end_date),
    "La Niña" = mtd_get_enso_ee(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_LaNina_Normals',
                                start_date = start_date, 
                                end_date = end_date),
    "ENSO Neutral" =  mtd_get_enso_ee(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_ENSONeutral_Normals',
                                      start_date = start_date, 
                                      end_date = end_date)
  )
  
}

mtd_get_enso_series_ee <- function(){
  
  out <- list(
    "El Niño" = getENSOSeries(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_ElNino_Normals'),
    "La Niña" = getENSOSeries(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_LaNina_Normals'),
    "ENSO Neutral" =  getENSOSeries(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_ENSONeutral_Normals'),
    "Normal" = getENSOSeries(collection = 'users/bocinsky/GRIDMET_MT_1981-2010_Normals')
  )
  
  out %>%
    purrr::transpose() %>% 
    map(unlist) %>% 
    do.call(rbind,.) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(day = 1:365) %>%
    dplyr::select(day, everything()) %>%
    tidyr::gather(key = "variable", 
                  value = "value",
                  -day) %>%
    tidyr::separate(col = variable, 
                    into = c("climate","variable"), 
                    sep = "\\.") %>%
    tidyr::separate(col = variable, 
                    into = c("variable","statistic"), 
                    sep = "_") %>%
    tidyr::spread(variable, value) %>%
    dplyr::mutate(pr = mm_to_in(pr),
                  tmmn = k_to_f(tmmn),
                  tmmx = k_to_f(tmmx)) %>%
    tidyr::gather(variable, value, -day, -climate, -statistic) %>%
    spread(statistic, value) %>%
    dplyr::rename(`0%` = `0`,
                  `25%` = `25`,
                  `50%` = `50`,
                  `75%` = `75`,
                  `100%` = `100`,
                  `mean` = mean) %>%
    tidyr::gather(statistic, value, -day, -climate, -variable) %>%
    dplyr::mutate(climate = factor(climate),
                  statistic = factor(statistic, 
                                     levels = c("mean",
                                                "0%",
                                                "25%",
                                                "50%",
                                                "75%",
                                                "100%")))
  
}
