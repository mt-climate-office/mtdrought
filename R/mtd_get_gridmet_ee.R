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

mtd_get_gridmet_ee <- function(start_date = "2018-01-01", 
                               end_date = "2018-12-31", 
                               base_name = 'GRIDMET',
                               raw_dir = "./data/GRIDMET",
                               overwrite = FALSE){
  
  GRIDMET <- getGridmet(start_date = start_date,
                        end_date = end_date,
                        base_name = base_name)
  
  dates <- GRIDMET %$%
    dates %>%
    purrr::map(as.character) %>%
    unlist() %>%
    lubridate::as_date()
  
  GRIDMET["dates"] <- NULL
  
  start_date <- head(dates,1)
  end_date <- tail(dates,1)
  
  outfile <- paste0(raw_dir, "/", 
                    base_name,
                    "_",
                    start_date,"_",end_date,
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
    
    attr(GRIDMET,"dates") <- dates
    
    readr::write_rds(GRIDMET, outfile, compress = "gz")
    
    return(GRIDMET)
  } else {
    
    return(readr::read_rds(outfile))
  }
}

mtd_get_gridmet_climatology_ee <- function(collection = "IDAHO_EPSCOR/GRIDMET", 
                                           vars = c("pr", "tmmn", "tmmx"),
                                           # geom = mcor::mt_state_simple %>% sf::st_bbox() %>% sf::st_as_sfc(),
                                           start_date = '1981-01-01', 
                                           end_date = '2011-01-01'){
  
  out <- getDailyClimatology(collection = collection,
                             vars = vars,
                             # geom = geom %>%
                             #   sf::st_geometry() %>%
                             #   geojsonio::geojson_json(convert_wgs84 = TRUE) %>%
                             #   as.character(),
                             start_date = start_date,
                             end_date = end_date) %>% 
    purrr::transpose() %>% 
    map(unlist) %>% 
    tibble::as_data_frame() %>%
    dplyr::mutate(Day = 1:365) %>%
    dplyr::select(Day, everything()) %>%
    tidyr::gather(key = "Variable", 
                  value = "Value",
                  -Day) %>%
    tidyr::separate(col = Variable, 
                    into = c("Variable","Statistic"), 
                    sep = "_") %>%
    tidyr::spread(Statistic, Value) %>%
    dplyr::rename(`0%` = `0`,
                  `25%` = `25`,
                  `50%` = `50`,
                  `75%` = `75`,
                  `100%` = `100`) %>%
    dplyr::select(Day, Variable, mean, `0%`, `25%`, `50%`, `75%`, `100%`) %>%
    # tidyr::gather(Statistic, Value, -Day, -Variable) %>%
    # tidyr::spread(Variable, Value) %>%
    # dplyr::mutate(pr = pr * 0.0393701,
    #               tmmn = ((tmmn - 273.15) * (9/5)) + 32,
    #               tmmx = ((tmmx - 273.15) * (9/5)) + 32) %>%
    # dplyr::rename(prcp = pr,
    #               tmin = tmmn,
    #               tmax = tmmx) %>%
    # tidyr::gather(Variable, Value, -Day, -Statistic) %>%
    # tidyr::spread(Statistic, Value) %>%
    tidyr::nest(-Variable) 
  
  names(out$data) <- out$Variable
  
  return(out$data)
  
}
