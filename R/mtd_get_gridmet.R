mtd_get_gridmet <- function(dates = "latest",
                            raw_dir = "./data/gridmet",
                            pcpn_fun = sum,
                            temp_fun = mean){
  
  dir.create(raw_dir,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(dates != "latest" && dates >= Sys.Date())
    dates <- "latest"
  
  gridmet <- mcor::mco_get_gridmet(dates = dates,
                                   out_dir = raw_dir)
  
  gridmet_dates <- gridmet[[1]] %>%
    names() %>%
    stringr::str_replace("X","") %>%
    lubridate::as_date()
  
  gridmet$precipitation_amount %<>%
    raster::calc(pcpn_fun) %>%
    mm_to_in(.)
  
  gridmet$daily_minimum_temperature %<>%
    raster::calc(temp_fun) %>%
    k_to_f(.)
  
  gridmet$daily_maximum_temperature %<>%
    raster::calc(temp_fun) %>%
    k_to_f(.)
  
  
  ## Normals
  normals <- mcor::mco_get_gridmet_normals(out_dir = stringr::str_c(raw_dir,"/normals/"))
  
  gridmet_normals$precipitation_amount %<>% 
    purrr::map(mm_to_in)
  
  gridmet_normals$daily_minimum_temperature %<>% 
    purrr::map(k_to_f)
  
  gridmet_normals$daily_maximum_temperature %<>% 
    purrr::map(k_to_f)
  
  climatology <- 
    gridmet_normals %>%
    purrr::map(function(x){
      x %>%
        purrr::map(mtd_aggregate_normals) %>%
        tibble::as_tibble() %>% 
        dplyr::mutate(YDAY = 1:365)
    })
  
  normals$precipitation_amount$mean %<>%
    magrittr::extract2(gridmet_dates %>%
                         lubridate::yday()) %>%
    raster::calc(pcpn_fun)
  
  normals$daily_minimum_temperature$mean %<>%
    magrittr::extract2(gridmet_dates %>%
                         lubridate::yday()) %>%
    raster::calc(temp_fun)
  
  normals$daily_maximum_temperature$mean %<>%
    magrittr::extract2(gridmet_dates %>%
                         lubridate::yday()) %>%
    raster::calc(temp_fun)
  
  normals %<>% purrr::transpose()
  
  out <- list(gridmet,normals$mean) %>%
    purrr::transpose() %>%
    purrr::map(function(x){
      raster::brick(x) %>%
        magrittr::set_names(c("value","normals"))
    })
  
  attr(out,"dates") <- gridmet_dates
  return(out)
  
}
