mtd_get_gridmet <- function(dates = "latest",
                             data_out = "./data/gridmet",
                            pcpn_fun = sum,
                            temp_fun = mean){
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(dates != "latest" && dates >= Sys.Date())
    dates <- "latest"
  
  gridmet <- mcor::mco_get_gridmet(dates = dates,
                                   out_dir = data_out)
  
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

  normals <- mco_get_gridmet_normals()
  
  normals$precipitation_amount %<>%
    magrittr::extract2(gridmet_dates %>%
                         lubridate::yday()) %>%
    raster::calc(pcpn_fun) %>%
    mm_to_in(.)
  
  normals$daily_minimum_temperature %<>%
    magrittr::extract2(gridmet_dates %>%
                         lubridate::yday()) %>%
    raster::calc(temp_fun) %>%
    k_to_f(.)
  
  normals$daily_maximum_temperature %<>%
    magrittr::extract2(gridmet_dates %>%
                         lubridate::yday()) %>%
    raster::calc(temp_fun) %>%
    k_to_f(.)
  
  out <- list(gridmet,normals) %>%
    purrr::transpose() %>%
    purrr::map(function(x){
      raster::brick(x) %>%
        magrittr::set_names(c("value","normals"))
    })
  
  attr(out,"dates") <- gridmet_dates
  return(out)

}
