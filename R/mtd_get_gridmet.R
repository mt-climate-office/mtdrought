mtd_get_gridmet <- function(dates,
                            raw_dir = "./data/gridmet",
                            pcpn_fun = sum,
                            temp_fun = mean,
                            overwrite = FALSE){
  
  dir.create(raw_dir,
             recursive = TRUE,
             showWarnings = FALSE)
  
  outfile <- paste0(raw_dir, "/", head(dates,1),"_",tail(dates,1),
                    ".rds")
  
  if(file.exists(outfile) & !overwrite){
    return(outfile %>%
             readr::read_rds())
  }
  
  # if(dates != "latest" && dates >= Sys.Date())
  #   dates <- "latest"
  
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
  
  outfile <- paste0(raw_dir, "/", head(gridmet_dates,1),"_",tail(gridmet_dates,1),
                    ".rds")
  
  if(overwrite || !file.exists(outfile)){
    unlink(outfile,
           recursive = TRUE, 
           force = TRUE)
    
    ## Normals
    # normals <- mcor::mco_get_gridmet_normals(out_dir = stringr::str_c(raw_dir,"/normals/"))
    
    normals <- 
      mcor::mco_get_gridmet_normals(out_dir = stringr::str_c(raw_dir,"/normals/")) %>%
      purrr::map(
        function(x){
          x %>%
            purrr::map(function(i){
              i[[lubridate::as_date(gridmet_dates) %>%
                   lubridate::yday()]]
            })
        }
      )
    
    normals$precipitation_amount %<>%
      purrr::map(raster::calc, fun = base::sum, na.rm = TRUE) %>%
      raster::brick() %>%
      mm_to_in()
    
    normals$daily_minimum_temperature %<>%
      purrr::map(raster::calc, fun = base::mean, na.rm = TRUE) %>%
      raster::brick() %>%
      k_to_f()
    
    normals$daily_maximum_temperature %<>%
      purrr::map(raster::calc, fun = base::mean, na.rm = TRUE) %>%
      raster::brick() %>%
      k_to_f()
    
    normals %<>%
      purrr::map(magrittr::set_names, c("normal.000", 
                                        "normal.0025", 
                                        "normal.025", 
                                        "normal.050", 
                                        "normal.075",
                                        "normal.0975",
                                        "normal.100", 
                                        "normal.mean")) %>%
      purrr::map(magrittr::extract2, c("normal.000", 
                                       "normal.025", 
                                       "normal.050", 
                                       "normal.075",
                                       "normal.100", 
                                       "normal.mean"))
    
    gridmet$precipitation_amount %<>%
      magrittr::set_names("value") %>%
      as.list() %>%
      c(
        normals$precipitation_amount %>% 
          as.list()
      ) %>%
      raster::brick()
    
    gridmet$daily_minimum_temperature %<>%
      magrittr::set_names("value") %>%
      as.list() %>%
      c(
        normals$daily_minimum_temperature %>% 
          as.list()
      ) %>%
      raster::brick()
    
    gridmet$daily_maximum_temperature %<>%
      magrittr::set_names("value") %>%
      as.list() %>%
      c(
        normals$daily_maximum_temperature %>% 
          as.list()
      ) %>%
      raster::brick()
    
    gridmet %<>%
      magrittr::set_names(c("pr","tmmn","tmmx")) %>%
      purrr::map(mcor::mco_mask,
                 mcor::mt_state)
    
    attr(gridmet,"dates") <- gridmet_dates
    
    readr::write_rds(gridmet, 
                     outfile, 
                     compress = "gz")
  }
  
  c("precipitation_amount",
    "daily_minimum_temperature",
    "daily_maximum_temperature") %T>%
    {
      paste0(raw_dir, "/", ., ".Rds") %>%
        purrr::walk(unlink,
                    recursive = TRUE, 
                    force = TRUE)
    } %>%
    paste0(raw_dir, "/", ., ".nc") %>%
    purrr::walk(unlink,
                recursive = TRUE, 
                force = TRUE)
  
  return(outfile %>%
           readr::read_rds())
  
}
