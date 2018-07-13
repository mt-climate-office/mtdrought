reticulate::source_python("../ee/getMOD16A2.py")

mtd_get_mod16a2 <- function(start_date = "2018-01-01", 
                            end_date = "2018-12-31", 
                            base_name = 'MOD16A2',
                            raw_dir = "./data/MOD16A2",
                            overwrite = FALSE){
  
  MOD16A2 <- getMOD16A2(start_date = start_date,
                        end_date = end_date,
                        base_name = base_name)
  
  dates <- MOD16A2$dates %>% 
    purrr::transpose() %>%
    magrittr::extract2("properties") %>%
    purrr::transpose() %>%
    magrittr::extract2("system:index") %>%
    unlist() %>%
    lubridate::as_date() %>%
    sort()
  
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
    out.temp <- paste0(tempdir(), "/", base_name, ".zip")
    unlink(out.temp,
           recursive = TRUE, 
           force = TRUE)
    unlink(paste0(tempdir(), "/", base_name),
           recursive = TRUE, 
           force = TRUE)
    
    download.file(MOD16A2$path,
                  destfile = out.temp)
    
    unzip(out.temp,
          exdir = paste0(tempdir(), "/", base_name))
    
    out <- 
      paste0(tempdir(), "/", base_name) %>%
      list.files(pattern = ".tif",
                 full.names = TRUE) %>%
      raster::stack(quick = TRUE) %>%
      raster::readAll()
    
    names(out) %<>%
      stringr::str_extract("ET_[^_]+$") %>%
      stringr::str_remove("ET_") %>%
      as.integer() %>%
      tidyr::replace_na(0) %>%
      stringr::str_pad(2, pad = "0")
    
    out <- out[[out %>% names() %>% sort()]]
    
    names(out) <- 
      dates
    
    out[out == 32761] <- NA
    
    readr::write_rds(out, outfile, compress = "gz")
    
    return(out)
  } else {
    return(readr::read_rds(outfile))
  }
}

mtd_get_mod16a2_normals <- function(base_name = 'MOD16A2',
                                    raw_dir = "./data/MOD16A2",
                                    overwrite = FALSE){
  
  outfile <- paste0(raw_dir, "/", 
                    base_name,
                    "_normals.rds")
  
  if(overwrite || !file.exists(outfile)){
    
    unlink(outfile,
           recursive = TRUE, 
           force = TRUE)
    
    unlink(paste0(tempdir(), "/", base_name),
           recursive = TRUE, 
           force = TRUE)
    
    dir.create(paste0(tempdir(), "/", base_name),
               recursive = TRUE,
               showWarnings = FALSE)
    
    MOD16A2 <- getMOD16A2normals() %>%
      purrr::imap(function(x, name){
        
        out.temp <- paste0(tempdir(), "/", 
                           base_name, "/", 
                           name,".zip")
        
        unlink(paste0(tempdir(), "/", 
                      base_name, "/", 
                      name),
               recursive = TRUE, 
               force = TRUE)
        
        download.file(x$path,
                      destfile = out.temp)
        
        unzip(out.temp,
              exdir = paste0(tempdir(), "/", base_name, "/", name))
        
        out <- 
          paste0(tempdir(), "/", base_name, "/", name) %>%
          list.files(pattern = ".tif",
                     full.names = TRUE) %>%
          raster::stack(quick = TRUE) %>%
          raster::readAll()
        
        names(out) %<>%
          stringr::str_extract("_[^_]+$") %>%
          stringr::str_remove("_") %>%
          as.integer() %>%
          tidyr::replace_na(0) %>%
          stringr::str_pad(2, pad = "0")
        
        out <- out[[out %>% names() %>% sort()]]
        
        names(out) <- 
          x$dates
        
        return(out)
      })
    
    readr::write_rds(MOD16A2, outfile, compress = "gz")
    
    return(MOD16A2)
  } else {
    return(readr::read_rds(outfile))
  }
}



