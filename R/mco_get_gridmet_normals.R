mco_get_gridmet_normals <- 
  function(raw_dir = "./data/gridmet/normals")
  {
    if(!file.exists(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))){
      gridmet <- mcor::mco_get_gridmet(dates = c("1981-01-01","2010-12-31")) %>%
        purrr::map(function(x){
          out <- x %>%
            raster::brick()
          
          raster::projection(out) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          
          out.days <- out %>%
            names() %>%
            stringr::str_remove("X") %>%
            # as.integer() %>%
            # magrittr::add(lubridate::as_date("1900-01-01")) %>%
            lubridate::yday()
          
          out %<>%
            raster::zApply(by = out.days,
                           fun = mean)
          
          # out %<>%
          #   raster::t() %>%
          #   raster::flip("x")
          
          gc()
          gc()
          
          out[[1:365]]
          
        })
      
      gridmet %>%
        readr::write_rds(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))
      
      # gridmet_dimensions <- stars::st_dimensions(gridmet[[1]] %>%
      #                                              stars::st_as_stars())
      # 
      # names(gridmet_dimensions) <- c("x","y","time")
      # gridmet_dimensions$time$offset <- lubridate::as_date("1981-01-01")
      # gridmet_dimensions$time$delta <- lubridate::as_date("1981-01-02") - lubridate::as_date("1981-01-01")
      # gridmet_dimensions$time$values <- list(NULL)
      # 
      # gridmet %<>%
      #   stars::st_as_stars()
      # 
      # attr(gridmet, which = "dimensions") <- gridmet_dimensions
      # 
      # names(gridmet) <- c("prcp","tmin","tmax")
      # 
      # gridmet$prcp %<>% as.array() %>%
      #   units::set_units(mm)
      # 
      # gridmet$tmin %<>% as.array() %>%
      #   units::set_units(K)
      # 
      # gridmet$tmax %<>% as.array() %>%
      #   units::set_units(K)
      # 
      # gridmet %>%
      #   readr::write_rds(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))
    }
    
    readr::read_rds(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))
    
  }
