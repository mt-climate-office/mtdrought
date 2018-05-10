mtd_get_macav2_monthly_midcentury_summary <- 
  function (months = 1:12,
            data_out = "./data/",
            pcpn_fun = function(x){sum(x, na.rm = TRUE)},
            temp_fun = function(x){mean(x, na.rm = TRUE)})
  {
    dir.create(data_out,
               showWarnings = FALSE,
               recursive = TRUE)
    
    if(!file.exists(stringr::str_c(data_out,"/macav2_monthly/macav2_monthly_midcentury_medians.Rds"))){
      
      maca_midcentury_medians <- 
        mcor::mco_get_macav2_monthly(x = mt_state %>%
                                       sf::st_buffer(10000),
                                     out_dir = stringr::str_c(data_out,"/macav2_monthly/raw_data/"),
                                     elements = c("pr",
                                                  "tasmin",
                                                  "tasmax"),
                                     scenarios = c("rcp45")) %>%
        dplyr::mutate(data = purrr::map(data,
                                        function(x){
                                          x %>%
                                            raster::subset(
                                              x %>%
                                                names() %>%
                                                gsub("X","",.) %>%
                                                as.Date(format = "%Y.%m.%d") %>%
                                                lubridate::year() %in%
                                                c(2040:2069) %>%
                                                which()
                                            ) 
                                        }))
      
      
      elements <- names(maca_midcentury_medians) %>%
        purrr::map(stringr::str_split, pattern = "_") %>%
        unlist(recursive = F) %>%
        purrr::map_chr(`[[`,3)
      
      models <- names(maca_midcentury_medians) %>%
        purrr::map(stringr::str_split, pattern = "_") %>%
        unlist(recursive = F) %>%
        purrr::map_chr(`[[`,4)
      
      maca_midcentury_medians %<>%
        purrr::map(function(x){
          x %>%
            raster::readAll() %>%
            raster::stackApply(indices = x %>%
                                 names() %>%
                                 stringr::str_remove("X") %>%
                                 lubridate::as_date() %>%
                                 lubridate::month(),
                               fun = median)
        }) %>%
        magrittr::set_names(models) %>%
        split(elements)
      
      
      maca_midcentury_medians %<>%
        magrittr::set_names(c("precipitation_amount",
                              "daily_maximum_temperature",
                              "daily_minimum_temperature"))
      
      maca_midcentury_medians %>%
        readr::write_rds(stringr::str_c(data_out,"/macav2_monthly/macav2_monthly_midcentury_medians.Rds"),
                         compress = "xz")
    }
    
    normals <- mcor::mco_get_gridmet_normals(out_dir = stringr::str_c(data_out,"/gridmet/normals/"))
    
    normals$precipitation_amount %<>%
      magrittr::extract2((1:365) %>%
                           as.character() %>%
                           lubridate::as_date(tz = "America/Denver",
                                              format = "%j") %>%
                           lubridate::month() %>%
                           magrittr::is_in(months) %>%
                           which()
      ) %>%
      raster::calc(pcpn_fun) %>%
      mm_to_in(.)
    
    normals$daily_minimum_temperature %<>%
      magrittr::extract2((1:365) %>%
                           as.character() %>%
                           lubridate::as_date(tz = "America/Denver",
                                              format = "%j") %>%
                           lubridate::month() %>%
                           magrittr::is_in(months) %>%
                           which()
      ) %>%
      raster::calc(temp_fun) %>%
      k_to_f(.)
    
    normals$daily_maximum_temperature %<>%
      magrittr::extract2((1:365) %>%
                           as.character() %>%
                           lubridate::as_date(tz = "America/Denver",
                                              format = "%j") %>%
                           lubridate::month() %>%
                           magrittr::is_in(months) %>%
                           which()
      ) %>%
      raster::calc(temp_fun) %>%
      k_to_f(.)
    
    maca_midcentury_medians <- 
      stringr::str_c(data_out,"/macav2_monthly/macav2_monthly_midcentury_medians.Rds") %>%
      readr::read_rds()
    
    suppressWarnings(
      maca_midcentury_medians$precipitation_amount %<>%
        purrr::map(function(x){
          x[[months]] %>%
            pcpn_fun()
        }) %>%
        raster::brick() %>%
        raster::calc(fun=function(z){quantile(z, probs = c(0.25, 0.5, 0.75), na.rm = T)}) %>%
        magrittr::set_names(c("lower","value","upper")) %>%
        mm_to_in(.) %>%
        raster::rotate() %>%
        raster::projectRaster(normals$precipitation_amount,
                              method = "ngb")
    )
    
    suppressWarnings(
      maca_midcentury_medians$daily_minimum_temperature %<>%
        purrr::map(function(x){
          x[[months]] %>%
            temp_fun()
        }) %>%
        raster::brick() %>%
        raster::calc(fun=function(z){quantile(z, probs = c(0.25, 0.5, 0.75), na.rm = T)}) %>%
        magrittr::set_names(c("lower","value","upper")) %>%
        k_to_f(.) %>%
        raster::rotate() %>%
        raster::projectRaster(normals$daily_minimum_temperature,
                              method = "ngb")
    )
    
    suppressWarnings(
      maca_midcentury_medians$daily_maximum_temperature %<>%
        purrr::map(function(x){
          x[[months]] %>%
            temp_fun()
        }) %>%
        raster::brick() %>%
        raster::calc(fun=function(z){quantile(z, probs = c(0.25, 0.5, 0.75), na.rm = T)}) %>%
        magrittr::set_names(c("lower","value","upper")) %>%
        k_to_f(.) %>%
        raster::rotate() %>%
        raster::projectRaster(normals$daily_maximum_temperature,
                              method = "ngb")
    )
    
    
    out <- list(maca_midcentury_medians,normals) %>%
      purrr::transpose() %>%
      purrr::map(function(x){
        raster::brick(x) %>%
          magrittr::set_names(c("lower","value","upper","normals"))
      })
    
    attr(out,"months") <- months
    
    out
    
  }
