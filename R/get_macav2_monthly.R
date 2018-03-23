get_macav2_monthly <- function(x = NULL, 
                               elements = c("pr",
                                            "tasmin",
                                            "tasmax"), 
                               # models = c("bcc-csm1-1",
                               #            "bcc-csm1-1-m"),
                               scenarios = c("historical",
                                             "rcp45",
                                             "rcp85"),
                               raw_dir = "./"){
  
  macav2_datasets <- get_macav2_monthly_datasets() 
  macav2_datasets <- elements %>%
    stringr::str_c("_",.,"_") %>%
    purrr::map(~str_subset(macav2_datasets, .x)) %>%
    unlist() %>%
    sort()
  
  # macav2_datasets <- models %>%
  #   stringr::str_c("_",.,"_") %>%
  #   purrr::map(~str_subset(macav2_datasets, .x)) %>%
  #   unlist() %>%
  #   sort()
  
  macav2_datasets <- scenarios %>%
    stringr::str_c("_",.,"_") %>%
    purrr::map(~str_subset(macav2_datasets, .x)) %>%
    unlist() %>%
    sort()
  
  x_bbox <- x %>% 
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    lwgeom::st_transform_proj(4326) %>%
    # magrittr::add(c(360,0)) %>%
    sf::st_bbox()
  
  macav2_datasets %>%
    purrr::map(function(dataset){
      tryCatch({
        
        
        out_file <- stringr::str_c(raw_dir,"/",dataset,".nc")
        
        if(!file.exists(out_file)){
          nc <- ncdf4::nc_open(stringr::str_c('http://thredds.northwestknowledge.net:8080/thredds/dodsC/',dataset,'.nc'))
          var <- names(nc$var)
          ncdf4:::nc_close(nc)
          
          httr::GET(stringr::str_c("http://thredds.northwestknowledge.net:8080/thredds/ncss/",dataset,".nc",
                                   "?",
                                   "var=", var,
                                   "&north=", x_bbox[["ymax"]],
                                   "&west=", x_bbox[["xmin"]],
                                   "&east=", x_bbox[["xmax"]],
                                   "&south=", x_bbox[["ymin"]],
                                   "&disableProjSubset=on",
                                   "&horizStride=1",
                                   "&temporal=all",
                                   "&addLatLon=true",
                                   "&accept=netcdf4"),
                    httr::write_disk(out_file, overwrite = TRUE))
        }
        
        raster::brick(out_file)
        
      },
      error = function(e){return(NULL)})
    }) %>%
    magrittr::set_names(macav2_datasets)
}
