get_bcsd_swe_midcentury_summary <- 
  function (month = 4,
            data_out = "./data/BCSD_mon_VIC")
  {
    dir.create(data_out,
               showWarnings = FALSE,
               recursive = TRUE)
    
    if(!file.exists(stringr::str_c(data_out,"/bcsd_swe_midcentury_medians.Rds"))){
      
      macav2_datasets <- get_macav2_monthly_datasets() %>%
        stringr::str_subset("_pr_") %>%
        stringr::str_subset("_rcp45_") %>%
        sort()
      
      BCSD_swe_vars <- thredds::tds_ncss_list_vars("https://cida.usgs.gov/thredds/ncss/BCSD_mon_VIC/dataset.html") %>%
        stringr::str_subset("swe") %>%
        stringr::str_subset("_rcp45_") %>%
        sort()
      
      bcsd_swe_midcentury_medians <- 
        BCSD_swe_vars %>%
        purrr::map_chr(function(var){
          thredds::tds_ncss_download(ncss_url = "https://cida.usgs.gov/thredds/ncss/BCSD_mon_VIC/dataset.html",
                                     out_file = stringr::str_c(data_out,"/raw_data/",var,".nc"),
                                     bbox = mt_climate_divisions_simple %>% 
                                       sf::st_bbox() %>%
                                       sf::st_as_sfc() %>%
                                       sf::st_transform(4326) %>%
                                       # magrittr::add(c(360,0)) %>%
                                       sf::st_bbox(),
                                     vars = var,
                                     overwrite = FALSE)
        }) %>%
        purrr::compact() %>%
        purrr::map(raster::brick) %>%
        purrr::map(function(x){
          midcentury <- x %>%
            raster::subset(
              x %>%
                names() %>%
                gsub("X","",.) %>%
                as.Date(format = "%Y.%m.%d") %>%
                lubridate::year() %in%
                c(2040:2069) %>%
                which()
            ) 
          
          midcentury %>%
            raster::stackApply(indices = midcentury %>%
                                 names() %>%
                                 gsub("X","",.) %>%
                                 as.Date(format = "%Y.%m.%d") %>%
                                 lubridate::month(),
                               fun = median) %>%
            magrittr::set_names(month.abb)
        }) %T>%
        readr::write_rds(stringr::str_c(data_out,"/bcsd_swe_midcentury_medians.Rds"),
                         compress = "xz")
    }
    
    stringr::str_c(data_out,"/bcsd_swe_midcentury_medians.Rds") %>%
      readr::read_rds() %>%
      purrr::map(function(x){
        x[[month.abb[month]]]
      }) %>%
      raster::brick() %>%
      raster::calc(fun=function(x){
        quantile(x, 
                 probs = c(0.25, 0.5, 0.75),
                 na.rm = TRUE)
      }) %>%
      mm_to_in()
    
  }
