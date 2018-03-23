mtd_get_macav2_midcentury_summary <- 
  function (months = 1:3,
            element = "tasmax",
            agg_fun = mean,
            data_out = "./data/macav2_monthly")
  {
    dir.create(data_out,
               showWarnings = FALSE,
               recursive = TRUE)
    
    if(!file.exists(stringr::str_c(data_out,"/maca_midcentury_medians_",element,".Rds"))){
      
      maca_midcentury_medians <- 
        get_macav2_monthly(x = mt_state,
                           raw_dir = stringr::str_c(data_out,"/raw_data/"),
                           elements = element,
                           scenarios = c("rcp45")) %>%
        purrr::compact() %>%
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
        readr::write_rds(stringr::str_c(data_out,"/maca_midcentury_medians_",element,".Rds"),
                         compress = "xz")
    }
    
    stringr::str_c(data_out,"/maca_midcentury_medians_",element,".Rds") %>%
      readr::read_rds() %>%
      purrr::map(function(x){
        x[[month.abb[months]]] %>%
          agg_fun()
      }) %>%
      raster::brick() %>%
      raster::calc(fun=function(x){quantile(x, probs = c(0.25, 0.5, 0.75))}) %>%
      {if(element == "pr") 
        mm_to_in(.) 
        else 
          k_to_f(.)}
    
  }
