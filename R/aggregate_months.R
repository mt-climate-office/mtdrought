aggregate_months <- function(x,
                             months = 6:8,
                             months_fun = sum,
                             agg_stat = median){
  
  x %<>%
    raster::subset(
      x %>%
        names() %>%
        gsub("X","",.) %>%
        as.Date(format = "%Y.%m.%d") %>%
        lubridate::month() %in% 
        c(months) %>%
        which()
    )
  
  x %>%
    raster::stackApply(indices = x %>%
                         names() %>%
                         gsub("X","",.) %>%
                         as.Date(format = "%Y.%m.%d") %>%
                         lubridate::year(),
                       fun = months_fun) %>%
    agg_stat()
}
