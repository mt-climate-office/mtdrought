mtd_get_climdiv <- function(dates = "latest",
                            raw_dir = "./data/climdiv",
                            pcpn_fun = sum,
                            temp_fun = mean){
  
  dir.create(data_out,
             recursive = TRUE,
             showWarnings = FALSE)
  
  if(dates != "latest" && dates >= Sys.Date())
    dates <- "latest"
  
  climdiv <- mcor::mco_get_climdiv(c("pcpn",
                                     "tmax",
                                     "tmin")) %>%
    dplyr::mutate(Date = lubridate::as_date(
      stringr::str_c(Year,"-",Month,"-15")
      )) %>%
    dplyr::arrange(Element,
                   `Division code`,
                   Date) %>%
    dplyr::mutate(Value = ifelse(Value == -99.9, 
                                 NA, 
                                 Value),
                  Value = ifelse(Element == "pcpn" & Value == -9.99, 
                                 NA, 
                                 Value)) %>%
    na.omit()
  
  if(dates == "latest"){
    dates <- max(climdiv$Date, 
                 na.rm = TRUE)
  } else {
    dates %<>%
      lubridate::as_date() %>% 
      lubridate::round_date("month") %>%
      magrittr::add(lubridate::days(14)) %>%
      unique()
  }
  
  climdiv %<>%
    split(climdiv$Element) %>%
    purrr::map(~ select(.x, -Element)) %>%
    purrr::map(~ rename(.x, value = Value)) %>%
    purrr::map(function(x){
      x %>%
        dplyr::filter(Year %in% 1981:2010) %>%
        dplyr::group_by(`Division code`, 
                        Month) %>%
        dplyr::summarise(normals = mean(value, 
                                       na.rm = TRUE)) %>%
        dplyr::right_join(x, 
                          by = c("Division code",
                                    "Month")) %>%
        dplyr::ungroup() %>%
        dplyr::select(`Division code`,
                      Date,
                      value,
                      normals)
    }) %>%
    purrr::map(~ filter(.x, Date %in% dates))

  return(climdiv)
 
}
