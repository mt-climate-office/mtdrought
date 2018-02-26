get_macav2_monthly_datasets <- function(){
  xml2::read_html("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html") %>%
    xml2::as_list() %$%
    html %$%
    body %$%
    table %>%
    purrr::map(function(x){x$td$a %>% attr("href")}) %>%
    unlist() %>%
    magrittr::set_names(NULL) %>%
    gsub("reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html?dataset=",
         "",
         x = .,
         fixed = T)
}
