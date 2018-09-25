mtd_get_oni <- function(){
  # Read the three-month seasonal Oceanic Niño Index
  oni <- readr::read_table("http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
  
  # ENSO Events are defined as 5 consecutive overlapping 3-month periods at or above the +0.5º anomaly for warm (El Niño) events and at or below the -0.5º anomaly for cold (La Niña) events.
  la_nina_rle <- (oni$ANOM <= -0.5) %>%
    rle()
  la_nina_rle$values <- la_nina_rle %$%
    tibble::tibble(lengths = lengths,
                   values = values) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(`La Niña` = ifelse(lengths >= 5 & values,T,F)) %$%
    `La Niña`
  oni$`La Niña` <- inverse.rle(la_nina_rle)
  
  el_nino_rle <- (oni$ANOM >= 0.5) %>%
    rle()
  el_nino_rle$values <- el_nino_rle %$%
    tibble::tibble(lengths = lengths,
                   values = values) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(`El Niño` = ifelse(lengths >= 5 & values,T,F)) %$%
    `El Niño`
  oni$`El Niño` <- inverse.rle(el_nino_rle)
  
  oni %<>%
    dplyr::mutate(Neutral = ifelse(`La Niña` | `El Niño`,F,T),
                  ENSO = ifelse(`La Niña`, 
                                "La Niña",
                                ifelse(`El Niño`, 
                                       "El Niño",
                                       "ENSO Neutral"))) %>%
    dplyr::select(SEAS,
                  YR,
                  TOTAL,
                  ANOM,
                  ENSO) %>%
    dplyr::rename(Season = SEAS,
                  Year = YR,
                  ONI = TOTAL,
                  Anomaly = ANOM)
  
  oni
}

mtd_season_to_dates <- function(season, year){
  seasons <- list(
    "DJF" = c("Dec","Jan","Feb"),
    "JFM" = c("Jan","Feb","Mar"),
    "FMA" = c("Feb","Mar","Apr"),
    "MAM" = c("Mar","Apr","May"),
    "AMJ" = c("Apr","May","Jun"),
    "MJJ" = c("May","Jun","Jul"),
    "JJA" = c("Jun","Jul","Aug"),
    "JAS" = c("Jul","Aug","Sep"),
    "ASO" = c("Aug","Sep","Oct"),
    "SON" = c("Sep","Oct","Nov"),
    "OND" = c("Oct","Nov","Dec"),
    "NDJ" = c("Nov","Dec","Jan")
  )
  
  if(season == "DJF"){
    years <- c(year-1,year,year)
  }else if(season == "NDJ"){
    years <- c(year,year,year+1)
  }else{
    years <- c(year,year,year)
  }
  
  months <- match(seasons[[season]], month.abb)
  
  purrr::pmap(.l = list(x = months, y = years), 
              .f = function(x,y){
                lubridate::ymd(stringr::str_c(y,"-",x), truncated = 1)
              }) %>% 
    purrr::map(.f = function(x){
      seq(from = x,
          by = 1,
          length.out = lubridate::days_in_month(x))
    }) %>%
    do.call(c, .)
  
}

mtd_get_enso_dates <- function(){
  
  mtd_get_oni() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Days = list(
      mtd_season_to_dates(season = Season,
                          year = Year))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ENSO) %>%
    dplyr::summarise(Days = list(do.call(c, Days) %>%
                                   unique() %>%
                                   sort())) %>%
    tidyr::unnest()
  
}
