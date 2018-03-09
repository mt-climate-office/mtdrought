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
