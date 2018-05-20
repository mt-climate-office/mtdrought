mtd_aggregate_normals <- function(rast, 
                                  mask = mcor::mt_state){
  rast %>%
    mcor::mco_mask(mask) %>%
    raster::cellStats(mean) %>%
    as.vector()
}
