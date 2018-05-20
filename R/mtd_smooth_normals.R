mtd_smooth_normals <- function(x, ...){
  x %>%
    dplyr::mutate_at(.vars = dplyr::vars(-YDAY),
                     .funs = dplyr::funs(mtd_gam_smooth(x = YDAY, y = ., ...)))
}
