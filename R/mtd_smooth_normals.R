mtd_smooth_normals <- function(x, ...){
  x %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("normal.")),
                     .funs = dplyr::funs(mtd_gam_smooth(x = day, 
                                                        y = ., 
                                                        ...)))
}
