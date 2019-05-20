mtd_smooth_normals <- function(x, family = gaussian()){
  x %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("normal.")),
                     .funs = list(~mtd_gam_smooth(x = day, 
                                                  y = .,
                                                  family = family)))
}
