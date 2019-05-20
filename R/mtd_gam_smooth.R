mtd_gam_smooth <- function(x,
                           y,
                           family = gaussian()){
 tibble::tibble(x = x,
                 y = y) %>%  
    mgcv::gam(y ~ s(x,
                    bs = "cc"),
              data = .,
              family = family) %$%
    fitted.values
}
