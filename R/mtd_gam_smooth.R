mtd_gam_smooth <- function(x,
                           y,
                           ...){
  tibble::tibble(x = x,
                 y = y) %>%  
    mgcv::gam(y ~ s(x,
                    bs = "cc"),
              data = .,
              ...) %$%
    fitted.values
}
