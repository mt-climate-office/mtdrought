k_to_f <- function(x){
  x %>%
    magrittr::multiply_by(9/5) %>% # C/K to F
    magrittr::subtract(459.67)
}
