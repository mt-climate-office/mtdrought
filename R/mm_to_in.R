mm_to_in <- function(x){
  x %>%
    magrittr::multiply_by(0.0393701) # mm to inches
}
