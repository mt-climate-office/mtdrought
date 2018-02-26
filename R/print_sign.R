print_sign <- function(x){
  out <- sapply(x,function(y){
    if(y>0) return("+")
    if(y<=0) return("")
    # if(y==0) return("")
    stop("Cannot calculate sign!")
  })
  return(out)
  
}
