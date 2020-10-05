TMV = read.csv("tortoise_movement_data.csv")

monthday_from_absolute <- function(x){
  if (x <= 31) {
    return(c(1, x))
  }
  else if (x <= 59) {
    return(c(2, x-31))
  }
  else if (x <= 90) {
    return(c(3, x-59))
  }
  else if (x <= 120) {
    return(c(4, x-90))
  }
  else if (x <= 151) {
    return(c(5, x-120))
  }
  else if (x <= 181) {
    return(c(6, x-151))
  }
  else if (x <= 212) {
    return(c(7, x-181))
  }
  else if (x <= 243) {
    return(c(8, x-212))
  }
  else if (x <= 273) {
    return(c(9, x-243))
  }
  else if (x <= 304) {
    return(c(10, x-273))
  }
  else if (x <= 334) {
    return(c(11, x-304))
  }
  else if (x <= 365) {
    return(c(12, x-334))
  }
}

nw_md = lapply(TMV$Day, monthday_from_absolute)
nw_md
