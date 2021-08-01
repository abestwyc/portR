star <- function(x) {
  stopifnot(is.numeric(x))
  if (x <= .01) {
    "***"
  } else if (x <= .05) {
    "** "
  } else if (x <= .1) {
    "*  "
  } else{
    "   "
  }
}
