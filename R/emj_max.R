#' Min and max with NA
#'
#' These functions take in a vector of numbers and returns the minimum or maximum.
#' If no non-missing values are present, NA is returned.
#' 
#' @param x Vector to compute minimum or maximum of.
#' @export
#' 
emj_max <- function(x) {
  x_na <- na.omit(x)
  if(length(x_na)==0) {
    NA
  } else {
    max(x_na)
  }
}