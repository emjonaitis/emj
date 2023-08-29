#' Convert nano to pico
#'
#' This function is suitable for converting a measurement in nanograms/mL to 
#' picograms/mL. If the input is character, the function will coerce the input
#' to numeric first, then reconvert it to character before returning it.
#' 
#' @param x Value to convert.
#' @return Converted value.
#' @keywords emj, conversion
#' @export

nano_to_pico <- function(x) {
  if (!is.numeric(x)) {
    return(as.character(1000*as.numeric(x)))
  }
  else {
    return (1000*x)
  }
}