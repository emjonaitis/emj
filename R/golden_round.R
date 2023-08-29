#' Round figures nicely
#'
#' This function rounds an input value based on modified Roche rules.
#' Absolute values between 0 and 1 are rounded to two significant digits;
#' between 1 and 100, to two decimal places; and above 100, to the nearest 
#' whole number. Non-numeric values are returned as-is.
#' 
#' @param x Value to round.
#' @return Rounded value.
#' @keywords emj, round
#' @export

golden_round <- function(x, as.char=TRUE) {
  if (abs(x)>0 & abs(x)<1) {
    x.num <- signif(x, 2)
    x.char <- as.character(x.num)
    x.firstnonzero <- gregexpr("[1-9]", x.char)[[1]][1]
    x.printdec <- x.firstnonzero-1
  } else {
    x.printdec <- ifelse(abs(x)>=1 & abs(x)<=100, 2,
                         ifelse(abs(x)>100 & abs(x)<=1000, 1, 0))
    x.num <- round(x, x.printdec)
  }
  x.round <- ifelse(!is.numeric(x), x, x.num)
  if (as.char==TRUE) {
    x.format <- paste0("%.",x.printdec,"f")
    return(sprintf(x.format, x.num))
  }
  else {
    return(x.num)
  }
}