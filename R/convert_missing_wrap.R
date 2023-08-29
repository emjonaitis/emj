#' Clean up missing values in UP data
#'
#' This function takes in a column of raw UP data and returns a column with missing value codes replaced as NA.
#' @param x Vector containing untransformed WRAP data.
#' @return Vector containing cleaned UP data.
#' @seealso \code{\link{UP_harm}}
#' @keywords UP
#' @export

convert_missing_wrap <- Vectorize(function(x) {
  # if (is.null(raw.name) | !is.character(raw.name) | !(raw.name %in% missval$variable)) {
  #   stop("Unrecognized raw.name value. See the help file for a list of permitted names.")
  # }
  # These are hard-coded WRAP missing value options. Does not work for Vitamin B
  replace <- c(666,777,888,999)
  newval <- ifelse(max(x==replace)==TRUE, NA, x)
  newval.n <- tryCatch(as.numeric(newval),
                       warning=function(w) {message(paste("We found a problem.\nInput value:",x,
                                                          "\nNew value:",newval,
                                                          "\nMissing values:",
                                                          paste(replace, collapse=",")))})
  if (is.null(newval.n)) {
    newval.out <- NA
  } else {newval.out <- newval.n}
  newval.out
})
