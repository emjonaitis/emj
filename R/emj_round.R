#' Round nicely
#'
#' This function takes in a number or vector of numbers and rounds it to two decimal 
#' places for large numbers, or two significant figures for small ones.
#' It returns a string or vector of strings.
#' @param number Number to round.
#' @param lb Lower bound. An entry whose absolute value is below this limit will be handled separately.
#' @param censor Governs the behavior of the lb parameter. If TRUE, absolute values below the lower bound will be formatted as left-censored (e.g., "<.0001"). If FALSE, values below the lower bound will be rounded to 0. When no lower bound is specified, censor=TRUE has no effect.
#' @import dplyr 
#' @import magrittr
#' @export

emj_round <- function(number, lb=0, censor=FALSE) {
  options(scipen=99)
  df     <- data.frame(number) %>%
            mutate(index=c(1:n()), isNA=is.na(number))
  df.num   <- filter(df, isNA==FALSE & abs(number) > lb) 
  if (nrow(df.num)>0) {
    df.num <- df.num %>%
              mutate(d = 2+I(log10(abs(number))<0)*as.integer(abs(log10(abs(number)))),
                     fmt = paste0("%.",d,"f"),
                     rr = round(number, digits=d),
                     out = sprintf(fmt, rr)) %>%
              dplyr::select(index, out)
  }
  df.na  <- filter(df, isNA==TRUE) %>%
            mutate(out = "") %>%
            dplyr::select(index, out)
  if (censor==TRUE & lb>0) {
    df.zero <- filter(df, abs(number)<=lb) %>%
               mutate(out = paste0("<", lb)) %>%
               dplyr::select(index, out)
    
  } else {
    df.zero <- filter(df, abs(number)<=lb) %>%
               mutate(out = "0") %>%
               dplyr::select(index, out)
  }
  df        <- rbind(df.na, df.num, df.zero) %>%
               merge(df, .)
  return(df$out)
}