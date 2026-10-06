#' Round nicely
#'
#' This function takes in a number or vector of numbers and rounds it to two decimal 
#' places for large numbers, or two significant figures for small ones.
#' It returns a string or vector of strings.
#' @param number Number to round.
#' @param ll Lower bound. Below this, the number will be formatted as left-censored, e.g. "<.0001". Note: should only be used for fields with a lower limit of 0.
#' @import dplyr 
#' @import magrittr
#' @export

emj_round <- function(number, ll=NULL) {
  options(scipen=99)
  df     <- data.frame(number) %>%
    mutate(index=c(1:n()), isNA=is.na(number))
  if (is.null(ll)) {
    df.num <- filter(df, isNA==FALSE & number!=0) 
  } else {
    df.num   <- filter(df, isNA==FALSE & number!=0 & number >= ll) 
    df.ll    <- filter(df, isNA==FALSE & number!=0 & number < ll) %>%
                mutate(out = paste0("<", ll)) %>%
                dplyr::select(index, out)
  }
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
  df.zero <- filter(df, number==0) %>%
    mutate(out = "0") %>%
    dplyr::select(index, out)
  if (is.null(ll)) {
    df     <- rbind(df.na, df.num, df.zero) %>%
              merge(df, .)
  } else {
    df     <- rbind(df.na, df.num, df.zero, df.ll) %>%
              merge(df, .)
  }
  return(df$out)
}