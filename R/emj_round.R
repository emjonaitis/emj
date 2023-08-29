#' Round nicely
#'
#' This function takes in a number or vector of numbers and rounds it to two decimal 
#' places for large numbers, or two significant figures for small ones.
#' It returns a string or vector of strings.
#' @param number Number to round.
#' @import dplyr 
#' @import magrittr
#' @export

emj_round <- function(number) {
  options(scipen=99)
  df     <- data.frame(number) %>%
    mutate(index=c(1:n()), isNA=is.na(number))
  df.num <- filter(df, isNA==FALSE & number!=0) 
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
  df     <- rbind(df.na, df.num, df.zero) %>%
    merge(df, .)
  return(df$out)
}