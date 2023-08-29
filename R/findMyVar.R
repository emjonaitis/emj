#' findMyVar
#'
#' This function takes in a freeze name and a string and returns all table-field
#' pairs in the freeze which partly match the string.
#'  
#' @param freeze String containing name of ODBC object defining the freeze.
#' @param x String text to match using grepl. Search is case-insensitive.
#' @return Data frame containing potential matches.
#' @keywords WRAP
#' @importFrom odbc odbc dbConnect dbListTables dbListFields
#' @export


findMyVar <- function(freeze, x) {
  freeze <- odbc::dbConnect(odbc::odbc(), dsn=freeze)
  x_anycase <- tolower(x)
  
  alltables <- odbc::dbListTables(freeze)
  alltables <- alltables[22:90]
  alltables.df <- data.frame(thistable=character(),thisvarlist=character())
  
  for (t in 1:length(alltables)) {
    thistable <- alltables[t]
    thisvarlist <- odbc::dbListFields(freeze, paste0(thistable))
    thisvarlist_anycase <- tolower(thisvarlist)
    thistable.df <- data.frame(thistable, thisvarlist, thisvarlist_anycase)
    alltables.df <- rbind(alltables.df, thistable.df)
  }
  
  filter(alltables.df, grepl(x_anycase,thisvarlist_anycase)) %>%
    select(Table=thistable, Field=thisvarlist)
}