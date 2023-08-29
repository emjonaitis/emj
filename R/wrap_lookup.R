#' Look up identifiers in WRAP database
#'
#' This function takes in a participant ID and returns a data frame with all IDs and, optionally, visit dates.
#' @param wrap Vector containing WRAPNos.
#' @param reggie Vector containing Reggieids.
#' @param dbid Vector containing DBIDs.
#' @return Data frame containing cleaned UP data.
#' @seealso \code{\link{UP_harm}}
#' @keywords WRAP
#' @importFrom dplyr filter arrange
#' @importFrom odbc odbc dbConnect dbGetQuery
#' @export

wrap_lookup <- function(wrap=NULL, reggie=NULL, dbid=NULL, get.vis=FALSE) {
  if ((is.null(wrap) & is.null(reggie) & is.null(dbid)) |
      (is.null(wrap) + is.null(reggie) + is.null(dbid) < 2)) {
    stop("Must enter exactly one of: wrap, reggie, dbid")
  }
  # This is the filter we apply for a particular person.
  # In this case I started with DBID 1635, triangulated to WRAPNo, and then
  # Caite identified two siblings of potential interest.
  if (get.vis==TRUE) {
    df <- data.frame(odbc::dbGetQuery(odbc::dbConnect(odbc::odbc(), dsn="WRAP2007_Prod_DOM_64"), 
                           "SELECT e.Reggieid, e.WRAPNo, ts.DBID, ts.VisNo, 
                               ts.dtTesting, ts.dtBloodDraw, ts.dtLPDraw, ts.SchedStatus
                               FROM Everyone e INNER JOIN TestingSchedule ts 
                               ON e.DBID = ts.DBID"))
    if (!is.null(wrap)) {
      df <- dplyr::arrange(dplyr::filter(df, WRAPNo %in% wrap), WRAPNo, VisNo)
    } else if (!is.null(reggie)) {
      df <- dplyr::arrange(dplyr::filter(df, ReggieID %in% reggie), Reggieid, VisNo)
    } else if (!is.null(dbid)) {
      df <- dplyr::arrange(dplyr::filter(df, DBID %in% dbid), DBID, VisNo)
    }
  } else {
    df <- data.frame(odbc::dbGetQuery(odbc::dbConnect(odbc::odbc(), dsn="WRAP2007_Prod_DOM_64"), 
                           "SELECT Reggieid, WRAPNo, DBID FROM Everyone"))
    if (!is.null(wrap)) {
      df <- dplyr::arrange(dplyr::filter(df, WRAPNo %in% wrap), WRAPNo)
    } else if (!is.null(reggie)) {
      df <- dplyr::arrange(dplyr::filter(df, ReggieID %in% reggie), Reggieid)
    } else if (!is.null(dbid)) {
      df <- dplyr::arrange(dplyr::filter(df, DBID %in% dbid), DBID)
    }
  }
  return(df)
}