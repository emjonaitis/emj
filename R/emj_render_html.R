#' Render R Markdown HTML output using emj standard directory conventions
#'
#' This function takes in a filename 
#' @param filename Name of the .Rmd file to knit (without extension).
#' @param path Location of the main project directory.
#' @import rmarkdown 
#' @export

emj_render_html <- function(filename, path) {
   rmd_file <- paste0(path, "/r/", filename, ".Rmd")
   html_file <- paste0(path, "/docs/", filename, "_", format(Sys.Date(), '%Y-%m-%d'), ".html")
   rmarkdown::render(rmd_file, output_file = html_file)
}