#' Load a CaNSample Object from File or URL
#'
#' Loads an `.RData` file containing a single `CaNSample` object. Ensures
#' the file contains only one object and returns it.
#'
#' @param path_or_url A file path or URL to an `.RData` file.
#'
#' @return The loaded CaNSample object, or `NULL` if invalid.
#' @keywords internal
load_CaNSample <- function(path_or_url) {
  e <- new.env()
  load(path_or_url, envir = e)
  objs <- ls(envir = e)
  
  if (length(objs) != 1) {
    shiny::showNotification("RData must contain exactly one object.", type = "error")
    return(NULL)
  }
  
  e[[objs[[1]]]]
}
