#' wrapper for pkgload::load_all
#'
#' use this when writing new code rather than 'Build/Install and restart'. much faster
#' @param dir package directory
#' @export

load_pkg <- function(dir = "C:/Users/t/Documents/hydrusR"){

  pkgload::load_all(dir)

} #end function
