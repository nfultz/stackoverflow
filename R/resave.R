#' Resave a session
#' 
#' @param ... symbols of objects
#' @param list a character vector of object names; unfortunately named
#' @param file the file to update
#' 
#' @author Neal Fultz and
#'   \href{http://stackoverflow.com/users/1201032/flodel}{flodel}, 
#' @references \url{http://stackoverflow.com/a/11813377/986793}
#' @export
#' @seealso \code{\link[base]{load}}, \code{\link[base]{save}}

resave <- function(..., list = character(), file) {
  e <- new.env()
  load(file, e)
  list <- union(list, as.character(substitute((...)))[-1L])
  copyEnv(parent.frame(), e, list)
  save(list = ls(e, all.names=TRUE), envir = e, file = file)
}

