#' Zip / Enumerate from python
#'
#' \code{zip}s together parallel lists into a list-of-lists. 
#' 
#' \code{enumerate} zips together a list with it's indices.
#'
#' @param ... Objects to be zipped together.
#' 
#' @return a list of lists
#' 
#' @references \url{https://stackoverflow.com/questions/9281323/zip-or-enumerate-in-r/57564884#57564884}
#' @author \href{https://stackoverflow.com/users/986793/neal-fultz}{Neal Fultz}
#' 
#' @examples 
#' zip(1:5,1:10)
#' @export
zip <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE)
}

#' @rdname zip
#' @examples 
#' enumerate(l=LETTERS)
enumerate <- function(...) {
  zip(ix=seq_along(..1), ...)
}