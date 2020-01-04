#' Zip / Enumerate from python
#'
#' \code{zip2}s together parallel lists into a list-of-lists. It is named zip2 to not collide with utils.
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
#' zip2(1:5,1:10)
#' @export
zip2 <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE)
}

#' @rdname zip2
#' @examples 
#' enumerate(l=LETTERS)
#' @export
enumerate <- function(...) {
  zip2(ix=seq_along(..1), ...)
}