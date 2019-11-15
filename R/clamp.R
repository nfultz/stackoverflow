#' Clamp a value into a range
#'
#' Splits paths into folders.
#'
#' @param x vector 
#' @param e1 the first edge
#' @param e2 the other edge, defaults to the negation of e1.
#' 
#' @return x, with values outside the boundaries replaced with the boundary points. 
#' 
#' @references \url{https://stackoverflow.com/questions/32599695/clamp-variable-within-range}
#' @author \href{https://stackoverflow.com/users/3093387/josliber}{josliber}, 
#' 
#' @examples 
#' 
#' clamp(-10:10, 2, -2)
#' clamp(-10:10, -2)
#' clamp(-10:10, 2)
#' 
#' @export
clamp <- function(x, e1, e2=-e1){
  e1 <- sort(c(e1,e2))
  pmin(pmax(x, e1[1]), e1[2])
}
