#' Reflect upper/lower triangle across diagonal
#' 
#' Create a new matrix by copying the lower(upper) triangle to the other half.
#' 
#' @param m a square matrix
#' @param from lower or upper triangle
#' 
#' @return a symmetric square matrix
#' 
#' @author \href{https://stackoverflow.com/users/980833/josh-obrien}{Josh O'Brien}
#' @references \url{https://stackoverflow.com/questions/26166569/copy-upper-triangle-to-lower-triangle-for-several-matrices-in-a-list}
#' @export
#' @examples 
#'   x <- matrix(1:9,3,3)
#'   reflect_triangle(x, "lower")
#'   reflect_triangle(x, "upper")
reflect_triangle <- function(m, from=c("lower", "upper")) {
  ix <- switch(match.arg(from), lower=upper.tri, upper=lower.tri)(m, diag=FALSE)
  m[ix] <- t(m)[ix]
  m
}
