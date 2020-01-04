#' Replace NAs in parallel vectors
#' 
#' Replaces NA elements of x with corresponding element of y, and NA elements of 
#' that with corresponding element from dots.
#' 
#' @section Changes:
#' 
#' Rather than using eagerly evaluating the dot arguments and Reducing over them,
#' instead we use recursion to evaluate them lazily. 
#' 
#' @param x a vector
#' @param y replacement values
#' @param ... further replacement values
#' @return x with NAs replaced with y
#' 
#' @author \href{https://stackoverflow.com/users/903061/gregor}{Gregor Thomas}, 
#' @references \url{https://stackoverflow.com/a/19254510/986793}
#' 
#' @examples 
#' 
#' x <- c(1:4, NA, 1:4, NA)
#' y <- c(1:9, NA)
#' z <- c(NA, NA, 1:8)
#' coalesce(x,y,z) 
#' 
#' @export
coalesce <- function(x,y,...) {
  i <- which(is.na(x))
  x[i] <- y[i]
  if(...length() && length(i)) Recall(x, ...) else x
}
