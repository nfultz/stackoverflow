#' Flatten a list without type coercion
#'
#' @param x a nested list
#'
#' @author Tommy, Joshua Ulrich, Josh O'Brien, Neal Fultz
#' @references \url{http://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion}
#' @export

flatten2 <- function(x) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x; 0 })
  y
}
