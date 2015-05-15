#' Find duplicates in a vector
#' 
#' This will find all duplicates in a run, unlike \code{\link{duplicated}} which
#' finds duplicates globally.
#' 
#' @param x a vector
#' 
#' @export
#' @author \href{http://stackoverflow.com/users/980833/josh-obrien}{Josh O'Brien}, Neal Fultz 
#' @references \url{http://stackoverflow.com/questions/30260507/exclude-subsequent-duplicated-rows-in-r}
#' 
#' @examples
#' duplicated2(c(2,3,3,2,2,3,3,3,3,2,2))

duplicated2 <- function(x) {
  sequence(rle(x)$lengths) > 1
}