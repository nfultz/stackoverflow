#' Convert indices to logical vector
#' 
#' Gives a logical vector which is TRUE for the indices provided
#' 
#' @param ix an vector of indices
#' @param n the length of the output vector; defaults to the maximum index
#' @param nm (optional) names for the vector 
#' 
#' @return  a logical vector of length \code{n} and names \code{nm}
#' 
#' If \code{nm} is specified, \code{ix} may be a character vector instead.
#' 
#' @section Changes:
#' 
#' Rather than using a \code{useNames} logical to copy the names attribute from 
#' one vector to another, you may specify names via the \code{nm} argument.
#'
#' @author \href{http://stackoverflow.com/users/709529/nick-sabbe}{Nick Sabbe}, Neal Fultz
#' @references \url{http://stackoverflow.com/a/7661128/986793}
#' @seealso \code{\link[base]{interaction}}
#' @export
#' @examples
#' 
#' x <- rnorm(50) > 1
#' ix <- which(x)
#' all.equal(x, invwhich(ix, 50))
#' 
#' all.equal(
#'   invwhich(grep('O', state.abb), 50),
#'   grepl('O', state.abb)
#' )

invwhich <- function(ix, n=max(if(is.numeric(ix))ix, length(nm)), nm) {
  i <- logical(n)
  if(!missing(nm)) names(i) <- nm
  i[ix]<-TRUE
  i
}

