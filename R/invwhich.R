#' Convert indices to logical vector
#' 
#' Gives a logical vector which is TRUE for the indices provided
#' 
#' @param ix an integer vector of indices
#' @param n the length of the output vector; defaults to the maximum index
#' @param nm (optional) names for the vector 
#' 
#' @return  a logical vector of length \code{n} and names \code{nm}
#' 
#' @author \href{http://stackoverflow.com/users/709529/nick-sabbe}{Nick Sabbe}, Neal Fultz
#' @references \url{http://stackoverflow.com/a/7661128/986793}
#' @seealso \code{\link[base]{interaction}}
#' @export
#' @examples
#' 
#' x <- rnorm(50) > 1
#' ix <- which(x)
#' all.equal(x, invwhich(ix, length(x)))

invwhich <- function(ix, n=max(if(is.numeric(ix))ix, length(nm)), nm) {
  i <- logical(n)
  if(!missing(nm)) names(i) <- nm
  i[ix]<-TRUE
  i
}

