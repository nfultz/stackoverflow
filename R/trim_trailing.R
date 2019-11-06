#' Strip leading / trailing zeros
#' 
#' Removes \code{value} from rightmost/leftmost elements of a vector.
#' 
#' @param x a vector
#' @param value a value to strip from x
#' 
#' @return a new vector, with values at the right removed
#' 
#' @examples 
#' trim_leading(c(0,0,0,0,1:5))
#' 
#' @export
#' @author \href{https://stackoverflow.com/users/986793/neal-fultz}{Neal Fultz}
#' @references \url{https://stackoverflow.com/questions/24009982/remove-zeros-in-the-start-and-end-of-a-vector/}

trim_trailing <- function(x, value=0) {
  w <- which.max(cumsum(x != value))
  x[seq.int(w)]
}

#' @export
#' @rdname trim_trailing
trim_leading <- function(x, value=0) {
  w <- which.max(cummax(x != value))
  x[seq.int(w, length(x))]
}