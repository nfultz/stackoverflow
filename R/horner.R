#' Evaluate Polynomial and Rational Functions using Horner's method
#' 
#' Calculate 
#' 
#' \deqn{y = (P_1 + P_2*x + P_3*x^2 + ... ) / ( Q_1 + Q_2*x + Q_3*x^2 + ...)}
#' 
#' If the coefficients have zeros as highest powers, those are ignored.
#' 
#' @param x a vector
#' @param P the coefficients of the polynomial in the numerator, in increasing order
#' @param Q the coefficients of the polynomial in the denominator
#' 
#' @return a vector
#' 
#' @examples
#' 
#' P <- c(1,-2,1)
#' horner.poly(polyroot(P), P)
#' 
#' @export
#' TODO!!!
#' @author \href{http://stats.stackexchange.com/users/9394/zen}{Zen}, Neal Fultz
#' @references \url{http://stats.stackexchange.com/questions/57262/implementation-of-dirichlet-cdf}
horner.poly <- function(x, P) {
  
  z <- 0

  P <- trim_trailing(P, 0)
  
  for(p in rev(P)) {
    z <- z*x + p
  }
  
  z
}

#' @export
#' @rdname horner.poly
horner.rational <- function(x, P, Q) horner.poly(x, P) / horner.poly(x, Q)

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

