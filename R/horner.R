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
#' @author \href{https://stackoverflow.com/users/9957245/torvin}{torvin}
#' @references \url{https://stackoverflow.com/questions/53256945/evaluate-polynominal-function}
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

