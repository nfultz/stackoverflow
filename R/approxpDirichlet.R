#' Approximate CDF of Dirichlet
#' 
#' A monte-carlo approximation of the Dirichlet CDF.
#' 
#' @param a Dirichlet parameters
#' @param t the proportions
#' @param N number of samples to draw
#' 
#' @examples
#' approxpDirichlet(c(1,3,1), c(0.299, 0.528, 0.204))
#' 
#' @importFrom stats rgamma
#' @export
#' @author \href{http://stats.stackexchange.com/users/9394/zen}{Zen}, Neal Fultz
#' @references \url{http://stats.stackexchange.com/questions/57262/implementation-of-dirichlet-cdf}

approxpDirichlet <- function(a, t, N=10000) {
  if(sum(t) <= 1) return(0)
  
  X <- rgamma(length(a)*N, a, 1)
  dim(X) <- c(length(a), N)
  X <- X <= tcrossprod(t, colSums(X))
  
  sum(apply(X, 2, all)) / N
}
