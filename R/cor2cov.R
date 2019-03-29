#' Back transform correlation matrix to variance-covariance matrix 
#' 
#' Compute a variance-covariance matrix from a correlation matrix and standard deviations.
#' 
#' @param V a variance covariance matrix
#' @param sd a vector of standard deviations - if ommitted, use the sqrt of the diagonal of V
#' 
#' @return a variance-covariance matrix
#' 
#' @author \href{https://stackoverflow.com/users/767760/s4m}{S4M}, 
#' @references \url{https://stackoverflow.com/questions/18740796/generate-covariance-matrix-from-correlation-matrix}
#' @export
#' @seealso \code{\link[stats]{cor}}
#' @examples 
#' stopifnot(all.equal(
#'   cor2cov(cor(mtcars), sapply(mtcars, sd)), 
#'   cov(mtcars)
#' ))
cor2cov <- function(V, sd=sqrt(diag(V))) {
  stopifnot(is.matrix(V))
  V * tcrossprod(sd)
}