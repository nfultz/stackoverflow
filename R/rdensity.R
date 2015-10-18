#' Distribution methods for density objects 
#' 
#' Density, distribution function, quantile function and random generation 
#' from a kernel density estimate (using linear approximation).
#' 
#' @param d a \code{density} object
#' 
#' @author \href{http://stackoverflow.com/users/295691/user295691}{user295691}, 
#'   Neal Fultz
#' @references \url{http://stackoverflow.com/questions/32871602/r-generate-data-from-a-probability-density-distribution}
#' 
#' @seealso \code{\link[stats]{density}}
#' @seealso \code{\link[stats]{approxfun}}
#' @seealso \code{\link[ks]{rkde}}
#' 
#' @importFrom stats approx runif
#' 
#' @examples 
#' x <- rnorm(100, mean=0:5)
#' d <- density(x)
#' r <- rdensity(10000, d)
#' plot(d)
#' lines(density(r), new=TRUE, col='blue', lty='dashed')

#' @param x a vector
#' @export
#' @rdname rdensity
ddensity <- function(x, d) approx(d$x, d$y, x, yleft=0, yright=0)$y

#' @param q a vector
#' @export
#' @rdname rdensity
pdensity <- function(q, d) approx(d$x, cdf(d$y), q, yleft=0, yright=1)$y

#' @param p a vector of probabilities
#' @export
#' @rdname rdensity
qdensity <- function(p, d) approx(cdf(d$y), d$x, p, yleft=-Inf, yright=Inf)$y

#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number of required
#' @export
#' @rdname rdensity
rdensity <- function(n, d) qdensity(runif(n), d)

  

cdf <- function(x)  {
  x <- cumsum(x)
  x / x[length(x)]
}