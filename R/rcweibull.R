#' Sample from conditional Weibull
#' 
#' The conditional weibull distribution is a truncated version  - the condition is that the observation has already survived
#'  
#' @param n  Number of samples to draw
#' @param lambda Weibull parameter
#' @param kappa Weibull parameter
#' @param T0 Left boundary
#' 
#' @author \href{https://stats.stackexchange.com/users/283201/jcken}{jcken}, 
#'   Neal Fultz
#' @references \url{hhttps://stats.stackexchange.com/questions/470058/how-to-draw-from-conditional-weibull-distribution/470064#470064}
#' 
#' @seealso \code{\link[stats]{rweibull}}
#' @importFrom stats rexp
#' 
#' @examples 
#' 
#' rcweibull(n=20, lambda=1:2, kappa=1:5, T0=1:20) 
#' 
#' @export
rcweibull <- function(n, lambda, kappa, T0){
  p <- rexp(n)
  
  ( T0^kappa + lambda^kappa * p)^(1/kappa) 
  
}

