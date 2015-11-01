#' Log-Likelihood for k-means clustering (for calculating AIC and BIC)
#' 
#' @param object a \code{kmeans} object
#' @param ... unused
#' 
#' @author Neal Fultz, inspired by Sherry Towers and 
#'   \href{http://stackoverflow.com/users/2514568/andy-clifton}{Andy Clifton}, 
#' @references \url{http://stackoverflow.com/questions/15839774/how-to-calculate-bic-for-k-means-clustering-in-r}
#' @export
#' @seealso \code{\link[stats]{logLik}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}
#' @examples 
#' cl <- kmeans(iris[-5], 3)
#' AIC(cl)

logLik.kmeans <- function(object, ...) structure(
  object$tot.withinss,
  nobs = length(object$cluster),
  df = nrow(object$centers) * ncol(object$centers),
  class = 'logLik'
)
