#' Reverse a scale
#'
#' Computes x = sz+c, which is the inverse of z = (x - c)/s 
#' provided by the \code{scale} function.
#' 
#' 
#' @param z a numeric matrix(like) object
#' 
#' @param center either NULL or a numeric vector of length 
#'                  equal to the number of columns of z  
#'                 
#' @param scale  either NULL or a numeric vector of length 
#'                  equal to the number of columns of z
#'
#' @seealso \code{\link{scale}}
#' 
#' @examples 
#'  mtcs <- scale(mtcars)
#'  
#'  all.equal(
#'    unscale(mtcs), 
#'    as.matrix(mtcars), 
#'    check.attributes=FALSE
#'  )
#'  
#'  oldSeed <- .Random.seed
#'  z <- unscale(rnorm(10), 2, .5)
#'  .Random.seed <- oldSeed
#'  x <- rnorm(10, 2, .5)
#'  all.equal(z, x, check.attributes=FALSE)
#' 
#' 
#' @author Neal Fultz
#' @references \url{https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting/46840073}
#'  
#' @export
unscale <- function(z, center = attr(z, "scaled:center"), scale = attr(z, "scaled:scale")) {
  z <- as.matrix(z)
  if (!is.null(scale))  z <- sweep(z, 2, scale, `*`)
  if (!is.null(center)) z <- sweep(z, 2, center, `+`)
  structure(z,
    "scaled:center"   = NULL,
    "scaled:scale"    = NULL,
    "unscaled:center" = center,
    "unscaled:scale"  = scale
  )
}

