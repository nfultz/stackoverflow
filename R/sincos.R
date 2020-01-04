#' sin/cos pairs for modeling
#' 
#' Compute the sin and cos of x.
#' 
#' @param x a vector
#' @param period a scalar, which x is scaled by
#' 
#' @return a matrix containing a _sin and _cos column
#' 
#' @references \url{https://stackoverflow.com/questions/51874305/tuple-variable-in-r-regression-model/54393605#54393605}
#' @author \href{https://stackoverflow.com/users/986793/neal-fultz}{Neal Fultz}
#' 
#' @examples 
#' 
#' data(sunspots)
#' lm(sunspots~sincos(time(sunspots), 5/pi))
#' 
#' @export
sincos <- function(x, period=168/2/pi) {
  a <- array(x %o% (1/period), dim=c(length(x), 2, length(period)))
  
  a[,1,] <- sin(a[,1,])
  a[,2,] <- cos(a[,2,])
  
  dim(a) <- c(length(x), 2 *length(period))
  colnames(a) <- outer(c("_sin","_cos"), seq_along(period), paste, sep='.')
  
  structure(a, class="sincos", period=period)
}

#' @importFrom stats makepredictcall
#' @export
makepredictcall.sincos <- function(var, call){
  if (as.character(call)[1L] != "sincos")
    return(call)
  call = match.call(sincos, call)
  call[["period"]] <- attr(var, "period")
  call
}