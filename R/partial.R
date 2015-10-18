#' Partially apply a function
#' 
#' Simplify a function by setting some arguments to pre-specified values
#' 
#' @param f a function
#' @param ... arguments to capture
#' 
#' @author \href{http://stackoverflow.com/users/3093387/josilber}{John Silberholz}, 
#' @references \url{http://stackoverflow.com/questions/32173901/how-to-efficiently-partially-apply-a-function-in-r}
#' 
#' @seealso \code{\link[pryr]{partial}}
#' @seealso \code{\link[functional]{Curry}}
#' 
#' @examples 
#' # Example 1:
#' f <- function(a, b, c, d) a+b+c+d
#' p <- partial(f, a=2, c=3)
#' p(b=0, d=1)
#' 
#' # captures a format string for printing out sleep data
#' labeller <- partial(sprintf, fmt="extra=%3.2f, group=%d, ID=%d")
#' do.call(labeller, sleep[1, , drop=FALSE])
#' 
#' @export
partial <- function(f, ...) {
  l <- list(...)
  function(...) {
    do.call(f, c(l, list(...)))
  }
}

