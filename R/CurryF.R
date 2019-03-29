#' Curry a function
#' 
#' Partially applies a function
#' 
#' @param FUN a function
#' @param ... arguments to apply
#' 
#' @return a partially applied function
#' 
#' @author \href{https://stackoverflow.com/users/1756702/a-webb}{A Webb}
#' @references 
#'   \url{https://stackoverflow.com/a/31900149/986793}
#' 
#' @export
#' @examples 
#'  x <- 1:2
#'  appender <- CurryF(append, x)
#'  x <- 5:6
#'  append <- sum
#'  appender(6)
CurryF <- function(FUN,...) {
  force(FUN); #needed for nesting Curry
  dots <- list(...);
  function(...) do.call(FUN,c(dots,list(...)))
}