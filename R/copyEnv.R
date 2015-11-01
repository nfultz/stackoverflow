#' Copy objects from one environment to another
#' 
#' @param from source environment
#' @param to target environment
#' @param names names of objects to copy
#' 
#' @author Neal Fultz
#' @references \url{http://stackoverflow.com/a/33465113/986793}
#' @export
#' @examples
#' 
#' e1 <- list2env(list(a=1, b=2))
#' e2 <- new.env()
#' copyEnv(e1,e2)
#' ls(e2)

copyEnv <- function(from, to, names=ls(from, all.names=TRUE)) {
  mapply(assign, names, mget(names, from), list(to), 
         SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(NULL)
}
