#' Substitute on an expression in a value
#'
#' If expr's value is an expression, substitute in any variables bound in \code{env}.
#' 
#' Differs in that substitute uses expr's expression and not value.
#'
#' @param expr an expression value
#' @param  env an environment or a list object.
#'
#' @author \href{https://stackoverflow.com/users/516548/g-grothendieck}{G. Grothendieck}
#' @references \url{https://stackoverflow.com/questions/47780150/use-variable-in-r-substitute/986793}
#' @seealso \link{substitute}
#' @examples 
#' a <- expression(z = y + x + 2)
#' substituteExpr(a, list(x=4))
#' @export
substituteExpr <- function(expr, env) {
  do.call(substitute, list(expr=expr[[1]], env=env))
}
