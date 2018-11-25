#' Transpose a list-of-lists
#' 
#' For a nested list \code{x}, returns another nested list \code{y} such that
#' \code{x[[a]][[b]] == y[[b]][[a]]} for all indices in the original list.
#' 
#' Occasionally, sparse matrices are represented this way. 
#' 
#' @seealso \code{\link[purrr]{transpose}} and \code{\link[data.table]{transpose}}
#' 
#' @param x a list of lists
#' @export
#' @author \href{https://stackoverflow.com/users/2902647/zerweck}{zerweck},
#'   Neal Fultz
#' @references \url{https://stackoverflow.com/questions/45734380/transpose-nested-list}
t.list <- function(x){
  x <- do.call(rbind, x)
  lapply(setNames(seq(ncol(x)), colnames(x)), function(j) x[,j])
}
