#' Is knitr in progress?
#'
#' @return TRUE if knitr is executing
#'
#' @author Yihui Xie and
#'   \href{https://stackoverflow.com/users/2706569/cl}{CL},
#' @references \url{https://stackoverflow.com/questions/33107908/how-to-tell-if-code-is-executed-within-a-knitr-rmarkdown-context}
#' @export

is.knitr.in.progress <- function() {
  isTRUE(getOption('knitr.in.progress'))
}

