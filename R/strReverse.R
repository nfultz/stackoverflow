#' Reverse each string of a vector
#' 
#' A function which will reverse every string in a vector of strings.
#' 
#' @param x a character vector
#' 
#' @export
#' @author Josh O'Brien
#' @references \url{https://stackoverflow.com/questions/13612967/how-to-reverse-a-string-in-r}
#' 
#' @examples
#' strReverse(c("abc", "Statistics"))

strReverse <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}