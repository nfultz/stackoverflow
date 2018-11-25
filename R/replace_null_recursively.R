#' Replace NULLs in nested lists
#' 
#' 
#' @param x a nested list
#' @param what a value
#' @return x with NULLs replaced with what
#' 
#' @author \href{https://stackoverflow.com/users/6621998/shayaa}{shayaa}, 
#' @references \url{https://stackoverflow.com/a/38950427/986793}
#' 
#' @export
replace_null_recursively <- function(x, what=NA_character_) {
  lapply(x, function(x) {
    if (is.list(x)) {
      replace_null_recursively(x)
    } else if (is.null(x)) { 
      what 
    } 
    else {
      x
    }
  })
}

