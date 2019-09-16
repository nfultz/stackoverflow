#' Multi-indicators / "Bag o Words"
#'
#' This creates an indicator matrix from several columns.
#'
#' @param ... the columns to bag
#' @param prefix a prefix for the column names
#' @param levels levels shared among all columns
#' 
#' @return a n*p indicator matrix
#' 
#'
#' @references \url{https://stackoverflow.com/questions/47055856/search-multiple-columns-for-string-to-set-indicator-variable/57381877#57381877}
#' @author \href{https://stackoverflow.com/users/986793/neal-fultz}{Neal Fultz}
#' 
#' @examples 
#' 
#' @importFrom stats setNames
#' @export
bag <- function(..., prefix=".", levels=NULL, `NA`=NULL) {
  
  # Go from multiple columns to list of vectors
  bags <- mapply(c, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  # Find unique levels
  if(is.null(levels)) {
    levels <- sort(Reduce(union, bags))
    
    # names persist through outer
    names(levels) <- paste0(prefix, levels)
  }
  
  # Calculate out[level,bag] = level %in% bag 
  out <- outer(levels, bags, Vectorize(`%in%`))
  
  # Output a data structure
  structure(+t(out), class='bag', levels=levels)
}



#' @export
makepredictcall.bag <- function(var, call){
  # Stolen from splines package
  if (as.character(call)[1L] != "bag")
    return(call)
  args <- c("prefix", "levels")
  
  
  at <- attributes(var)[args]
  xxx <- call
  xxx[args] <- NULL
  xxx[names(at)] <- at
  xxx
}