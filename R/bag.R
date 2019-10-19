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
#' df2 <- structure(list(Dx1 = c("231", "231", "001", "245", "231", "001", 
#' "231", "001", "231", "001", "001", "245", "001", "231", "245", 
#' "245", "001", "231", "245", "001"), Dx2 = c("001", "001", "001", 
#' "001", "001", "001", "001", "234", "001", "234", "001", "001", 
#' "001", "001", "001", "777", "777", "234", "001", "234"), Dx3 = c("456", 
#' "001", "444", "444", "001", "001", "444", "001", "001", "001", 
#' "444", "001", "444", "456", "456", "444", "444", "456", "001", 
#' "456")), class = "data.frame", row.names = c(NA, -20L))
#'
#' Y <- 1:nrow(df2)
#' m <- lm(Y~bag(Dx1, Dx2, Dx3), df2)
#' summary(m)
#'
#' 
#' 
#' 
#' @importFrom stats setNames
#' @export
bag <- function(..., prefix=".", levels=NULL) {
  
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
  structure(+t(out), class='bag', levels=levels, prefix=prefix)
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