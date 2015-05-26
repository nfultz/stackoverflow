#' Sample rows from a dataframe or matrix
#' 
#' @param x a data frame or matrix
#' @param size
#' @param replace
#' @param prob  
#' 
#' @section Changes
#' Matched parameters to sample -- njf, May 18, 2015
#' 
#' @seealso \code{\link{sample}}
#' 
#' @export
#' @author \href{http://stackoverflow.com/users/211116/spacedman}{Spacedman}
#' @references \url{http://stackoverflow.com/questions/8273313/random-rows-in-dataframe-in-r}

randomRows = function(x, size, replace=FALSE, prob=NULL){
   x[sample(nrow(x),size, replace, prob),]
}