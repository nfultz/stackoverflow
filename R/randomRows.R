#' Sample rows from a dataframe or matrix
#' 
#' @param x a data frame or matrix
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the vector being sampled.
#' 
#' @section Changes:
#' Matched parameters to sample -- njf, May 18, 2015
#' 
#' @seealso \code{\link{sample}}
#' @seealso \code{\link[dplyr]{sample_n}} for dplyr users
#' 
#' @export
#' @author \href{http://stackoverflow.com/users/211116/spacedman}{Spacedman}
#' @references \url{http://stackoverflow.com/questions/8273313/random-rows-in-dataframe-in-r}

randomRows <- function(x, size, replace=FALSE, prob=NULL){
   x[sample(nrow(x), size, replace, prob),]
}