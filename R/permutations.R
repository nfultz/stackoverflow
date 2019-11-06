#' Generate all distinct permutations of a vector
#' 
#' 
#' @param x vector to permute
#' 
#' @return A matrix of all distinct permutations (by row)
#' 
#' @author \href{https://stackoverflow.com/users/827280/museful}{Museful}
#' @references \url{https://stackoverflow.com/a/20199902/986793}
#' @export
#' @examples 
#'   permutations(LETTERS[1:4])
permutations <- function(x){
  pi <- permutations_impl(length(x))
  x <- x[pi]
  dim(x) <- dim(pi)
  x
}


permutations_impl <- function(n) {
  if(n==1) {
    return(matrix(1))
  } 
  sp <- Recall(n-1)
  p <- nrow(sp)
  A <- matrix(nrow=n*p,ncol=n)
  for(i in 1:n){
    A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
  }
  A
  
}
