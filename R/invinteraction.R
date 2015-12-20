#' Split an interaction'ed factor back into seperate variables
#' 
#' Inverse of \code{interaction}
#' 
#' @param fac the factor to split
#' @param ... optional, names for variables
#' @param sep the seperator between levels
#' 
#' @return  a data.frame of factors 
#' 
#' @section Changes:
#' 
#' Refactored to process the levels vector, rather than entire factor vector. 
#' 
#' @author \href{http://stackoverflow.com/users/1855677/42}{42}, Neal Fultz
#' @references \url{http://stackoverflow.com/a/10521926/986793}
#' @seealso \code{\link[base]{interaction}}
#' @export
#' @examples
#' 
#' f1 <- gl(2, 3)
#' f2 <- gl(3, 2)
#' invinteraction(f1:f2, sep=':') 
#'
#' ppl <- interaction(
#'   eyes = as.factor(sample(colors(), 10)),
#'   hair = as.factor(sample(colors(), 10))
#'   )
#' str(invinteraction(ppl, "eyes", "hair"))
#' 

invinteraction <- function(fac, ..., sep='.') {
  
  stbl <- do.call(rbind.data.frame, strsplit(levels(fac), sep, TRUE))
  stbl[] <- lapply(stbl, as.factor)
  
  colnames(stbl) <- if(missing(...)) paste0('V', seq_along(stbl)) else  c(...)
  
  `rownames<-`(stbl[fac, , drop=FALSE], NULL)
}