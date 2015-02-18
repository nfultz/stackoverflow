#' Split a vector into n chunks
#' 
#' @param x a vector
#' @param n number of chunks
#' 
#' @author mathheadinclouds, Dis Shishkov
#' @references \url{http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r}
#' @export

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

