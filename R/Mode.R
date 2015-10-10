#' Calculate mode (most common element) of a vector
#' 
#' @param x a vector
#' @param ux vector of values x may take  
#' 
#' @section  Changes:
#' Factored \code{ux} into argument -- njf, May 18, 2015
#' 
#' @export
#' @author \href{http://stackoverflow.com/users/169947/ken-williams}{Ken Williams}
#' @references \url{http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode}

Mode <- function(x, ux=unique(x)) ux[which.max(tabulate(match(x, ux)))]