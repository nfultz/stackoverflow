#' Find efficient frontier
#'
#' A predicate that is TRUE if a point is on the efficient frontier.
#'
#' @param ... coordinates to scan
#' @return logical vector, TRUE if point is on efficient frontier
#' 
#' @references \url{https://stackoverflow.com/a/36209989/986793}
#' @author \href{https://stackoverflow.com/users/986793/neal-fultz}{Neal Fultz}
#' 
#' @examples 
#' 
#' df <- data.frame(x=rnorm(100), y=rnorm(100))
#' plot(df)
#' points(subset(df, frontier(x,y)), col='red', pch=15)
#' points(subset(df, frontier(-x,y)), col='green', pch=15)
#' points(subset(df, frontier(x,-y)), col='blue', pch=15)
#' points(subset(df, frontier(-x,-y)), col='orange', pch=15)
#'  
#' @export




frontier <- function(...) {
  X <- list(...)

  i <- order(..., decreasing = TRUE)
  ret <- logical(length(i))
  
  for(z in X[-1]){
    i <- i[z[i] == cummax(z[i])]
  }
  
  
  ret[i] <- TRUE
  ret
}
