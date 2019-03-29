#' Recursivly split a data.frame
#'
#' When there are multiple factors to split by, Base R split returns a 
#' flattened structure by splitting on the interaction of all factors. 
#' rsplit instead returns a nested list-of-lists.
#'
#' @param x  a data.frame or vector
#' @param by a data.frame of factors
#' @param drop drop unused factor levels
#' 
#' @return a nested list of dataframes, split by each element of \code{by}
#' 
#' 
#' Inspired by, but different from the below
#'
#' @references \url{https://stackoverflow.com/questions/47802545/converting-data-frame-into-deeply-nested-list/47802935#47802935}
#' @author Neal Fultz
#'
#' @importFrom stats setNames
#' @export
rsplit <- function(x, by, drop=FALSE){
  if(is.atomic(by))  return(split(x,by,drop=drop))
  if(length(by) == 1) return(split(x,by[[1]],drop=drop))
  mapply(rsplit,
         x=split(x, by[[1]], drop=drop),
         by=t(lapply(by[, -1, drop=FALSE], split, by[[1]], drop=drop)),
         drop=drop, SIMPLIFY = FALSE)
}
