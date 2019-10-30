#' Split paths into folders
#'
#' Splits paths into folders.
#'
#' @param x character vector of file paths
#' 
#' @references \url{https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector/29232017#29232017}
#' @author \href{https://stackoverflow.com/users/269476/james}{James}
#' 
#' @examples 
#' 
#' split_path("~")
#' 
#' @export
split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
