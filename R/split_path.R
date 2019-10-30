#' Split paths into folders
#'
#' Splits paths into folders.
#'
#' @param x character vector of file paths
#' 
#' @references \url{https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector/29232017#29232017}
#' @author \href{https://stackoverflow.com/users/269476/james}{James}, Neal Fultz for vectorized version
#' 
#' @examples 
#' 
#' split_path("~")
#' 
#' @export
split_path <- function(x) {
  dname <- dirname(x)
  bname <- basename(x)
  i <- !is.na(x) & (x == dname)
  bname[i] <- x[i]
  dname[i] <- NA
  if(all(is.na(bname))) NULL else cbind(bname, split_path(dname), deparse.level=0)
}
