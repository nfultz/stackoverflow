#' Remove duplicated columns
#'
#' Drops duplicated columns from a data.frame (or other list-like object).
#'
#' @param df a data.frame
#' @return data.frame without duplicated columns
#' 
#' @references \url{https://stackoverflow.com/a/58475153/986793}
#' @author \href{https://stackoverflow.com/users/3732271/akrun}{akrun}
#' 
#' @examples 
#' 
#' df <- data.frame(a=1:10, b=1:10, c=2:11)
#' 
#' unique_columns(df)
#' 
#' @export


unique_columns <- function(df) df[!duplicated(as.list(df))]
