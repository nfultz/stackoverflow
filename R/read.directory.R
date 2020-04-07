#' Bulk import data files 
#' 
#' Replaces NA elements of x with corresponding element of y, and NA elements of 
#' that with corresponding element from dots.
#' 
#' @section Changes:
#' 
#' Rather than using eagerly evaluating the dot arguments and Reducing over them,
#' instead we use recursion to evaluate them lazily. 
#' 
#' @param path        a character vector of full path names
#' @param pattern     an optional \link[=regex]{regular expression}. Only file names which match the regular expression will be returned.
#' @param reader      a function that can read data from a file name.
#' @param ...         optional arguments to pass to the reader function (eg \code{stringsAsFactors}).
#' @param reducer     a function to unnest the individual data files. Use I to retain the nested structure. 
#' @param recursive 	logical. Should the listing recurse into directories?
#'  
#' @author \href{https://stackoverflow.com/users/903061/gregor}{Gregor Thomas}, 
#' @references \url{https://stackoverflow.com/a/19254510/986793}
#' 
#' @importFrom utils read.csv
#' @export
read.directory <- function(path='.', pattern=NULL, reader=read.csv, ..., 
                           reducer=function(dfs) do.call(rbind.data.frame, dfs), recursive=FALSE) {
  files <- list.files(path, pattern, full.names = TRUE, recursive = recursive)
  
  reducer(lapply(files, reader, ...))
}
