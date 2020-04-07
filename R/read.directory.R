#' Bulk import data files 
#' 
#' Read in each file at a path and then unnest them. Defaults to csv format.
#' 
#' @param path        a character vector of full path names
#' @param pattern     an optional \link[=regex]{regular expression}. Only file names which match the regular expression will be returned.
#' @param reader      a function that can read data from a file name.
#' @param ...         optional arguments to pass to the reader function (eg \code{stringsAsFactors}).
#' @param reducer     a function to unnest the individual data files. Use I to retain the nested structure. 
#' @param recursive 	logical. Should the listing recurse into directories?
#'  
#' @author Neal Fultz
#' @references \url{https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once}
#' 
#' @importFrom utils read.csv
#' @export
read.directory <- function(path='.', pattern=NULL, reader=read.csv, ..., 
                           reducer=function(dfs) do.call(rbind.data.frame, dfs), recursive=FALSE) {
  files <- list.files(path, pattern, full.names = TRUE, recursive = recursive)
  
  reducer(lapply(files, reader, ...))
}
