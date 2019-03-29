#' sprintf, with named references
#'
#' This converts named references in a format string (marked by curly braces), and passes through to \code{\link{sprintf}}.
#'
#' @param fmt a character vector of format strings, each of up to 8192 bytes.
#' @param ... values to be interpolated, optionally with names.
#' 
#' @return a character vector.
#' 
#'
#' @references \url{https://stackoverflow.com/questions/17475803/sprintf-format-strings-reference-by-name/55423080#55423080}
#' @author Neal Fultz
#' 
#' @examples 
#' sprintf_named("%{HIA}s!!! %{RYLAH}s", RYLAH="Rock You Like a Hurricane", HIA="Here I Am")
#' @importFrom stats setNames
#' @seealso \code{\link{sprintf}}
#' @export
sprintf_named <- function(fmt, ...) {
  args <- list(...)
  argn <- names(args)
  if(is.null(argn)) return(sprintf(fmt, ...))
  
  for(i in seq_along(args)) {
    if(argn[i] == "") next;
    fmt <- gsub(sprintf("%%{%s}", argn[i]), sprintf("%%%d$", i), fmt, fixed = TRUE)
  }
  
  do.call(sprintf, append(args, fmt, 0))
}
