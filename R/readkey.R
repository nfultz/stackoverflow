#' Wait for a keypress
#' 
#' @section Changed Feb 23, 2015 by njf:
#' \code{prompt} may be set by a parameter rather than hard coding it.
#' 
#' 
#' @param prompt the text to display
#' 
#' @author nnn, arulmr, Neal Fultz
#' @references \url{http://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r}
#' @export
readkey <- function(prompt="Press [enter] to continue") invisible(readline(prompt))