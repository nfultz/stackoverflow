#' Wait for a keypress
#' 
#' @author nnn, arulmr
#' @references \url{http://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r}
#' @export
readkey <- function() invisible(readline(prompt="Press [enter] to continue"))