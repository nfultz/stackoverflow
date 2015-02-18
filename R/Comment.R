#' Multi-line Comments
#' 
#' @param ... comment, not evaluated.
#' 
#' @examples
#' Comment( `
#' 
#' # Put anything in here except back-ticks.
#'
#' api_idea <- function() {
#'   return TRUE
#' }
#' 
#' # Just to show api_idea isn't really there...
#' print( api_idea )
#' 
#' `)
####
#' 
#' @export
#' @author thell, Neal Fultz
#' @references \url{http://stackoverflow.com/questions/1231195/multiline-comment-workarounds}
Comment <- function(...) {invisible()}