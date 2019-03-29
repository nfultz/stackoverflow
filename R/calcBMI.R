#' Calculate Body Mass Index
#'
#' This calculates Body Mass Index
#'
#' @param w Weight (in pounds)
#' @param f Height (feet)
#' @param i Height (inches)
#' 
#' @return BMI
#' 
#'
#' @references \url{https://stackoverflow.com/questions/16782598/declaring-dynamic-variable-in-r/16782661#16782661}
#' @author \href{https://stackoverflow.com/users/190277/ben-bolker}{Ben Bolker}
#' 
#' @examples 
#' calcBMI(199, 5, 9)
#' @importFrom stats setNames
#' @seealso \code{\link{sprintf}}
#' @export
calcBMI <- function (w=204, f=6, i=1) {
  i <- f * 12 + i 
  703.06958 * w / i / i
}
