#' Check if package is available
#'
#' A predicate for whether a package is installed 
#'
#' @param pkg a character string with the name of a single package. An error occurs if more than one package name is given.
#' 
#' @return \code{TRUE} if a package is installed, and \code{FALSE} otherwise. 
#' 
#' @references \url{https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages/38082613#38082613}
#' @author \href{https://stackoverflow.com/users/1863950/artem-klevtsov}{Artem Klevtsov}
#' 
#' @examples 
#' 
#' is_inst("grDevices")
#' 
#' @export

is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}
