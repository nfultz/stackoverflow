#' Is this in Rstudio?
#'
#' Tests if inside RStudio.
#'
#' @return TRUE if running in RStudio child process, or Rstudio console specifically.
#'
#' @references \url{https://stackoverflow.com/a/17804414/986793}
#' @author \href{https://stackoverflow.com/users/1345455/coatless}{coatless}, \href{https://stackoverflow.com/users/946850/krlmlr}{krlmr}
#'
#' @examples
#'
#' is.rstudio() && is.rstudio.console()
#'
#' @export

is.rstudio = function() {
  Sys.getenv("RSTUDIO") == 1
}

is.rstudio.console = function(){
  .Platform$GUI == "RStudio"
}


