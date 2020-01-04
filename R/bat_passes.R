#' Bat passes
#'
#' Data from a study on the effect of light on bats.
#'
#' @docType data
#'
#' @usage data(bat_passes)
#'
#' @format A data.frame with 80 observations and 5 variables.
#' 
#' \describe{
#'   \item{Location}{Five locations in the study}
#'   \item{Al.N}{Dark or Light condition}
#'   \item{Buzzes}{Count of buzzes per day}
#'   \item{Passes}{Count of passes per day}
#'   \item{Date}{Date of observation}
#' }
#'
#' @keywords datasets
#'
#' @references nausicaa (\url{https://stats.stackexchange.com/users/190274/nausicaa}), 
#' poisson glm to observe whether effects of artificial light on the number of bat passes in each location were significant, 
#' URL (version: 2018-03-09): \url{https://stats.stackexchange.com/q/325334}

#'
#' @source \url{https://stats.stackexchange.com/q/325334}
#'
#' @examples
#' data(bat_passes)
#' head(bat_passes)
"bat_passes"