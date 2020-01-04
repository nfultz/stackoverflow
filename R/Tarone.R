#' Tarone's Z Test
#' 
#' Tests the goodness of fit of the binomial distribution.
#' 
#' @param M Counts
#' @param N Trials
#' 
#' @return a \code{htest} object
#' 
#' @author \href{https://stats.stackexchange.com/users/173082/reinstate-monica}{Ben O'Neill}
#' @references \url{https://stats.stackexchange.com/a/410376/6378} and
#' R. E. TARONE, Testing the goodness of fit of the binomial distribution, Biometrika, Volume 66, Issue 3, December 1979, Pages 585–590, \url{https://doi.org/10.1093/biomet/66.3.585}
#' @importFrom stats pnorm
#' @export
#' @examples 
#'  #Generate example data
#' N <- c(30, 32, 40, 28, 29, 35, 30, 34, 31, 39)
#' M <- c( 9, 10, 22, 15,  8, 19, 16, 19, 15, 10)
#' Tarone.test(N, M)
Tarone.test <- function(N, M) {
  
  #Check validity of inputs
  if(!(all(N == as.integer(N)))) { stop("Error: Number of trials should be integers"); }
  if(min(N) < 1) { stop("Error: Number of trials should be positive"); }
  if(!(all(M == as.integer(M)))) { stop("Error: Count values should be integers"); }
  if(min(M) < 0) { stop("Error: Count values cannot be negative"); }
  if(any(M > N)) { stop("Error: Observed count value exceeds number of trials"); }
  
  #Set description of test and data
  method      <- "Tarone's Z test";
  data.name   <- paste0(deparse(substitute(M)), " successes from ", 
                        deparse(substitute(N)), " trials");
  
  #Set null and alternative hypotheses
  null.value  <- 0;
  attr(null.value, "names") <- "dispersion parameter";
  alternative <- "greater";
  
  #Calculate test statistics
  estimate    <- sum(M)/sum(N);
  attr(estimate, "names") <- "proportion parameter";
  S           <- ifelse(estimate == 1, sum(N),
                        sum((M - N*estimate)^2/(estimate*(1 - estimate))));
  statistic   <- (S - sum(N))/sqrt(2*sum(N*(N-1))); 
  attr(statistic, "names") <- "z";
  
  #Calculate p-value
  p.value     <- 2*pnorm(-abs(statistic), 0, 1);
  attr(p.value, "names") <- NULL;
  
  #Create htest object
  TEST        <- list(method = method, data.name = data.name,
                      null.value = null.value, alternative = alternative,
                      estimate = estimate, statistic = statistic, p.value = p.value);
  class(TEST) <- "htest";
  TEST; 
}