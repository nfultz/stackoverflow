#' Approximate AUC
#' 
#' AUC can be computed exactly by sorting the fitted values, which is often
#' computationally slow. Instead, we can approximate the AUC numerically using
#' monte carlo.
#' 
#' @param y the actual class labels [0-1]
#' @param yhat the predicted probabilities
#' @param n number of samples to draw
#' 
#' @examples
#' g <- glm(y~x,data=data.frame(x=1:10,y=1:10))
#' classMethods(g)
####
#' 
#' @export
#' @author \href{http://stackoverflow.com/users/227734/erik}{erik}, Neal Fultz
#' @references \url{http://stackoverflow.com/questions/4903092/calculate-auc-in-r}
approxAUC <- function(y, yhat, n=1000) {
  pos <- sample(yhat, n, replace=TRUE, prob=y)
  neg <- sample(yhat, n, replace=TRUE, prob=1-y)
  mean(pos > neg)
}