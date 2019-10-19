#df <- data.frame(v=c(.01, .012, .013, .014, .016), 
#                 r=c(.15,.12,.20,.21,.10))


frontier <- function(x, y) {
  i <- order(x, y, decreasing = TRUE)
  i <- i[y[i] == cummax(y[i])]
  
  ret <- logical(length(x))
  ret[i] <- TRUE
  ret
}
