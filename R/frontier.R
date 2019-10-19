#df <- data.frame(v=c(.01, .012, .013, .014, .016), 
#                 r=c(.15,.12,.20,.21,.10))


frontier <- function(x, y, q=1) {
  a <- c(1,-1,-1,1)[q]
  b <- c(1,1,-1,-1)[q]
  
  x <- a*xtfrm(x)
  y <- b*xtfrm(y)
  
  i <- order(x, y, decreasing = TRUE)
  i <- i[y[i] == cummax(y[i])]
  
  ret <- logical(length(x))
  ret[i] <- TRUE
  ret
}
