#' Efficient binary search for character vectors
#' 
#' @param val  values
#' @param tab  table to find values in
#' 
#' @author Martin Morgan, Neal Fultz
#' @references \url{http://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search/} and
#'             \url{https://stat.ethz.ch/pipermail/r-help/2011-April/274182.html}
#' @export
#' @examples
#'  bsearch7(sample(letters, 5000, replace=TRUE, letters)
bsearch7 <-
     function(val, tab, L=1L, H=length(tab))
{
     b <- cbind(L=rep(L, length(val)), H=rep(H, length(val)))
     i0 <- seq_along(val)
     repeat {
         updt <- M <- b[i0,"L"] + (b[i0,"H"] - b[i0,"L"]) %/% 2L
         tabM <- tab[M]
         val0 <- val[i0]
         i <- tabM < val0
         updt[i] <- M[i] + 1L
         i <- tabM > val0
         updt[i] <- M[i] - 1L
         b[i0 + i * length(val)] <- updt
         i0 <- which(b[i0, "H"] >= b[i0, "L"])
         if (!length(i0)) break;
     }
     b[,"L"] - 1L
}

