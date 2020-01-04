#' Handle Missing Values with Fill + Dummy
#'
#' Handles missing values by filling in with mean, and adding a dummy variable.
#'
#' @param object an R object, typically a data.frame
#' @param ... other arguments (not used)
#' 
#' @references \url{https://stackoverflow.com/questions/54642599/impute-constant-and-create-missingness-dummy/54757973#54757973}
#' @author \href{https://stackoverflow.com/users/986793/neal-fultz}{Neal Fultz}
#' 
#' @examples 
#' 
#' df <- structure(list(Y = c(3.83, 22.73, 13.85, 14.09, 20.55, 18.51, 
#' 17.76, 9.42, 15.88, 27.81), X1 = 1:10, X2 = c(2L, NA, NA, 4L, 
#' 8L, 7L, 6L, 1L, 3L, 9L)), .Names = c("Y", "X1", "X2"), row.names = c(NA, 
#' -10L), class = "data.frame")
#' 
#' (m <- lm(Y~X1+X2, df, na.action = na.dummy))
#' m2 <- fix_predvars(m)
#' attr(terms(m2), "predvars")
#' predict(m2, newdata = data.frame(X1=2,X2=NA_real_))
#' 
#' @export
na.dummy <- function(object, ...) {
  UseMethod("na.dummy", object)
}

#' @export
na.dummy.numeric <- function(object, ..., m=mean(object, na.rm=TRUE)) {
  i <- is.na(object)
  
  structure(cbind(replace(object, i, m), `NA`=+i), 
            class='na.dummy', m=m)
}

#' @export
na.dummy.data.frame <- function(object, ...) {
  
  w <- vapply(object, anyNA, TRUE)
  cm <- rep(NA, length(object))

  for(j in which(w)) {
    object[[j]] <- na.dummy(object[[j]])
    cm[j] <- attr(object[[j]], 'm')
  }

  structure(object, na.action=structure(cm, class='dummy'))
}

#' @importFrom stats terms na.action
#' @export
#' @rdname na.dummy
fix_predvars <- function(object){
  
  
  pv <- attr(terms(object), "predvars")
  
  cm <- na.action(object)
  
  
  for(j in seq_along(cm)) {
    if(is.na(cm[j])) next
    
    newpv <- quote(na.dummy())
    newpv[[2]] <- pv[[j+1]]
    newpv[["m"]] <- cm[j]
    pv[[j+1]] <- newpv
    
  }
  attr(object$terms, 'predvars') <- pv
  
  object
}

#' @importFrom stats makepredictcall
#' @export
makepredictcall.na.dummy <- function(var, call){
  if (as.character(call)[1L] != "na.dummy")
    return(call)
  call["m"] <- attr(var, "m")
  call
}
