#' List all methods for an object
#' 
#' The built-in methods() function will give all available methods for a specified class,
#' or for a specified generic function, but not for an object. Objects can have multiple 
#' classes, so this can be complicated to calculate.
#' 
#' @param cl a vector of class names, or an object
#' 
#' @examples
#' g <- glm(y~x,data=data.frame(x=1:10,y=1:10))
#' classMethods(g)
####
#' 
#' @importFrom  utils methods
#' @export
#' @author \href{http://stackoverflow.com/users/2372064/mrflick}{MrFlick}
#' @references \url{http://stackoverflow.com/questions/23840404/function-to-return-all-s3-methods-applicable-to-an-object}
classMethods <- function(cl) {
  if(!is.character(cl)) {
    cl<-class(cl)
  }
  ml<-lapply(cl, function(x) {
    sname <- gsub("([.[])", "\\\\\\1", paste0(".", x, "$"))
    m <- methods(class=x)
    data.frame(
      m=as.vector(m), 
      c=x, n=sub(sname, "", as.vector(m)),
      attr(m,"info"),
      stringsAsFactors=F
    )
  })
  df<-do.call(rbind, ml)
  df<-df[!duplicated(df$n),]
  structure(df$m, 
            byclass=FALSE,
            info=data.frame(visible=df$visible, from=df$from, row.names = df$m), 
            class="MethodsFunction")
}