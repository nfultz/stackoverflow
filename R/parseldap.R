#' Parse LDAP output into dataframe
#'
#' @param ldapraw A length-one character vector containing the raw LDAP output
#' @return a data.frame with one row per person
#'
#' @author \href{https://stackoverflow.com/users/3792484/user3792484}{user3792484},
#'   rewrite by Neal Fultz
#' @references \url{https://stackoverflow.com/questions/22793855/how-do-i-run-a-ldap-query-using-r}

#' @export
parseLDAP<-function(ldapraw)
{
  # seperate by two new lines
  lines <- readLines(textConnection(ldapraw))
  i <- grepl("^DN: ", lines)
  recs <- cumsum(i)
  i <- i | nchar(lines) == 0
  recs <- split(lines[!i], recs[!i])

  recs <- sapply(recs, function(x){
    m <- regmatches(x, regexec("^\t([^:]*): (.*)$", x))
    k <- as.character(lapply(m, `[`, 2))
    v <- as.character(lapply(m, `[`, 3))
    list2env(lapply(split(v, k), paste, collapse=" "))
  })
  
  all_cols <- Reduce(union, lapply(recs, names))
  
  recs <- lapply(recs, mget, x=all_cols, ifnotfound=NA_character_)

  do.call(rbind.data.frame, c(recs, stringsAsFactors = FALSE, row.names = NULL))
}