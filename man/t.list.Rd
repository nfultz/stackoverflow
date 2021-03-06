% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tlist.R
\name{t.list}
\alias{t.list}
\title{Transpose a list-of-lists}
\usage{
\method{t}{list}(x)
}
\arguments{
\item{x}{a list of lists}
}
\description{
For a nested list \code{x}, returns another nested list \code{y} such that
\code{x[[a]][[b]] == y[[b]][[a]]} for all indices in the original list.
}
\details{
Occasionally, sparse matrices are represented this way.
}
\references{
\url{https://stackoverflow.com/questions/45734380/transpose-nested-list}
}
\seealso{
\code{\link[purrr]{transpose}} and \code{\link[data.table]{transpose}}
}
\author{
\href{https://stackoverflow.com/users/2902647/zerweck}{zerweck},
  Neal Fultz
}
