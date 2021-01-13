% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{match.call2}
\alias{match.call2}
\title{Extension of `match.call()`}
\usage{
match.call2(n = 0, ...)
}
\arguments{
\item{n}{How far up the call-stack they would like to extract. The default,
`n=0` produces the same result as `match.call()` so this can be
inserted wherever `match.call()` is used.}
}
\description{
Extends the built-in function `match.call()` by allowing the user
to specify how far up the call stack they would like to extract.
}
\examples{
f <- function(n){
  g(n)
}

g <- function(n){
  h(n)
}

h <- function(n){
  match.call2(n)
}

f(0)
f(1)
f(2)


}