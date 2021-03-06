% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/built_in_distributions.R
\name{r_unif}
\alias{r_unif}
\title{Generate Uniformly Distributed Values}
\usage{
r_unif(min = 0, max = 1, ..., n = default_n(min, max), .seed = NULL)
}
\arguments{
\item{min, max}{vectors of lower and upper limits of the distribution}

\item{...}{Unused}

\item{n}{number of observations to generate. The \code{\link[=default_n]{default_n()}} function will
provide a default value within context}

\item{.seed}{One of the following:
\itemize{
\item NULL (default) will not change the current seed. This is the
usual case for generating random numbers.
\item A numeric value. This will be used to set the seed before generating
the random numbers. This seed will be stored with the results.
\item TRUE. A random seed value will be generated and set as the seed
before the results are generated. Again, this will be stored with
the results.
}

To extract the random seed from a previously generated set of
values, use \code{pull_seed()}}
}
\value{
A numeric vector of length \code{n}
}
\description{
Generates a set of Uniformly distributed values.
}
\examples{

set_n(5)

r_unif()

r_unif(1:5, 6:10)

r_unif(1:5, 10)

r_unif(n = 10)
}
