

#' @name r_bern
#'
#' @title Generate Bernoulli Distributed Values
#'
#' @description
#' Generates a set of Bernoulli distributed values.
#'
#' @param prob
#' vector of probability of successes, between 0 & 1
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_bern(0.9)
#'
#' r_bern(seq(0,1,0.1))
#'
#' r_bern(1/4,n=10)
#'
#'
#' @export
r_bern <- function(prob=0.5,...,n=default_n(prob),.seed=NULL){
  check_n(n)
  check_must_be_between(prob,0,1)

  if(!is.null(.seed)) set.seed(.seed)
  stats::rbinom(n=n,size=1,prob=prob)
}


#' @name r_lgl
#'
#' @title Generate Logical Values
#'
#' @description
#' Generates a set of Logical values.
#'
#' @param prob
#' vector of probability of `TRUE` results, between 0 & 1
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_lgl(0.9)
#'
#' r_lgl(seq(0,1,0.1))
#'
#' r_lgl(1/4,n=10)
#'
#'
#' @export
r_lgl <- function(prob=0.5,...,n=default_n(prob),.seed=NULL){
  check_n(n)
  check_must_be_between(prob,0,1)

  with_seed(
    .seed,
    stats::rbinom(n=n,size=1,prob=prob) == 1
  )
}



#' @name r_letters
#'
#' @title Generate Random Letters
#'
#' @description
#' Generates a set of Random Letters.
#'
#' @param nchar
#' vector of number of characters to return, positive integer
#'
#' @inheritParams r_norm
#'
#'
NULL


#' @describeIn r_letters Uses only lower-case letters
#'
#' @examples
#'
#' set_n(5)
#'
#' r_letters(3)
#'
#' r_letters(1:10)
#'
#' r_letters(3,n=10)
#'
#' @export
r_letters <- function(nchar=1,...,n=default_n(nchar),.seed=NULL){
  check_n(n)
  check_must_be_integer(nchar)
  check_must_be_positive(nchar)

  if(length(nchar) != 1 && length(nchar) != n)
    error_glue("length of nchar is incompatible with context")

  if(length(nchar) != n){
    nchar <- rep(nchar,n)
  }

  with_seed(
    .seed,
    vapply(
      lapply(nchar,sample,x=base::letters,replace=T),
      paste0,
      collapse="",
      character(1))
  )
}

#' @describeIn r_letters Uses only upper-case letters
#'
#' @examples
#'
#' r_LETTERS(3)
#'
#' r_LETTERS(1:10)
#'
#' r_LETTERS(3,n=10)
#'
#' @export
r_LETTERS <- function(nchar=1,...,n=default_n(nchar),.seed=NULL){
  check_n(n)
  check_must_be_integer(nchar)
  check_must_be_positive(nchar)

  if(length(nchar) != 1 && length(nchar) != n)
    error_glue("length of nchar is incompatible with context")

  if(length(nchar) != n){
    nchar <- rep(nchar,n)
  }

  with_seed(
    .seed,
    vapply(
      lapply(nchar,sample,x=base::LETTERS,replace=T),
      paste0,
      collapse="",
      character(1))
  )
}

#' @describeIn r_letters Uses lower- & upper-case letters
#'
#' @examples
#'
#' r_Letters(3)
#'
#' r_Letters(1:10)
#'
#' r_Letters(3,n=10)
#'
#' @export
r_Letters <- function(nchar=1,...,n=default_n(nchar),.seed=NULL){
  check_n(n)
  check_must_be_integer(nchar)
  check_must_be_positive(nchar)

  if(length(nchar) != 1 && length(nchar) != n)
    error_glue("length of nchar is incompatible with context")

  if(length(nchar) != n){
    nchar <- rep(nchar,n)
  }

  with_seed(
    .seed,
    vapply(
      lapply(nchar,sample,x=c(base::letters,base::LETTERS),replace=T),
      paste0,
      collapse="",
      character(1))
  )
}
