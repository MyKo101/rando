

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
#' r_bern(seq(0, 1, 0.1))
#'
#' r_bern(1 / 4, n = 10)
#' @export
r_bern <- function(prob = 0.5, ..., n = default_n(prob), .seed = NULL) {
  check_n(n)
  check_must_be_between(prob, 0, 1)

  with_seed(
    .seed,
    stats::rbinom(n = n, size = 1, prob = prob)
  )
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
#' r_lgl(seq(0, 1, 0.1))
#'
#' r_lgl(1 / 4, n = 10)
#' @export
r_lgl <- function(prob = 0.5, ..., n = default_n(prob), .seed = NULL) {
  check_n(n)
  check_must_be_between(prob, 0, 1)

  with_seed(
    .seed,
    stats::rbinom(n = n, size = 1, prob = prob) == 1
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
#' r_letters(3, n = 10)
#' @export
r_letters <- function(nchar = 1, ..., n = default_n(nchar), .seed = NULL) {
  check_n(n)
  check_must_be_integer(nchar)
  check_must_be_positive(nchar)

  with_seed(
    .seed,
    letter_sample(letters,
      nchar = nchar,
      n = n
    )
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
#' r_LETTERS(3, n = 10)
#' @export
r_LETTERS <- function(nchar = 1, ..., n = default_n(nchar), .seed = NULL) {
  check_n(n)
  check_must_be_integer(nchar)
  check_must_be_positive(nchar)

  with_seed(
    .seed,
    letter_sample(LETTERS,
      nchar = nchar,
      n = n
    )
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
#' r_Letters(3, n = 10)
#' @export
r_Letters <- function(nchar = 1, ..., n = default_n(nchar), .seed = NULL) {
  check_n(n)
  check_must_be_integer(nchar)
  check_must_be_positive(nchar)

  with_seed(
    .seed,
    letter_sample(c(letters, LETTERS),
      nchar = nchar,
      n = n
    )
  )
}


letter_sample <- function(x, nchar, n) {
  if (length(nchar) != n) {
    nchar <- rep(nchar, n)
  }
  vapply(
    lapply(nchar, sample, x = x, replace = T),
    paste0,
    collapse = "",
    character(1)
  )
}

#' @name r_matrix
#'
#' @title Generate a random Matrix
#'
#' @description
#' Generate a random matrix, given a random number generator and it's
#' dimensions. By default, this will generate a square matrix.
#'
#' @param engine
#' The rando function that will be used to generate the random numbers
#'
#' @param ncol,nrow
#' dimensions of the matrix. The [default_n()] function will provide
#' a default value within context.
#'
#' @param col_names,row_names
#' names to be assigned to the rows or columns. This is also used in
#' deciding the dimensions of the result.
#'
#'
#' @inheritParams r_norm
#'
#' @examples
#' set_n(5)
#'
#' r_matrix(r_norm)
#'
#' r_matrix(r_unif,min=1,max=2)
#'
#'
#' r_matrix(r_norm,mean=10,sd=2,ncol=2)
#'
#'
#' @export
#'

r_matrix <- function(engine,col_names=NULL,row_names=NULL,...,
                     nrow=default_n(row_names),ncol=default_n(col_names),.seed=NULL){
  check_n(ncol)
  check_n(nrow)

  res <- with_seed(
    .seed,
    matrix(engine(...,n=ncol*nrow),nrow=nrow,ncol=ncol)
  )

  if(!is.null(col_names)) colnames(res) <- col_names
  if(!is.null(row_names)) rownames(res) <- row_names

  res


}
