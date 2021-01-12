#' @name is_wholenumber
#'
#' @title Check if a Number is Whole
#'
#' @description
#' The built-in function `is.integer()` will check if a number is of
#' the `integer` class. However, we would usually want a function
#' that can check if a number is a _whole number_. It is also
#' vectorised over the input.
#'
#' @param x
#' Number to check
#'
#' @param tol
#' tolerance to check the values
#'
#' @examples
#' is.integer(2)
#' is_wholenumber(2)
#'
#' is.integer(seq(2,3,0.25))
#' is_wholenumber(seq(2,3,0.25))
#'
#' @export
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  if(is.numeric(x)) abs(x - round(x)) < tol else rep(FALSE,length(x))
}


#' @name match.call2
#'
#' @title Extension of `match.call()`
#'
#' @description
#' Extends the built-in function `match.call()` by allowing the user
#' to specify how far up the call stack they would like to extract.
#'
#' @param n
#' How far up the call-stack they would like to extract. The default,
#' `n=0` produces the same result as `match.call()` so this can be
#' inserted wherever `match.call()` is used.
#'
#' @examples
#' f <- function(n){
#'   g(n)
#' }
#'
#' g <- function(n){
#'   h(n)
#' }
#'
#' h <- function(n){
#'   match.call2(n)
#' }
#'
#' f(0)
#' f(1)
#' f(2)
#'
#'
#' @export
#'
match.call2 <- function(n=0,...){
  match.call(definition=sys.function(sys.parent(n+1)),
             call = sys.call(sys.parent(n+1)),
             envir=parent.frame(n+3),...)
}


