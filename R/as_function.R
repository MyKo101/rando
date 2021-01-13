#' @name as_function
#'
#' @title Convert to function
#'
#' @description
#' This function is a wrapper around [rlang::as_function()] which adds
#' a few extra features
#'
#' * formulas can use `.t` in place of `.x` to be easier to understand
#'  in time-based functions
#' * functions can take additional named arguments.
#'
#' @param x
#' A function or formula, see [rlang::as_function()] for more
#' information
#'
#' @param env
#' Environment in which to fetch the function in case `x` is a string
#'
#' @examples
#'
#' f1 <- as_function(~ .t + 1)
#' f1(10)
#'
#' f2 <- as_function(~ .t + x)
#'
#' f2(10, x = 2)
#' @export
#'
as_function <- function(x, env = parent.frame()) {
  f <- rlang::as_function(x, env = env)
  if (rlang::is_lambda(f)) {
    cl <- class(f)
    formals(f) <- c(formals(f), .t = quote(..1))
    body(f) <- call(
      "{",
      quote(extract_dots()),
      body(f)
    )
    structure(f, class = c("rando_lambda_function", cl))
  } else {
    f
  }
}
