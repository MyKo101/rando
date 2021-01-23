#' @name as_function
#'
#' @title Convert to function
#'
#' @description
#' This function is a wrapper around [rlang::as_function()] which adds
#' a two extra features:
#'
#' * formulas can use `.t` in place of `.x` to be easier to understand
#'  in time-based functions
#' * functions can take additional named arguments.
#'
#' @param x
#' a function or formula, see [rlang::as_function()] for more
#' information
#'
#' @param env
#' Environment in which to fetch the function in case `x` is a string
#'
#' @return
#' Either:
#' * the function as it is passed to `as_function()`, whether as a
#' string or a name
#' * the function derived from a formula, where the first argument
#' is passed as `.`, `.x` or `.t`, the second argument is passed as
#' `.y` and any other named arguments are passed as they are named
#'
#' @examples
#'
#' f1 <- as_function(mean)
#' f1(1:10)
#'
#' f2 <- as_function("sum")
#' f2(1,2,3)
#'
#' f3 <- as_function(~.x + 1)
#' f3(9)
#'
#' f4 <- as_function(~ .t + 1)
#' f4(10)
#'
#' f5 <- as_function(~.x + .y)
#' f5(1,2)
#'
#' f6 <- as_function(~ .t + alpha)
#' f6(10, alpha = 2)
#'
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
