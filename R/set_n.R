#' @name set_n
#'
#' @title Set and Get the Default Value for n
#'
#' @description
#' Set and get the global value for n for rando functions
#'
#' @param n
#' value to set as the default n
#'
#' @return
#' The current _global_ default value for n.\cr
#' `set_n()` returns this value invisibly
#'
#' @examples
#' set_n(100)
#'
#' @export
#'
set_n <- function(n) {
  if (is.null(n)) {
    options(rando.n = NULL)
  } else {
    check_n(n)
    options(rando.n = n)
  }
  invisible(n)
}

#' @rdname set_n
#'
#' @examples
#' get_n()
#'
#' @export
get_n <- function() {
  getOption("rando.n")
}


