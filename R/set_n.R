#' @name set_n
#'
#' @title Set the default value for n
#'
#' @description
#' This function sets the default value for n, the number of random
#' numbers to generate. If n is not specified nor implied by context,
#' this is the default value that will be used.
#'
#'
#' @param n
#' value to set as the default n for random number generating
#'
#' @examples
#' set_n(100)
#'
#' length(r_norm())
#'
#' set_n(10)
#' length(r_norm())
#'
#' @export
#'
#'
#'
set_n <- function(n){
  if(is.null(n)){
    options(rando.n = NULL)
  } else {
    check_n(n)
    options(rando.n = n)
  }
  invisible(n)
}

#' @rdname set_n
#'
#' @param ...
#' parameters passed from random number generating functions
#'
#' @details
#' The \code{default_n()} function looks for a possible value for
#' \code{n} in multiple places, in order, these locations are:
#'
#' 1. Assesses whether it is being called in a \code{tibble()} call and
#'    calculate how many rows the are being generated.
#'
#' 2. If this fails, it checks for a value of \code{dplyr::n()},
#'    which returns the number of rows of the current data structure
#'    if we are inside a \code{dplyr} verb, such as [dplyr::mutate()]
#'
#' 3. If parameters supplied to \code{default_n()}, then the length of
#'    these arguments will be used to define \code{n}
#'
#' 4. The last check is to see if \code{set_n()} has defined a default
#'    \code{n} to be used globally.
#'
#' @examples
#' default_n()
#'
#'
#' @export
default_n <- function(...){

  .calls <- sys.calls()
  .calls_1 <- lapply(.calls,`[[`,1)
  .calls_names <- vapply(.calls_1,deparse1,character(1))

  args <- list(...)
  args <- args[!vapply(args,is.null,logical(1))]

  # Checks if we are evaluating inside a tibble declaration
  n <- if("tibble_quos" %in% .calls_names){
    .tibble_env <- sys.frame(which("tibble_quos" == .calls_names))
    if("current_size" %in% names(.tibble_env)){
      cs <- .tibble_env[["current_size"]]
    } else{
      cs <- .tibble_env[["first_size"]]
    }

    if(!is.null(cs) && cs > 1) cs else NULL
  } else NULL

  #If not, checks if we are inside a dplyr verb
  if(is.null(n)){
    n <- tryCatch(dplyr::n(),error=function(e) NULL)
  }

  #If not, get the length of the other arguments
  if(is.null(n) && length(args)>0){
    n_list <- vapply(args,length,numeric(1))
    if(all(n_list == 1)){
      n <- NULL
    } else{
      n_list <- n_list[n_list != 1]
      n <- unique(n_list)
      if(length(n) > 1){
        err_fun <- .calls_names[[length(.calls_names)-2]]
        error_glue("Inconsistent parameter lengths supplied to {err_fun}()")
      }

    }
  }

  #If not, get the default value as set by set_n()
  if(is.null(n)){
    n <- getOption("rando.n")
  }

  if(is.null(n)){
    n <- 1
  }

  n

}
