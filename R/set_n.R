#' @name set_n
#'
#' @title Set the default value for n
#'
#' @description
#' Set and get the global value for n
#'
#' @param n
#' value to set as the default n for random number generating
#'
#' @examples
#' set_n(100)
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
#' @export
get_n <- function() {
  getOption("rando.n")
}




#' @name default_n
#'
#' @title Find the Default value for n in Context
#'
#' @description
#' Checks for various information surrounding the call to this function
#' to figure out what value for n should be used
#'
#' @param ...
#' parameters to check the length of
#'
#'

NULL

#' @describeIn default_n Checks for the default value of n in a
#' context aware manner.
#'
#' @examples
#' default_n()
#' tibble::tibble(id = 1:3, n = default_n())
#' df <- tibble::tibble(id = 1:4)
#' dplyr::mutate(df, n = default_n())
#' default_n(1:5)
#' set_n(10)
#' default_n()
#' @export
default_n <- function(...) {
  con_n <- null_switch(
    tibble_n(),
    dplyr_n(),
    1
  )

  arg_n <- args_n(...)

  n_list <- c(con_n, arg_n)

  if (all(n_list == 1)) {
    null_switch(get_n(), 1)
  } else {
    n_list <- n_list[n_list != 1]
    n <- unique(n_list)
    if (length(n) > 1) {
      err_fun <- deparse1(sys.call(sys.parent())[[1]])
      error_glue("Inconsistent parameter lengths supplied to {err_fun}()")
    }
    n
  }
}

#' @describeIn default_n If we are inside of a call to  \code{tibble()},
#'  it returns the number of rows in the \code{tibble}, otherwise
#'  \code{NULL}
#' @examples
#' tibble::tibble(id = 1:3, n = tibble_n())
#' @export

tibble_n <- function() {
  .calls <- sys.calls()
  .calls_1 <- lapply(.calls, `[[`, 1)
  .calls_names <- vapply(.calls_1, deparse1, character(1))

  if ("tibble_quos" %in% .calls_names) {
    .tibble_env <- sys.frame(which("tibble_quos" == .calls_names))
    if ("current_size" %in% names(.tibble_env)) {
      cs <- .tibble_env[["current_size"]]
    } else {
      cs <- .tibble_env[["first_size"]]
    }
    if (!is.null(cs) && cs > 1) cs else NULL
  } else {
    NULL
  }
}


#' @describeIn default_n If we are in \code{dplyr} verb function, it
#' returns the number of rows in the current data, otherwise \code{NULL}
#' @examples
#' dplyr::mutate(df, n = dplyr_n())
#' @export

dplyr_n <- function() {
  tryCatch(dplyr::n(), error = function(e) NULL)
}


#' @describeIn default_n Checks the lengths of the non-\code{NULL}
#' arguments passed to it. Errors if they are inconsistent.
#'
#' @examples
#' args_n(1:3)
#' \dontrun{
#' args_n(1:3, 1:4)
#' }
#'
#' @export


args_n <- function(...) {
  args <- list(...)
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) > 0) {
    vapply(args, NROW, numeric(1))
  } else {
    1
  }
}
