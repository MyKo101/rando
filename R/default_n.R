#' @name default_n
#'
#' @title Find the Default Value for n in Context
#'
#' @description
#' Checks for various information surrounding the call to this function
#' to figure out what value for n should be used
#'
#' @return
#' The context aware value for n
#'
#' @details
#' The `default_n()` function will run through the other
#' functions found here until it finds a viable value for n. \cr\cr
#' It first checks for contxt to see if calls external to `default_n()`
#' indicate which value should be used:
#'
#' *  `blueprint_n()` - Checks if the function is being called
#' within a blueprinting function, and returns the value supplied to
#' that function, see [blueprint()].
#' *  `tibble_n()` - Checks if the function is being called
#' within the declaration of a tibble. It then checks the lengths of
#' the other arguments being passed to the call. If you want to
#' specify how many rows should be generate you can use the `.rows`
#' argument in your `tibble()` call, see [`tibble()`][tibble::tibble()]
#' * `dplyr_n()` - Checks if the function is being used within a
#' [`dplyr`][dplyr::dplyr] verb, if so, it returns the value of
#' [`n()`][dplyr::n()]
#'
#' It then checks the lengths of the arguments supplied via `...`,
#' if there is a discrepancy between these arguments and the context
#' aware value found above, it will throw an error.
#'
#' If all the above values return `1` or `NULL`, we then check for
#' a global n assigned by [set_n()], if none is set then `default_n()`
#' will return `1`.
#'
#'
#' @param ...
#' parameters to check the lengths of
#'
#' @examples
#' # Global Values:
#' set_n(NULL)
#' default_n()
#' set_n(10)
#' default_n()
#'
#' # In a blueprint:
#' bp <- blueprint(x=r_norm(),n=default_n())
#' bp(n=7)
#' bp <- blueprint(x=r_norm(),n=blueprint_n())
#' bp(n=8)
#'
#' # In a tibble:
#' tibble::tibble(id = 1:3, n = default_n())
#' tibble::tibble(id = 1:5, n = tibble_n())
#'
#' # In a dplyr verb:
#' df <- tibble::tibble(id = 1:4)
#' dplyr::mutate(df, n = default_n())
#' dplyr::mutate(df, n = dplyr_n())
#'
#' # From arguments:
#' default_n(1:5)
#' default_n(1:5,c("a","b","c","d","e"))
#' args_n(1:3,c("a","b","d"))
#' args_n(1:3, 1:4)
#'
#' \dontrun{
#' default_n(1:3, 1:4)
#' tibble::tibble(id=1:5,n=default_n(1:4))
#' }
#'

NULL

#' @rdname default_n
#'
#' @export
#'
default_n <- function(...) {
  con_n <- null_switch(
    blueprint_n(),
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

#' @rdname default_n
#'
#' @export
#'
blueprint_n <- function(){
  get0("..blueprint_n",ifnotfound=NULL)
}

#' @rdname default_n
#'
#' @export
#'
tibble_n <- function() {
  .calls <- sys.calls()
  .calls_1 <- lapply(.calls, `[[`, 1)
  .calls_names <- vapply(.calls_1, deparse1, character(1))

  if ("tibble_quos" %in% .calls_names) {
    .tibble_env <- sys.frame(which("tibble_quos" == .calls_names))
    cs <- .tibble_env[["first_size"]]
    if (!is.null(cs) && cs > 1) cs else NULL
  } else {
    NULL
  }
}


#' @rdname default_n
#'
#' @export
#'
dplyr_n <- function() {
  tryCatch(dplyr::n(), error = function(e) NULL)
}


#' @rdname default_n
#'
#' @export
#'
args_n <- function(...) {
  args <- list(...)
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) > 0) {
    vapply(args, NROW, numeric(1))
  } else {
    1
  }
}



