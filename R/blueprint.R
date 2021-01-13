#' @name blueprint
#'
#' @title Blueprint a Dataset
#'
#' @description
#' Allows for the generation of population based on a prescribed
#'
#' @param ...
#' arguments used to generate the blueprint
#'
#' @examples
#' make_tbl <- blueprint(
#'   x = r_norm(),
#'   y = r_norm()
#' )
#'
#' make_tbl(n = 2)
#'
#' make_tbl(n = 5)
#'
#' make_tbl2 <- blueprint(
#'   x = r_norm(mean = x_mu),
#'   y = r_unif(min = y_min, max = y_max)
#' )
#'
#' make_tbl2(x_mu = 10, y_min = -10, y_max = -5)
#' @return
#' a function which can produce a tibble based on the provided
#' blueprint
#'
#' @export
#'


blueprint <- function(...) {
  .call <- match.call()
  .call[[".rows"]] <- quote(n)
  .call[[1]] <- quote(tibble::tibble)

  f <- function(..., n = default_n(...), .seed = NULL) NULL

  body(f) <- call(
    "{",
    quote(list2env(list(...), environment())),
    call("with_seed", quote(.seed), .call)
  )
  environment(f) <- new.env(parent = parent.frame())
  structure(
    f,
    class = c("rando_blueprint", "function")
  )
}

#' @rdname blueprint
#' @param bp
#' Obj to check
#'
#' @examples
#' is_blueprint(make_tbl)
#' @export

is_blueprint <- function(bp) {
  inherits(bp, "rando_blueprint")
}

#' @name bp_where
#'
#' @title Blueprint based on a condition
#'
#' @description
#' Runs a blueprint function where a condition is true, otherwise
#' returns `NA` values
#'
#'
#' @param condition
#' Condition to check before evaluating. Results will be given where
#' this is \code{TRUE}, and \code{NA} when this is \code{FALSE}
#'
#' @param bp
#' Blueprint function to run based on the condition
#'
#' @param ...
#' arguments passed on to Blueprint, such as \code{.seed}
#'
#' @examples
#' make_tbl <- blueprint(
#'   x = r_norm(),
#'   y = r_unif()
#' )
#'
#' set_n(10)
#' i <- r_lgl()
#'
#' bp_where(i, make_tbl)
#'
#' df <- tibble::tibble(
#'   id = 1:10,
#'   cnd = r_lgl()
#' )
#' dplyr::mutate(df, bp_where(cnd, make_tbl))
#' @export
#'
bp_where <- function(condition, bp, ...) {
  if (!is.logical(condition)) {
    error_glue("condition argument passed to bp_where() should be logical")
  }
  if (!is_blueprint(bp)) {
    error_glue("bp argument passed to bp_where() should be a blueprint function")
  }

  full_set <- bp(n = sum(condition), ...)
  rows <- rep(NA, length(condition))
  rows[condition] <- 1:sum(condition)
  full_set[rows, ]
}
