#' @name blueprint
#'
#' @title Blueprint a Population
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
#' make_tbl(n=2)
#'
#' make_tbl(n=5)
#'
#' make_tbl2 <- blueprint(
#'   x = r_norm(mean=x_mu),
#'   y = r_unif(min=y_min,max=y_max)
#' )
#'
#' make_tbl2(x_mu = 10, y_min = -10, y_max=-5)
#'
#'
#'
#' @return
#' a function which can produce a tibble based on the provided
#' blueprint
#'
#' @export
#'


blueprint <- function(...){
  .call <- match.call()
  .call[[".rows"]] <- quote(n)
  .call[[1]] <- quote(tibble::tibble)

  f <- function(...,n=default_n(...),.seed=NULL) NULL

  body(f) <- call(
    "{",
    quote(list2env(list(...),environment())),
    call("with_seed",quote(.seed),.call)
  )
  environment(f) <- new.env(parent = parent.frame())
  f
}
