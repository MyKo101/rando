#' @name extract_dots
#'
#' @title Extract the ellipsis inside a function
#'
#' @description
#' Allow the named entries in `...` to be used easily within a
#' function by attaching them to the function's environment
#'
#'
#' @examples
#'
#' \dontrun{
#' f <- function(...){
#'   a + b
#' }
#'
#' #Throws an error because a and b are trapped inside ...
#' f(a = 1, b = 2)
#'
#' }
#'
#' f <- function(...){
#'   extract_dots()
#'   a + b
#' }
#' f(a = 1, b = 2)
#'
#'
#' @export
extract_dots <- function(){
  .pcall <- match.call(sys.function(sys.parent()),
                       sys.call(sys.parent()),
                       expand.dots=F,
                       envir=parent.frame(2L))
  dots <- .pcall[["..."]]
  dots <- dots[names(dots) != ""]
  list2env(lapply(dots,eval),parent.frame())
}
